{-|
  Module:      Servant.Server.Experimental.Auth.Cookie
  Copyright:   (c) 2016 Al Zohali
  License:     BSD3
  Maintainer:  Al Zohali <zohl@fmap.me>
  Stability:   experimental

  = Description

  Authentication via encrypted client-side cookies, inspired by
  client-session library by Michael Snoyman and based on ideas of the
  paper \"A Secure Cookie Protocol\" by Alex Liu et al.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Servant.Server.Experimental.Auth.Cookie
  ( CipherAlgorithm
  , AuthCookieData
  , AuthCookieException (..)

  , AuthCookieExceptionHandler
  , AuthCookieHandler

  , PayloadWrapper(..)
  , ExtendedPayloadWrapper(..)
#if MIN_VERSION_servant(0,9,1)
  , Cookied
  , CookiedWrapper
  , CookiedWrapperClass(..)
  , cookied
#endif

  , RandomSource
  , mkRandomSource
  , getRandomBytes
  , generateRandomBytes

  , ServerKey
  , ServerKeySet (..)

  , PersistentServerKey
  , mkPersistentServerKey

  , RenewableKeySet
  , RenewableKeySetHooks (..)
  , mkRenewableKeySet

  , AuthCookieSettings (..)
  , SessionSettings (..)
  , ExpirationType (..)

  , EncryptedSession (..)
  , emptyEncryptedSession

  , encryptSession
  , decryptSession

  , addSession
  , removeSession
  , addSessionToErr
  , removeSessionFromErr
  , getSession
#if MIN_VERSION_servant(0,9,0)
  , getHeaderSession
#endif

  , defaultAuthHandler

  -- exposed for testing purpose
  , Cookie(..)
  , SerializedEncryptedCookie
  , EncryptedCookie

  , IVBytes
  , PayloadBytes
  , PaddingBytes
  , MACBytes

  , base64Encode
  , base64Decode
  , cerealEncode
  , cerealDecode

  , renderSession
  , parseSessionRequest
  , parseSessionResponse
  , unProxy

  , mkCookieKey
  , mkPadding
  , mkMAC
  , applyCipherAlgorithm
  ) where

import Blaze.ByteString.Builder (toByteString)
import Control.Arrow ((&&&), first)
import Control.Monad
import Control.Monad.Catch (MonadThrow (..), Exception)
import Control.Monad.Except
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Hash (HashAlgorithm(..))
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC)
import Crypto.Random (DRG(..), drgNew)
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.List (partition)
import Data.Maybe (listToMaybe)
import Data.Proxy
import Data.Serialize (Serialize(..))
import Data.Tagged (Tagged (..), retag)
import Data.Time
import Data.Time.Clock.Serialize ()
import Data.Typeable
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (hCookie, HeaderName, RequestHeaders, ResponseHeaders)
import Network.Wai (Request, requestHeaders)
import Servant (addHeader)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.ResponseHeaders (AddHeader)
import Servant.Server (err403, ServerError(..))
import Servant.Server.Experimental.Auth
import Web.Cookie
import qualified Crypto.MAC.HMAC        as H
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BSC8
import qualified Data.Serialize as Serialize (encode, decode)
import qualified Network.HTTP.Types as N(Header)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if MIN_VERSION_servant(0,9,0)
import Servant (ToHttpApiData (..))
import Data.Text (Text)
#else
import Data.ByteString.Conversion (ToByteString (..))
#endif

#if MIN_VERSION_servant(0,9,1)
import Servant (noHeader)
import Servant.API.ResponseHeaders (Headers)
import qualified Servant.API.Header as S(Header)
#endif

#if MIN_VERSION_http_types(0,10,0)
import Network.HTTP.Types.Header (hSetCookie)
#endif

#if MIN_VERSION_http_types(0,10,0)
#else
hSetCookie :: HeaderName
hSetCookie = "Set-Cookie"
#endif

----------------------------------------------------------------------------
-- General types

-- | A type for encryption and decryption functions operating on 'ByteString's.
type CipherAlgorithm c = c -> IV c -> ByteString -> ByteString

-- | A type family that maps user-defined data to 'AuthServerData'.
type family AuthCookieData

-- | How to represent expiration to the client's browser.
data ExpirationType
   = Expires -- ^ cookies will be provided with 'Expires' flag. Deprecated in favour of 'Max-Age'.
   | MaxAge  -- ^ cookies will be provided with 'Max-Age' flag. Doesn't work with older versions of IE.
   | Session -- ^ cookies will be removed when the user closes the browser.
   deriving (Eq, Show, Generic)

instance Default ExpirationType where
  def = Session

instance Serialize ExpirationType

-- | Format used in 'Expires' cookie field.
expirationFormat :: String
expirationFormat = "%a, %d %b %Y %H:%M:%S GMT"


-- | Wrapper for session value that goes into cookies' payload.
data PayloadWrapper a = PayloadWrapper {
    pwSession    :: a
  , pwSettings   :: SessionSettings
  , pwExpiration :: UTCTime
  } deriving (Generic)

instance (Serialize a) => Serialize (PayloadWrapper a)

-- | Wrapper for session value with metadata that doesn't go into payload.
data ExtendedPayloadWrapper a = ExtendedPayloadWrapper {
    epwSession    :: a
  , epwSettings   :: SessionSettings
  , epwExpiration :: UTCTime
  , epwRenew      :: Bool
  }


type instance AuthServerData (AuthProtect "cookie-auth") = ExtendedPayloadWrapper AuthCookieData


-- | Representation of a cookie.
data Cookie = Cookie {
    cookieIV      :: Tagged IVBytes      ByteString
  , cookiePayload :: Tagged PayloadBytes ByteString
  , cookiePadding :: Tagged PaddingBytes ByteString
  , cookieMAC     :: Tagged MACBytes     ByteString
  }

instance Serialize Cookie where
  put Cookie {..} = do
    put $ unTagged cookieIV
    put $ unTagged cookiePayload
    put $ unTagged cookiePadding
    put $ unTagged cookieMAC

  get = do
    cookieIV       <- Tagged <$> get
    cookiePayload  <- Tagged <$> get
    cookiePadding  <- Tagged <$> get
    cookieMAC      <- Tagged <$> get
    return Cookie {..}


-- | A newtype wrapper over 'ByteString'
newtype EncryptedSession = EncryptedSession ByteString
  deriving (Eq, Show, Typeable)

-- | An empty 'EncryptedSession'
emptyEncryptedSession :: EncryptedSession
emptyEncryptedSession = EncryptedSession ""

#if MIN_VERSION_servant(0,9,0)
instance ToHttpApiData EncryptedSession where
  toHeader (EncryptedSession s) = s
  toUrlPiece = error "toUrlPiece @EncryptedSession: not implemented"
#else
instance ToByteString EncryptedSession where
  builder (EncryptedSession s) = builder s
#endif

#if MIN_VERSION_servant(0,9,1)
-- | Helper type to wrap endpoints.
type Cookied a = Headers '[S.Header "Set-Cookie" EncryptedSession] a
#endif

-- | The exception is thrown when something goes wrong with this package.
data AuthCookieException
  = CannotMakeIV ByteString
    -- ^ Could not make 'IV' for block cipher.
  | BadProperKey CryptoError
    -- ^ Could not initialize a cipher context.
  | TooShortProperKey Int Int
    -- ^ The key is too short for current cipher algorithm. Arguments of
    -- this constructor: minimal key length, actual key length.
  | IncorrectMAC ByteString
    -- ^ Thrown when Message Authentication Code (MAC) is not correct.
  | CookieExpired UTCTime UTCTime
    -- ^ Thrown when 'Cookie' has expired. Arguments of the constructor:
    -- expiration time, actual time.
  | SessionDeserializationFailed String
    -- ^ This is thrown when 'runGet' or 'Base64.decode' blows up.
  deriving (Eq, Show, Typeable)

instance Exception AuthCookieException

----------------------------------------------------------------------------
-- Tags for various bytestrings

-- | Tag for encrypted cookie.
data EncryptedCookie

-- | Tag for base64 serialized and encrypted cookie.
data SerializedEncryptedCookie

-- | Tag for raw server key bytes.
data ServerKeyBytes

-- | Tag raw cookie key bytes.
data CookieKeyBytes

-- | Tag for IV bytes.
data IVBytes

-- | Tag for encrypted or raw payload bytes.
data PayloadBytes

-- | Tag for pading bytes.
data PaddingBytes

-- | Tag for MAC of a cookie.
data MACBytes


-- | Wrapper for 'Base64.encode'.
base64Encode :: Tagged EncryptedCookie ByteString -> Tagged SerializedEncryptedCookie ByteString
base64Encode = retag . fmap Base64.encode

-- | Wrapper for 'Base64.decode'.
base64Decode :: (MonadThrow m)
  => Tagged SerializedEncryptedCookie ByteString
  -> m (Tagged EncryptedCookie ByteString)
base64Decode = either (throwM . SessionDeserializationFailed) return
             . fmap Tagged . Base64.decode . unTagged

-- | Wrapper for 'Serialize.encode'.
cerealEncode :: (Serialize a) => a -> Tagged b ByteString
cerealEncode = Tagged . Serialize.encode

-- | Wrapper for 'Serialize.decode'.
cerealDecode :: (Serialize a, MonadThrow m) => Tagged b ByteString -> m a
cerealDecode = either (throwM . SessionDeserializationFailed) return
             . Serialize.decode . unTagged

----------------------------------------------------------------------------
-- Random source

-- | A wrapper of self-resetting 'DRG' suitable for concurrent usage.
data RandomSource where
  RandomSource :: DRG d => IO d -> Int -> IORef (d, Int) -> RandomSource

-- | Constructor for 'RandomSource' value.
mkRandomSource :: (MonadIO m, DRG d)
  => IO d           -- ^ How to get deterministic random generator
  -> Int            -- ^ Threshold (number of bytes to be generated before resetting)
  -> m RandomSource -- ^ New 'RandomSource' value
mkRandomSource mkDRG threshold =
  RandomSource mkDRG threshold `liftM` liftIO ((,0) <$> mkDRG >>= newIORef)

-- | Extract pseudo-random bytes from 'RandomSource'.
getRandomBytes :: MonadIO m
  => RandomSource      -- ^ The source of random numbers
  -> Int               -- ^ How many random bytes to generate
  -> m ByteString      -- ^ The generated bytes in form of a 'ByteString'
getRandomBytes (RandomSource mkDRG threshold ref) n = do
  freshDRG <- liftIO mkDRG
  liftIO . atomicModifyIORef' ref $ \(drg, bytes) ->
    let (result, drg') = randomBytesGenerate n drg
        bytes'         = bytes + n
    in if bytes' >= threshold
         then ((freshDRG, 0), result)
         else ((drg', bytes'), result)

----------------------------------------------------------------------------
-- Server key

-- | Internal representation of a server key.
type ServerKey = ByteString

-- | Interface for a set of server keys.
class ServerKeySet k where
  getKeys   :: (MonadThrow m, MonadIO m) => k -> m (ServerKey, [ServerKey])
  -- ^ Retrieve current and rotated keys respectively.

  removeKey :: (MonadThrow m, MonadIO m) => k -> ServerKey -> m ()
  -- ^ Non-graciously remove the key from a keyset.


-- | A keyset containing only one key, that doesn't change.
data PersistentServerKey = PersistentServerKey
  { pskBytes :: ServerKey }

instance ServerKeySet PersistentServerKey where
  getKeys     = return . (,[]) . pskBytes
  removeKey _ = error "removeKey @PersistentServerKey: not implemented"

-- | Create instance of 'PersistentServerKey'.
mkPersistentServerKey :: ByteString -> PersistentServerKey
mkPersistentServerKey bytes = PersistentServerKey { pskBytes = bytes }


-- | Customizable actions for 'RenewableKeySet'.
data RenewableKeySetHooks s p = RenewableKeySetHooks
  { rkshNewState :: forall m. (MonadIO m, MonadThrow m)
    => p                  -- KeySet parameters
    -> ([ServerKey], s)   -- Current state
    -> m ([ServerKey], s) -- New state
    -- ^ Called when a keyset needs to refresh it's state. It's result might be
    -- discarded occasionally in favour of result yielded in another thread.

  , rkshNeedUpdate :: forall m. (MonadIO m, MonadThrow m)
    => p                 -- KeySet parameters
    -> ([ServerKey], s)  -- Current state
    -> m Bool            -- Whether to update the state
    -- ^ Called before retrieving the keys and refreshing the state.

  , rkshRemoveKey :: forall m. (MonadIO m, MonadThrow m)
    => p          -- KeySet parameters
    -> ServerKey  -- Key to remove
    -> m ()       -- Action to perform
    -- ^ Called after removing the key. This hook is called only if the key
    -- belongs to a keyset and called once per key. The only purpose of it is
    -- to clear the garbage after removing the key. The state might differs
    -- after removing the key and before calling the hook, therefore the hook
    -- doesn't rely on the state.
  }


-- | Customizable key set, that provides partial implementation of
-- 'ServerKeySet'.
data RenewableKeySet s p = RenewableKeySet
  { rksState      :: IORef ([ServerKey], s)
    -- ^ Key set state (keys and user-defined state).

  , rksParameters :: p
    -- ^ User-defined parameters of the key set.

  , rksHooks      :: RenewableKeySetHooks s p
    -- ^ USer-defined hooks of the key set.
  }

instance (Eq s) => ServerKeySet (RenewableKeySet s p) where
  getKeys RenewableKeySet {..} = getKeys' rksHooks where
    getKeys' RenewableKeySetHooks {..} = do
      state <- liftIO $ readIORef rksState
      rkshNeedUpdate rksParameters state
        >>= \needUpdate -> if not needUpdate
          then return $ toResult state
          else do
            state' <- rkshNewState rksParameters state
            liftIO $ atomicModifyIORef' rksState $ \state'' -> id &&& toResult $
              if (userState state /= userState state'')
              then state''
              else state'
    toResult = (head &&& tail) . fst
    userState = snd

  removeKey RenewableKeySet {..} key = do
    found <- liftIO $ atomicModifyIORef' rksState $ \(keys, s) -> let
      (found, keys') = first (not . null) . partition (== key) $ keys
      in ((keys', s), found)
    when found $ (rkshRemoveKey rksHooks) rksParameters key

-- | Create instance of 'RenewableKeySet'.
mkRenewableKeySet :: (MonadIO m)
  => RenewableKeySetHooks s p -- ^ Hooks
  -> p                        -- ^ Parameters
  -> s                        -- ^ Initial state
  -> m (RenewableKeySet s p)
mkRenewableKeySet rksHooks rksParameters userState = liftIO $ do
  rksState <- newIORef ([], userState)
  return RenewableKeySet {..}


----------------------------------------------------------------------------
-- Settings

-- | Options that determine authentication mechanisms. Use 'def' to get
-- default value of this type.
data AuthCookieSettings where
  AuthCookieSettings :: (HashAlgorithm h, BlockCipher c) =>
    { acsSessionField :: ByteString
      -- ^ Name of a cookie which stores session object
    , acsCookieFlags :: [ByteString]
      -- ^ Session cookie's flags
    , acsMaxAge :: NominalDiffTime
      -- ^ For how long the cookie will be valid (corresponds to “Max-Age”
      -- or "Expires" attribute).
    , acsPath :: ByteString
      -- ^ Scope of the cookie (corresponds to “Path” attribute).
    , acsHashAlgorithm :: Proxy h
      -- ^ Hash algorithm that will be used in 'hmac'.
    , acsCipher :: Proxy c
      -- ^ Symmetric cipher that will be used in encryption.
    , acsEncryptAlgorithm :: CipherAlgorithm c
      -- ^ Algorithm to encrypt cookies.
    , acsDecryptAlgorithm :: CipherAlgorithm c
      -- ^ Algorithm to decrypt cookies.
    } -> AuthCookieSettings

instance Default AuthCookieSettings where
  def = AuthCookieSettings
    { acsSessionField = "Session"
    , acsCookieFlags  = ["HttpOnly", "Secure"]
    , acsMaxAge       = fromIntegral (12 * 3600 :: Integer) -- 12 hours
    , acsPath         = "/"
    , acsHashAlgorithm = Proxy :: Proxy SHA256
    , acsCipher       = Proxy :: Proxy AES256
    , acsEncryptAlgorithm = ctrCombine
    , acsDecryptAlgorithm = ctrCombine }


-- | Options that determine session mechanisms. Use 'def' to get
-- default value of this type.
data SessionSettings = SessionSettings {
    ssExpirationType :: ExpirationType -- ^ How to represent expiration to the client's browser.
  , ssAutoRenew      :: Bool           -- ^ Whether to renew cookies automatically.
  } deriving (Show, Eq, Generic)

instance Serialize SessionSettings

instance Default SessionSettings where
  def = SessionSettings {
      ssExpirationType = def
    , ssAutoRenew      = False
    }

----------------------------------------------------------------------------
-- Encrypt/decrypt session

-- | Pack session object into a cookie.
--
-- The function can throw the following exceptions (of type
-- 'AuthCookieException'):
--
--     * 'TooShortProperKey'
--     * 'CannotMakeIV'
--     * 'BadProperKey'
encryptSession :: (MonadIO m, MonadThrow m, Serialize a, ServerKeySet k)
  => AuthCookieSettings -- ^ Options, see 'AuthCookieSettings'
  -> RandomSource       -- ^ Random source to use
  -> k                  -- ^ Instance of 'ServerKeySet' to use
  -> SessionSettings    -- ^ Session settings
  -> a                  -- ^ Session value
  -> m (Tagged SerializedEncryptedCookie ByteString)  -- ^ Serialized and encrypted session
encryptSession AuthCookieSettings {..} rs sks pwSettings pwSession = do
  pwExpiration  <- liftM (addUTCTime acsMaxAge) (liftIO getCurrentTime)
  cookieIV      <- mkIV rs acsCipher
  sk            <- liftM (Tagged . fst) (getKeys sks)
  key           <- mkCookieKey acsCipher acsHashAlgorithm sk cookieIV
  cookiePayload <- applyCipherAlgorithm acsEncryptAlgorithm cookieIV key (cerealEncode PayloadWrapper {..})
  cookiePadding <- mkPadding rs acsCipher cookiePayload
  let cookieMAC =  mkMAC acsHashAlgorithm sk Cookie {cookieMAC = "", ..}
  return . base64Encode . cerealEncode $ Cookie {..}


-- | Unpack session value from a cookie. The function can throw the same
-- exceptions as 'decryptCookie'.
--
-- The function can throw the following exceptions (of type
-- 'AuthCookieException'):
--
--     * 'TooShortProperKey'
--     * 'CannotMakeIV'
--     * 'BadProperKey'
--     * 'IncorrectMAC'
--     * 'CookieExpired'
--     * 'SessionDeserializationFailed'
decryptSession :: (MonadIO m, MonadThrow m, ServerKeySet k, Serialize a)
  => AuthCookieSettings                          -- ^ Options, see 'AuthCookieSettings'
  -> k                                           -- ^ Instance of 'ServerKeySet' to use
  -> Tagged SerializedEncryptedCookie ByteString -- ^ The 'ByteString' to decrypt
  -> m (ExtendedPayloadWrapper a)                -- ^ The decrypted 'Cookie'
decryptSession AuthCookieSettings {..} sks s = do
  Cookie {..} <- base64Decode s >>= cerealDecode
  let checkMAC sk = cookieMAC == mkMAC acsHashAlgorithm sk Cookie {..}
  (sk, renew) <- getKeys sks >>= \(currentKey, rotatedKeys) -> maybe
      (throwM $ IncorrectMAC (unTagged cookieMAC))
      (return)
      . listToMaybe
      . filter (checkMAC . fst)
      . map (first Tagged)
      $ ((currentKey, False):(map (,True) rotatedKeys))
  key <- mkCookieKey acsCipher acsHashAlgorithm sk cookieIV
  PayloadWrapper {..} <- applyCipherAlgorithm acsDecryptAlgorithm cookieIV key cookiePayload
                     >>= cerealDecode

  (liftIO getCurrentTime) >>= \t -> when (t >= pwExpiration) $ throwM (CookieExpired pwExpiration t)

  let SessionSettings {..} = pwSettings
  return ExtendedPayloadWrapper {
      epwSession    = pwSession
    , epwSettings   = pwSettings
    , epwExpiration = pwExpiration
    , epwRenew      = renew || ((ssExpirationType /= Session) && ssAutoRenew)
    }

----------------------------------------------------------------------------
-- Add/remove session

type AddSession r s = forall m a k. (MonadIO m, MonadThrow m, Serialize a, ServerKeySet k)
  => AuthCookieSettings -- ^ Options, see 'AuthCookieSettings'
  -> RandomSource       -- ^ Random source to use
  -> k                  -- ^ Instance of 'ServerKeySet' to use
  -> SessionSettings    -- ^ Session settings
  -> a                  -- ^ The session value
  -> r                  -- ^ The original response
  -> m s                -- ^ Modified response

type RemoveSession r s = forall m. (Monad m)
  => AuthCookieSettings -- ^ Options, see 'AuthCookieSettings'
  -> r                  -- ^ The original response
  -> m s                -- ^ Modified response


-- | Add cookie header to response. The function can throw the same
-- exceptions as 'encryptSession'.
addSession :: (AddHeader (e :: Symbol) EncryptedSession r s) => AddSession r s
addSession acs rs sk pwSettings pwSession response = do
  header <- renderSession acs rs sk pwSettings pwSession ()
  return (addHeader (EncryptedSession header) response)

-- | "Remove" a session by invalidating the cookie.
removeSession :: (AddHeader (e :: Symbol) EncryptedSession r s) => RemoveSession r s
removeSession acs response =
  return (addHeader (EncryptedSession $ renderExpiredSession acs) response)

-- | Add cookie session to error allowing to set cookie even if response is
-- not 200.
addSessionToErr :: AddSession ServerError ServerError
addSessionToErr acs rs sk pwSettings pwSession err = do
  header <- renderSession acs rs sk pwSettings pwSession ()
  return err { errHeaders = (hSetCookie, header) : errHeaders err }

-- | "Remove" a session by invalidating the cookie.
-- Cookie expiry date is set at 0  and content is wiped
removeSessionFromErr :: RemoveSession ServerError ServerError
removeSessionFromErr acs err =
  return $ err { errHeaders = (hSetCookie, renderExpiredSession acs) : errHeaders err }


-- | Request handler that checks cookies. If 'Cookie' is just missing, you
-- get 'Nothing', but if something is wrong with its format, 'getSession'
-- can throw the same exceptions as 'decryptSession'.
getSession :: (MonadIO m, MonadThrow m, Serialize a, ServerKeySet k)
  => AuthCookieSettings                   -- ^ Options, see 'AuthCookieSettings'
  -> k                                    -- ^ 'ServerKeySet' to use
  -> Request                              -- ^ The request
  -> m (Maybe (ExtendedPayloadWrapper a)) -- ^ The result
getSession acs sk request = getSession' (requestHeaders request) acs sk

#if MIN_VERSION_servant(0,9,0)
-- | Get session from `Header "cookie" ByteString` in a route. Useful
-- for checking authentication without denying access to route.
--
-- If 'Cookie' is missing, you get 'Nothing', but but if something is
-- wrong with its format, 'getSession' can throw the same exceptions
-- as 'decryptSession'.
getHeaderSession :: (MonadIO m, MonadThrow m, Serialize a, ServerKeySet k)
  => AuthCookieSettings
  -> k
  -> Text
  -> m (Maybe (ExtendedPayloadWrapper a))
getHeaderSession acs sk h = getSession' [(hCookie, toHeader h)] acs sk
#endif

getSession' :: (MonadIO m, MonadThrow m, Serialize a, ServerKeySet k)
  => RequestHeaders
  -> AuthCookieSettings
  -> k
  -> m (Maybe (ExtendedPayloadWrapper a))
getSession' headers acs@AuthCookieSettings {} sk = maybe
  (return Nothing)
  (liftM Just . decryptSession acs sk)
  (parseSessionRequest acs headers)


parseSession
  :: AuthCookieSettings
  -> HeaderName
  -> [N.Header]
  -> Maybe (Tagged SerializedEncryptedCookie ByteString)
parseSession AuthCookieSettings {..} hdr hdrs = sessionBinary where
  cookies = parseCookies <$> lookup hdr hdrs
  sessionBinary = Tagged <$> (cookies >>= lookup acsSessionField)

-- | Parse session cookie from 'RequestHeaders'.
parseSessionRequest
  :: AuthCookieSettings
  -> RequestHeaders
  -> Maybe (Tagged SerializedEncryptedCookie ByteString)
parseSessionRequest acs hdrs = parseSession acs hCookie hdrs

-- | Parse session cookie from 'ResponseHeaders'.
parseSessionResponse
  :: AuthCookieSettings
  -> ResponseHeaders
  -> Maybe (Tagged SerializedEncryptedCookie ByteString)
parseSessionResponse acs hdrs = parseSession acs hSetCookie hdrs


renderSession'
  :: AuthCookieSettings
  -> (Tagged SerializedEncryptedCookie ByteString)
  -> Maybe (ByteString, ByteString)
  -> ByteString
renderSession' AuthCookieSettings{..} (Tagged sessionBinary) expiration
  = toByteString . renderCookies
  $ (acsSessionField, sessionBinary)
  : ("Path", acsPath)
  : ((maybe id (:) expiration)
    $ ((,"") <$> acsCookieFlags))

-- | Render session cookie to 'ByteString'.
renderSession :: AddSession () ByteString
renderSession acs rs sk pwSettings pwSession _ = liftM2 (renderSession' acs)
  (encryptSession acs rs sk pwSettings pwSession)
  (renderExpiration (acsMaxAge acs) (ssExpirationType pwSettings))

-- | Render expired session to 'ByteString' (the date is set at 0 and the content is wiped).
renderExpiredSession :: AuthCookieSettings -> ByteString
renderExpiredSession acs = renderSession' acs (Tagged "") (Just ("Expires", longTimeAgo)) where
  longTimeAgo = BSC8.pack $ formatTime
    defaultTimeLocale
    expirationFormat
    timeOrigin
  timeOrigin = UTCTime (toEnum 0) 0

-- | Render expiration value depending on it's type.
renderExpiration :: (MonadIO m) => NominalDiffTime -> ExpirationType -> m (Maybe (ByteString, ByteString))

renderExpiration maxAge Expires
  = liftM (addUTCTime maxAge) (liftIO getCurrentTime)
  >>= \t -> return . Just $ ("Expires", BSC8.pack $ formatTime defaultTimeLocale expirationFormat t)

renderExpiration maxAge MaxAge = return . Just $ ("Max-Age", (BSC8.pack . show . n) maxAge)
  where n = floor :: NominalDiffTime -> Int

renderExpiration _ Session = return Nothing


#if MIN_VERSION_servant(0,9,1)
-- | Type for curried 'cookied' function (with fixed settings, random
-- source, keyset and session type).
type CookiedWrapper c = forall f r. (CookiedWrapperClass f r c) => f -> r

-- | Wrapper for endpoints that use cookies. It transforms function of type:
-- >>> q1 -> q2 -> ... -> Session -> ... -> qN -> Handler r
-- into
-- >>> q1 -> q2 -> ... -> ExtendedPayloadWrapper Session -> ... qN -> Handler (Cookied r)
--
-- Non-session variables number can be arbitrary. It supposed to be
-- used in tandem with 'Cookied' type.
--
-- Using this wrapper requires FlexibleContexts extention to be turned
-- on. In case of curring 'cookied' function, it's highly recommended
-- to provide signature for this (see 'CookiedWrapper').
cookied
  :: (ServerKeySet k)
  => AuthCookieSettings -- ^ Options, see 'AuthCookieSettings'
  -> RandomSource       -- ^ Random source to use
  -> k                  -- ^ Instance of 'ServerKeySet' to use
  -> Proxy c            -- ^ Type of session
  -> CookiedWrapper c   -- ^ Wrapper that transforms given functions.
cookied acs rs k p = wrapCookied (acs, rs, k, p) Nothing

-- | Class of functions that can be wrapped with 'cookied'.
class CookiedWrapperClass f r c where
  wrapCookied
    :: (ServerKeySet k)
    => (AuthCookieSettings, RandomSource, k, Proxy c) -- ^ Environment
    -> Maybe (PayloadWrapper c)                       -- ^ Session value (if found)
    -> f                                              -- ^ Tail of function to process
    -> r                                              -- ^ Wrapped function

-- When no arguments left: add session header to result.
instance (Serialize c, MonadIO m, MonadThrow m)
         => CookiedWrapperClass (m b) (m (Cookied b)) c where
  wrapCookied _               Nothing                    = fmap noHeader
  wrapCookied (acs, rs, k, _) (Just PayloadWrapper {..}) = (>>= addSession acs rs k pwSettings pwSession)

-- When the next argument is the one that should be wrapped: wrap it and carry it's value to the result.
instance (CookiedWrapperClass b b' c)
         => CookiedWrapperClass (c -> b) ((ExtendedPayloadWrapper c) -> b') c where
  wrapCookied env _ f = \ExtendedPayloadWrapper {..} -> let
    mc = if epwRenew
         then (Just PayloadWrapper {
                    pwSession    = epwSession
                  , pwSettings   = epwSettings
                  , pwExpiration = epwExpiration})
         else Nothing
    in wrapCookied env mc (f epwSession)

-- Otherwise: accept argument as is.
instance (CookiedWrapperClass b b' c)
         => CookiedWrapperClass (a -> b) (a -> b') c where
  wrapCookied env ms f = wrapCookied env ms . f
#endif

----------------------------------------------------------------------------
-- Default auth handler

-- | Type for exception handler.
type AuthCookieExceptionHandler m = forall a. AuthCookieException -> m (Maybe (ExtendedPayloadWrapper a))

-- | Type for cookied handler.
type AuthCookieHandler a
  = forall k. (ServerKeySet k)
  => AuthCookieSettings                              -- ^ Options, see 'AuthCookieSettings'
  -> k                                               -- ^ Instance of 'ServerKeySet' to use
  -> AuthHandler Request (ExtendedPayloadWrapper a)  -- ^ The result

-- | Cookie authentication handler.
defaultAuthHandler :: (Serialize a) => AuthCookieHandler a
defaultAuthHandler acs sk = mkAuthHandler $ \request -> do
  msession <- liftIO (getSession acs sk request)
  maybe (throwError err403) return msession

----------------------------------------------------------------------------
-- Helpers

-- | Applies 'H.hmac' algorithm to given data.
sign :: forall h. HashAlgorithm h
  => Proxy h           -- ^ The hash algorithm to use
  -> ByteString        -- ^ The key
  -> ByteString        -- ^ The message
  -> ByteString        -- ^ The result
sign Proxy key msg = BA.convert (H.hmac key msg :: HMAC h)
{-# INLINE sign #-}

-- | Truncates given 'ByteString' according to 'KeySizeSpecifier' or raises
-- | error if the key is not long enough.
mkProperKey :: MonadThrow m
  => KeySizeSpecifier  -- ^ Key size specifier
  -> ByteString        -- ^ The 'ByteString' to truncate
  -> m ByteString      -- ^ The resulting 'ByteString'
mkProperKey kss s = do
  let klen = BS.length s
      giveUp l = throwM (TooShortProperKey l klen)
  plen <- case kss of
    KeySizeRange l r ->
      if klen < l
        then giveUp l
        else return (min klen r)
    KeySizeEnum ls ->
      case filter (<= klen) ls of
        [] -> giveUp (minimum ls)
        xs -> return (maximum xs)
    KeySizeFixed l ->
      if klen < l
        then giveUp l
        else return l
  return (BS.take plen s)

-- | Derives key for a cookie based on server key and IV.
mkCookieKey
  :: (MonadThrow m, HashAlgorithm h, BlockCipher c)
  => Proxy c
  -> Proxy h
  -> Tagged ServerKeyBytes ByteString
  -> Tagged IVBytes ByteString
  -> m (Tagged CookieKeyBytes ByteString)
mkCookieKey c h (Tagged sk) (Tagged iv) = liftM Tagged $ mkProperKey (cipherKeySize (unProxy c)) (sign h sk iv)

-- | Generates random initial vector.
mkIV :: (MonadIO m, BlockCipher c)
  => RandomSource
  -> Proxy c
  -> m (Tagged IVBytes ByteString)
mkIV rs c = liftM Tagged $ getRandomBytes rs (blockSize (unProxy c))

-- | Generates padding of random bytes to align payload's length.
mkPadding :: (MonadIO m, BlockCipher c)
  => RandomSource
  -> Proxy c
  -> Tagged PayloadBytes ByteString
  -> m (Tagged PaddingBytes ByteString)
mkPadding rs c (Tagged payload) = liftM Tagged $ getRandomBytes rs l where
  bs = blockSize (unProxy c)
  n  = BS.length payload
  l  = (bs - (n `rem` bs)) `rem` bs

-- | Generates cookie's signature.
mkMAC :: (HashAlgorithm h)
  => Proxy h
  -> Tagged ServerKeyBytes ByteString
  -> Cookie
  -> Tagged MACBytes ByteString
mkMAC h (Tagged sk) Cookie {..} = Tagged . sign h sk $
       unTagged cookieIV
    <> unTagged cookiePayload
    <> unTagged cookiePadding

-- | Applies given encryption or decryption algorithm to given data.
applyCipherAlgorithm :: forall c m. (BlockCipher c, MonadThrow m)
  => CipherAlgorithm c
  -> Tagged IVBytes ByteString
  -> Tagged CookieKeyBytes ByteString
  -> Tagged PayloadBytes ByteString
  -> m (Tagged PayloadBytes ByteString)
applyCipherAlgorithm f (Tagged ivRaw) (Tagged keyRaw) (Tagged msg) = do
  iv <- case makeIV ivRaw :: Maybe (IV c) of
    Nothing -> throwM (CannotMakeIV ivRaw)
    Just  x -> return x
  key <- case cipherInit keyRaw :: CryptoFailable c of
    CryptoFailed err -> throwM (BadProperKey err)
    CryptoPassed   x -> return x
  (return . Tagged . BA.convert) (f key iv msg)

-- | Return bottom of type provided as 'Proxy' tag.

unProxy :: Proxy a -> a
unProxy Proxy = undefined

-- | Generates random sequence of bytes from new DRG
generateRandomBytes :: Int -> IO ByteString
generateRandomBytes size = (fst . randomBytesGenerate size <$> drgNew)
