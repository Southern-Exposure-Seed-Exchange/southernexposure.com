{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Auth
    ( -- * Tokens
      AuthToken(..)
    , makeToken
    , validateToken
      -- * Session Handling
    , WrappedAuthToken
    , Cookied
    , addSessionCookie
    , withCookie
    , temporarySession
    , permanentSession
      -- * Server Setup
    , sessionEntropy
    , mkPersistentServerKey
    , authServerContext
    ) where

import Control.Monad.Reader (MonadIO, MonadReader, ask, asks)
import Crypto.Random (drgNew)
import Data.Default (def)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Text.Encoding (decodeUtf8)
import Database.Persist (Entity(..), getBy)
import GHC.Generics (Generic)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
    ( AuthCookieData, ExtendedPayloadWrapper, AuthCookieSettings(..)
    , PersistentServerKey, RandomSource, SessionSettings(..), EncryptedSession
    , CookiedWrapperClass, ExpirationType(..), Cookied, mkPersistentServerKey
    , defaultAuthHandler, addSession, cookied, mkRandomSource
    )

import Config (Config(getCookieSecret, getCookieEntropySource))
import Models.DB (Unique(UniqueToken), Customer(customerAuthToken))
import Server (App, runDB, serverError)

import qualified Data.Text as T


-- | A serializable version of a Customer's authentication token. This is
-- encrypted & stored in a Cookie when the user logs in & can be used to
-- retrieve the Customer in routes that require authentication.
newtype AuthToken
    = AuthToken
        { fromAuthToken :: T.Text
        } deriving (Show, Eq, Generic)

instance Serialize AuthToken

type instance AuthCookieData = AuthToken

-- | Validate an AuthToken passed by a cookie by attempting to find the
-- matching Customer or throwing a 403 error if one does not exist.
validateToken :: AuthToken -> App (Entity Customer)
validateToken (AuthToken token) =
    runDB (getBy $ UniqueToken token)
    >>= maybe (serverError $ err403 { errBody = "Invalid Auth Token" })
            return

-- | Build the AuthToken for a Customer.
makeToken :: Customer -> AuthToken
makeToken = AuthToken . customerAuthToken

type WrappedAuthToken = ExtendedPayloadWrapper AuthToken



cookieSettings :: AuthCookieSettings
cookieSettings =
    def { acsMaxAge = fromInteger $ 10 * 365 * 24 * 60 * 60 -- 10 years
        }

addSessionCookie :: AddHeader e EncryptedSession a b => SessionSettings -> AuthToken -> a -> App b
addSessionCookie ss token val = do
    entropy <- asks getCookieEntropySource
    secret <- asks getCookieSecret
    addSession cookieSettings entropy secret ss token val

-- | Extract the AuthToken from the wrapper and pass it to a handler
-- function.
withCookie
    :: ( CookiedWrapperClass f (ExtendedPayloadWrapper AuthToken -> m b) AuthToken
       , MonadReader Config m
       )
    => ExtendedPayloadWrapper AuthToken
    -> f
    -> m b
withCookie token handler = ask >>= cookied_
  where
    cookied_ cfg =
        cookied cookieSettings (getCookieEntropySource cfg) (getCookieSecret cfg)
            (Proxy @AuthToken) handler token


-- | Cookie session settings for logins that expire when the browser
-- closes.
temporarySession :: SessionSettings
temporarySession =
    SessionSettings
        { ssExpirationType = Session
        , ssAutoRenew = False
        }

-- | Cookie session settings for permanent logins.
permanentSession :: SessionSettings
permanentSession =
    SessionSettings
        { ssExpirationType = MaxAge
        , ssAutoRenew = True
        }


-- TODO: Remove once all routes switched over to cookie auth.
type instance AuthServerData (AuthProtect "auth-token") = AuthToken
-- TODO: Remove once all routes switched over to cookie auth.
authHandler :: AuthHandler Request AuthToken
authHandler =
    mkAuthHandler handler
    where handler :: Request -> Handler AuthToken
          handler req =
            case lookup "Auth-Token" (requestHeaders req) of
                Nothing ->
                    throwError $ err401  { errBody = "Missing Auth-Token Header" }
                Just authToken ->
                    return . AuthToken $ decodeUtf8 authToken


-- | Build an entropy source for session secrets.
sessionEntropy :: MonadIO m => m RandomSource
sessionEntropy = mkRandomSource drgNew 5000

-- | Contains the Cookie Authorization Contect for the Server.
authServerContext :: PersistentServerKey -> Context (AuthHandler Request (ExtendedPayloadWrapper AuthToken) ': AuthHandler Request AuthToken ': '[])
authServerContext secret = defaultAuthHandler cookieSettings secret :. authHandler :. EmptyContext
