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
    , removeSessionCookie
    , withCookie
    , withValidatedCookie
    , validateCookieAndParameters
    , temporarySession
    , permanentSession
      -- * Server Setup
    , sessionEntropy
    , mkPersistentServerKey
    , authServerContext
    ) where

import Control.Monad ((>=>))
import Control.Monad.Reader (MonadIO, MonadReader, ask, asks)
import Crypto.Random (drgNew)
import Data.Default (def)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Database.Persist (Entity(..), getBy)
import GHC.Generics (Generic)
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
    ( AuthCookieData, ExtendedPayloadWrapper, AuthCookieSettings(..)
    , PersistentServerKey, RandomSource, SessionSettings(..), EncryptedSession
    , CookiedWrapperClass, ExpirationType(..), Cookied, mkPersistentServerKey
    , defaultAuthHandler, addSession, removeSession, cookied, mkRandomSource
    )

import Config (Config(getCookieSecret, getCookieEntropySource))
import Models.DB (Unique(UniqueToken), Customer(customerAuthToken))
import Server (App, runDB, serverError)
import Validation (Validation(..))

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

-- | Authorize the user by setting their session cookie.
addSessionCookie :: AddHeader e EncryptedSession a b => SessionSettings -> AuthToken -> a -> App b
addSessionCookie ss token val = do
    entropy <- asks getCookieEntropySource
    secret <- asks getCookieSecret
    addSession cookieSettings entropy secret ss token val

-- | De-authorize the user by invalidating their session cookie.
removeSessionCookie :: AddHeader e EncryptedSession a b => a -> App b
removeSessionCookie = removeSession cookieSettings

-- | Extract the AuthToken from the wrapper and pass it to a handler
-- function.
withCookie
    :: ( CookiedWrapperClass f (WrappedAuthToken -> m b) AuthToken
       , MonadReader Config m
       )
    => WrappedAuthToken
    -> f
    -> m b
withCookie token handler = ask >>= cookied_
  where
    cookied_ cfg =
        cookied cookieSettings (getCookieEntropySource cfg) (getCookieSecret cfg)
            (Proxy @AuthToken) handler token

-- | Extract and validate the AuthToken and pass the Custoemr to a handler
-- function. Throws a 403 error if the AuthToken does not match a Customer.
withValidatedCookie
    :: CookiedWrapperClass (App c) (App b) AuthToken
    => WrappedAuthToken
    -> (Entity Customer -> App c)
    -> App b
withValidatedCookie token handler =
    withCookie token $ validateToken >=> handler

-- | Validate both the cookie & route parameters. Throws a 403 if the
-- AuthToken is invalid and a 422 is the parameters are invalid.
--
-- The arguments are switched here to facilitate ETA reductions for routes
-- where these are the only arguments.
validateCookieAndParameters
    :: (CookiedWrapperClass (App c) (App b) AuthToken, Validation p)
    => (Entity Customer -> p -> App c)
    -> WrappedAuthToken
    -> p
    -> App b
validateCookieAndParameters handler token param =
    withCookie token $ \authToken -> do
        customer <- validateToken authToken
        validate param >>= handler customer


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


-- | Build an entropy source for session secrets.
sessionEntropy :: MonadIO m => m RandomSource
sessionEntropy = mkRandomSource drgNew 5000

-- | Contains the Cookie Authorization Contect for the Server.
authServerContext :: PersistentServerKey -> Context (AuthHandler Request WrappedAuthToken ': '[])
authServerContext secret = defaultAuthHandler cookieSettings secret :. EmptyContext
