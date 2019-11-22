{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Server
    ( App
    , AppSQL
    , runDB
    , stripeRequest
    , avalaraRequest
    , serverError
    , readCache
    , writeCache
    ) where

import Control.Concurrent.STM (atomically, readTVarIO, modifyTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Servant (Handler, throwError)
import Web.Stripe

import Cache (Caches)
import Config (Config(getPool, getCaches, getStripeConfig, getAvalaraConfig))

import qualified Avalara


-- | `App` is the monad stack used in the Servant Handlers. It wraps
-- Servant's `Handler` monad in a `ReaderT` monad that holds the server's
-- `Config` data.
type App =
    ReaderT Config Handler

type AppSQL =
    SqlPersistT App

-- | Run & return a database query.
runDB :: AppSQL a -> App a
runDB query =
    asks getPool >>= runSqlPool query


-- | Perform a request to the Stripe API.
stripeRequest :: FromJSON (StripeReturn a)
              => StripeRequest a
              -> App (Either StripeError (StripeReturn a))
stripeRequest req = do
    stripeConfig <- asks getStripeConfig
    liftIO $ stripe stripeConfig req


-- | Perform a request to Avalara's AvaTax API.
--
-- TODO: Add a setting allowing us to skip Avalara calls, since
-- eventaully our Sandbox account will expire.
--
-- TODO: Use with `try`. When a request fails, log & return the error.
avalaraRequest :: ReaderT Avalara.Config IO a -> App a
avalaraRequest req =
    asks getAvalaraConfig >>= liftIO . runReaderT req

-- | Throw an HTTP error.
serverError :: MonadError e Handler => e -> App a
serverError =
    lift . throwError


-- | Read a value from the available caches.
readCache :: (Caches -> a) -> App a
readCache selector =
    asks getCaches >>= fmap selector . liftIO . readTVarIO

-- | Apply some updater function to the caches.
writeCache :: (Caches -> Caches) -> App ()
writeCache updater =
    asks getCaches >>= liftIO . atomically . flip modifyTVar updater
