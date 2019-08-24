{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Server
    ( App
    , AppSQL
    , runDB
    , stripeRequest
    , serverError
    ) where

import Control.Monad.Reader (ReaderT, asks, lift, liftIO)
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Servant (Handler, throwError)
import Web.Stripe

import Config


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


serverError :: MonadError e Handler => e -> App a
serverError =
    lift . throwError
