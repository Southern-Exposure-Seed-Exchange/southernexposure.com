{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Server
    ( App
    , AppSQL
    , runDB
    , stripeRequest
    , avalaraRequest
    , serverError
    , logMsg
    , msgLoggerIO
    , readCache
    , writeCache
    ) where

import Control.Concurrent.STM (atomically, readTVarIO, modifyTVar)
import Control.Exception.Safe (MonadCatch, displayException, tryAny, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, lift, runReaderT)
import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Servant (Handler, throwError)
import System.Log.FastLogger (pushLogStrLn, toLogStr)
import Web.Stripe

import Cache (Caches)
import Config (Config(..))

import qualified Avalara
import qualified Data.Text as T


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
    result <- liftIO $ stripe stripeConfig req
    case result of
        Left e -> do
            logger <- asks getStripeLogger
            let requestString =
                    show (method req)
                        ++ " - " ++ T.unpack (endpoint req)
                        ++ " ? " ++ show (queryParams req)
            liftIO . pushLogStrLn logger . toLogStr
                $ "StripeError from request(" ++ requestString ++ "): " ++ show e
            return result
        _ ->
            return result


-- | Perform a request to Avalara's AvaTax API.
--
-- TODO: Add a setting allowing us to skip Avalara calls, since
-- eventaully our Sandbox account will expire.
--
-- TODO: Log the request data as well! Maybe refactor to:
--       @(a -> ReaderT .... b) -> a -> App b@
avalaraRequest
    :: (MonadReader Config m, MonadCatch m, MonadIO m)
    => ReaderT Avalara.Config IO (Avalara.WithError a)
    -> m (Avalara.WithError a)
avalaraRequest req = do
    cfg <- asks getAvalaraConfig
    response <- tryAny $ liftIO $ runReaderT req cfg
    loggerSet <- asks getAvalaraLogger
    case response of
        Left e -> do
            liftIO . pushLogStrLn loggerSet . toLogStr $ show e
            throwM e
        Right r@(Avalara.SuccessfulResponse _) ->
            return r
        Right r@(Avalara.ErrorResponse err) -> do
            liftIO . pushLogStrLn loggerSet . toLogStr $ show err
            return r
        Right r@(Avalara.HttpException err) -> do
            liftIO . pushLogStrLn loggerSet . toLogStr $ displayException err
            return r


-- | Throw an HTTP error.
serverError :: MonadError e Handler => e -> App a
serverError =
    lift . throwError


-- | Log a message to the server log.
logMsg :: Text -> App ()
logMsg msg = do
    loggerSet <- asks getServerLogger
    liftIO . pushLogStrLn loggerSet $ toLogStr msg

-- | Produce a function for logging messages in the IO monad.
msgLoggerIO :: App (Text -> IO ())
msgLoggerIO = do
    loggerSet <- asks getServerLogger
    return (pushLogStrLn loggerSet . toLogStr)


-- | Read a value from the available caches.
readCache :: (Caches -> a) -> App a
readCache selector =
    asks getCaches >>= fmap selector . liftIO . readTVarIO

-- | Apply some updater function to the caches.
writeCache :: (Caches -> Caches) -> App ()
writeCache updater =
    asks getCaches >>= liftIO . atomically . flip modifyTVar updater
