{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Server
    ( App
    , runApp
    , AppSQL
    , runDB
    , stripeRequest
    , avalaraRequest
    , serverError
    , logMsg
    , msgLoggerIO
    , readCache
    , writeCache
    , writeSetting
    ) where

import Control.Concurrent.STM (atomically, readTVarIO, modifyTVar, readTVar, writeTVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks, runReaderT)
import Control.Monad.Except (MonadError(..), ExceptT(..))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Servant (Handler(..))
import Web.Stripe
import UnliftIO (try, MonadUnliftIO, throwIO, catch, displayException, tryAny)
import Servant.Server (ServerError)
import Data.Coerce (coerce)
import Control.Monad.Catch (MonadThrow)

import Cache (Caches(..))
import Config (Config(..), timedLogStr)
import Models.DB (Settings, updateSettings)

import qualified Avalara
import qualified Data.Text as T


-- | `App` is the monad stack used in the Servant Handlers. It wraps
-- Servant's `Handler` monad in a `ReaderT` monad that holds the server's
-- `Config` data.
newtype App a = MkApp (ReaderT Config IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadReader Config)

instance MonadError ServerError App where
    throwError = throwIO
    catchError = catch

runApp :: App a -> Config -> Handler a
runApp = coerce . try @_ @ServerError 



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
            liftIO . logger . timedLogStr
                $ "StripeError from request(" ++ requestString ++ "): " ++ show e
            return result
        _ ->
            return result


-- | Perform a request to Avalara's AvaTax API.
--
-- TODO: Add a setting allowing us to skip Avalara calls, since
-- eventually our Sandbox account will expire.
--
-- TODO: Log the request data as well! Maybe refactor to:
--       @(a -> ReaderT .... b) -> a -> App b@
avalaraRequest
    :: (MonadReader Config m, MonadUnliftIO m)
    => ReaderT Avalara.Config IO (Avalara.WithError a)
    -> m (Avalara.WithError a)
avalaraRequest req = do
    cfg <- asks getAvalaraConfig
    response <- tryAny $ liftIO $ runReaderT req cfg
    logger <- asks getAvalaraLogger
    case response of
        Left e -> do
            liftIO . logger . timedLogStr $ show e
            throwIO e
        Right r@(Avalara.SuccessfulResponse _) ->
            return r
        Right r@(Avalara.ErrorResponse err) -> do
            liftIO . logger . timedLogStr $ show err
            return r
        Right r@(Avalara.HttpException err) -> do
            liftIO . logger . timedLogStr $ displayException err
            return r


-- | Throw an HTTP error.
serverError :: ServerError -> App a
serverError = throwError


-- | Log a message to the server log.
logMsg :: Text -> App ()
logMsg msg = do
    logger <- asks getServerLogger
    liftIO . logger $ timedLogStr msg

-- | Produce a function for logging messages in the IO monad.
msgLoggerIO :: App (Text -> IO ())
msgLoggerIO = do
    logger <- asks getServerLogger
    return (logger . timedLogStr)


-- | Read a value from the available caches.
readCache :: (Caches -> a) -> App a
readCache selector =
    asks getCaches >>= fmap selector . liftIO . readTVarIO

-- | Apply some updater function to the caches.
writeCache :: (Caches -> Caches) -> App ()
writeCache updater =
    asks getCaches >>= liftIO . atomically . flip modifyTVar updater

-- | Apply some updater function to the settings cache, updating the
-- database value as well.
writeSetting :: (Settings -> Settings) -> App ()
writeSetting updater = do
    cacheTVar <- asks getCaches
    settings <- liftIO . atomically $ do
        cache <- readTVar cacheTVar
        let newSettings = updater $ getSettingsCache cache
            newCache = cache { getSettingsCache = newSettings }
        writeTVar cacheTVar newCache
        return newSettings
    void . runDB $ updateSettings settings
