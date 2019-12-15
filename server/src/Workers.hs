{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | This module contains definitions for asynchronous actions that can be
performed by worker threads outside the scope of the HTTP request/response
cycle.

It uses the database as a queue, which isn't ideal but should be fine for
our low volume of messages. It uses the 'ImmortalQueue' module to expose
a queue consumer with a pool of worker threads.

-}
module Workers
    ( taskQueueConfig
    , Task(..)
    , enqueueTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (Exception(..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(toJSON), FromJSON, Result(..), fromJSON)
import Data.Foldable (asum)
import Data.Monoid ((<>))
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Database.Persist.Sql
    ( (==.), (<=.), Entity(..), SelectOpt(..), ToBackendKey, SqlBackend
    , SqlPersistT, runSqlPool, selectFirst, delete, insert_, fromSqlKey
    )
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Log.FastLogger (pushLogStrLn, toLogStr)

import Config (Config(..))
import Images (makeImageConfig, scaleExistingImage, optimizeImage)
import ImmortalQueue (ImmortalQueue(..))
import Models
import Models.PersistJSON (JSONValue(..))
import Emails (EmailType, getEmailData)
import Server (AppSQL)

import qualified Data.Text as T
import qualified Emails


-- | The possible tasks our async worker queue can process.
data Task
    = OptimizeImage
        FilePath
        -- ^ FilePath
        FilePath
        -- ^ Destination Directory
    | SendEmail EmailType
    deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task


-- | Enqueue a Task to be run asynchronously. An optional run time can be
-- passed, otherwise it will execute immediately.
enqueueTask :: Maybe UTCTime -> Task -> AppSQL ()
enqueueTask runAt task = do
    time <- liftIO getCurrentTime
    insert_ Job
        { jobAction = JSONValue $ toJSON task
        , jobQueuedAt = time
        , jobRunAt = runAt
        , jobRetries = 0
        }


-- | Process a queue of Jobs by querying the database and sending any due
-- jobs to worker threads.
taskQueueConfig :: Natural -> Config -> ImmortalQueue (Entity Job)
taskQueueConfig threadCount cfg@Config { getPool, getServerLogger } =
    ImmortalQueue
        { qThreadCount = threadCount
        , qPollWorkerTime = 1000
        , qPop = getNextItem
        , qPush = runSql . insert_ . entityVal
        , qHandler = performTask
        , qFailure = handleError
        }
  where
    runSql :: SqlPersistT IO a -> IO a
    runSql = flip runSqlPool getPool

    -- Grab the next item from Job table, preferring the jobs that have
    -- passed their scheduled run time. When a job is found, remove it from
    -- the database. If there are no jobs, wait 5 seconds before trying
    -- again.
    getNextItem :: IO (Entity Job)
    getNextItem = do
        currentTime <- getCurrentTime
        maybeJob <- runSql $
            (asum <$> sequence
                [ selectFirst [JobRunAt <=. Just currentTime]
                    [Asc JobRunAt]
                , selectFirst [JobRunAt ==. Nothing]
                    [Asc JobQueuedAt]
                ]) >>=
                    maybe (return Nothing)
                        (\e -> delete (entityKey e) >> return (Just e))
        case maybeJob of
            Nothing ->
                threadDelay (5 * 1000000) >> getNextItem
            Just a ->
                return a

    -- When an error occurs, log a message & re-add the job to the database.
    handleError :: Exception e => Entity Job -> e -> IO ()
    handleError (Entity _ job) e = case fromJSON (fromJSONValue $ jobAction job) of
        Error err -> do
            requeueJob job 3600
            logMsg $
                "Cannot Decode Worker Job: " <> T.pack err
        Success action -> do
            requeueJob job 150
            logMsg $
                "Worker Job Failed: " <> describeTask action
                    <> " - " <> T.pack (displayException e)

    -- Re add a job to the databse in the given amount of seconds.
    requeueJob :: Job -> Integer -> IO ()
    requeueJob job postponeSeconds = do
        currentTime <- getCurrentTime
        let reRunAt = addUTCTime (fromInteger postponeSeconds) currentTime
        runSql $ insert_  job
            { jobRetries =  jobRetries job + 1
            , jobRunAt = Just reRunAt
            }

    -- Log a message to the server log.
    logMsg :: T.Text -> IO ()
    logMsg =
        pushLogStrLn getServerLogger . toLogStr

    -- Describe the task for a log message.
    describeTask :: Task -> T.Text
    describeTask = \case
        OptimizeImage filePath _ ->
            "Optimize " <> T.pack filePath
        SendEmail emailType -> case emailType of
            Emails.AccountCreated cId ->
                "Customer #" <> showSqlKey cId <> " Created Account Email"
            Emails.PasswordReset cId _ ->
                "Customer #" <> showSqlKey cId <> " Requested Password Reset Email"
            Emails.PasswordResetSuccess cId ->
                "Customer #" <> showSqlKey cId <> " Password Reset Succeeded Email"
            Emails.OrderPlaced oId ->
                "Order #" <> showSqlKey oId <> " Placed Email"

    showSqlKey :: (ToBackendKey SqlBackend a) => Key a -> T.Text
    showSqlKey =
        T.pack . show .fromSqlKey

    -- Perform the action specified by the job, throwing an error if we
    -- cannot decode the Task.
    performTask :: Entity Job -> IO ()
    performTask (Entity _ job) =
        case fromJSON (fromJSONValue $ jobAction job) of
            Error decodingError ->
                error decodingError
            Success (OptimizeImage filePath destinationDirectory) -> do
                imgCfg <- makeImageConfig
                optimizeImage imgCfg filePath
                scaleExistingImage imgCfg filePath destinationDirectory
            Success (SendEmail emailType) ->
                runSql (getEmailData emailType) >>= \case
                    Left err ->
                        error $ T.unpack err
                    Right emailData ->
                        Emails.send cfg emailData
