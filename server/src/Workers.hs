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
    , AvalaraTask(..)
    , enqueueTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (Exception(..), throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT, asks, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (ToJSON(toJSON), FromJSON, Result(..), fromJSON)
import Data.Foldable (asum)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Scientific (Scientific)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Database.Persist.Sql
    ( (=.), (==.), (<=.), (<.), Entity(..), SelectOpt(..), ToBackendKey, SqlBackend
    , SqlPersistT, runSqlPool, selectFirst, delete, insert_, fromSqlKey
    , deleteWhere, update
    )
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Log.FastLogger (pushLogStrLn, toLogStr)

import Avalara
    ( RefundTransactionRequest(..), VoidTransactionRequest(..), VoidReason(..)
    , CommitTransactionRequest(..)
    )
import Config (Config(..))
import Emails (EmailType, getEmailData)
import Images (makeImageConfig, scaleExistingImage, optimizeImage)
import ImmortalQueue (ImmortalQueue(..))
import Models
import Models.PersistJSON (JSONValue(..))
import Models.Fields (AvalaraTransactionCode(..))
import Routes.AvalaraUtils (createAvalaraTransaction)
import Server (avalaraRequest)

import qualified Avalara
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Emails


-- | The possible tasks our async worker queue can process.
data Task
    = OptimizeImage
        FilePath
        -- ^ FilePath
        FilePath
        -- ^ Destination Directory
    | SendEmail EmailType
    | CleanDatabase
    | Avalara AvalaraTask
    deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

data AvalaraTask
    = RefundTransaction AvalaraTransactionCode Avalara.RefundType (Maybe Scientific)
    -- ^ Refund the Transction with given RefundType and an optional
    -- Percentage.
    | CreateTransaction OrderId
    -- ^ Create a Transaction for the Order, marking the Tax as included in
    -- the order total.
    | CommitTransaction Avalara.Transaction
    -- ^ Commit an existing transaction.
    | VoidTransaction Avalara.Transaction
    -- ^ Void/delete a Transaction that was made but whose payment
    -- processing failed.
    deriving (Show, Generic)

instance FromJSON AvalaraTask
instance ToJSON AvalaraTask

data AvalaraError
    = RequestFailed (Avalara.WithError Avalara.Transaction)
    | TransactionCreationFailed
    | NoTransactionCode
    | OrderNotFound
    deriving (Show)

instance Exception AvalaraError


-- | Enqueue a Task to be run asynchronously. An optional run time can be
-- passed, otherwise it will execute immediately.
enqueueTask :: MonadIO m => Maybe UTCTime -> Task -> SqlPersistT m ()
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
    runSql :: MonadBaseControl IO m => SqlPersistT m a -> m a
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
        CleanDatabase ->
            "Clean Database"
        Avalara (RefundTransaction code type_ amount) ->
            "Refund Avalara Transaction ("
                <> T.pack (show code) <> "; " <> T.pack (show type_) <> "; "
                <> T.pack (show amount) <> ")"
        Avalara (CreateTransaction orderId) ->
            "Create Avalara Transaction for Order #" <> showSqlKey orderId
        Avalara (CommitTransaction trans) ->
            "Commit Avalara Transaction " <> T.pack (show $ Avalara.tCode trans)
        Avalara (VoidTransaction trans) ->
            "Void Avalara Transaction " <> T.pack (show $ Avalara.tCode trans)

    showSqlKey :: (ToBackendKey SqlBackend a) => Key a -> T.Text
    showSqlKey =
        T.pack . show . fromSqlKey

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
            Success CleanDatabase ->
                runSql cleanDatabase
            Success (Avalara task) ->
                performAvalaraTask task

    -- Perform an Avalara-specific action.
    --
    -- TODO: Use an exception instead of `error`
    performAvalaraTask :: AvalaraTask -> IO ()
    performAvalaraTask = \case
        RefundTransaction code refundType refundPercent -> do
            date <- getCurrentTime
            let request =
                    RefundTransactionRequest
                        { rtrTransctionCode = Nothing
                        , rtrDate = date
                        , rtrType = refundType
                        , rtrPercentage = refundPercent
                        , rtrLines = []
                        , rtrReferenceCode = Nothing
                        }
                (AvalaraTransactionCode companyCode transctionCode) = code
            runReaderT (avalaraRequest $ Avalara.refundTransaction companyCode transctionCode request) cfg >>= \case
                Avalara.SuccessfulResponse _ ->
                    return ()
                e ->
                    throwM $ RequestFailed e
        CreateTransaction orderId -> flip runReaderT cfg $ do
            result <- fmap listToMaybe . runSql $ E.select $ E.from
                $ \(o `E.InnerJoin` sa `E.LeftOuterJoin` ba `E.InnerJoin` c) -> do
                    E.on $ c E.^. CustomerId E.==. o E.^. OrderCustomerId
                    E.on $ o E.^. OrderBillingAddressId E.==. ba E.?. AddressId
                    E.on $ o E.^. OrderShippingAddressId E.==. sa E.^. AddressId
                    E.where_ $ o E.^. OrderId E.==. E.val orderId
                    return (o, sa, ba, c)
            case result of
                Nothing ->
                    throwM OrderNotFound
                Just (o, sa, ba, c) -> runSql $
                    createAvalaraTransaction o sa ba c True >>= \case
                        Nothing ->
                            throwM TransactionCreationFailed
                        Just transaction -> do
                            companyCode <- lift $ asks getAvalaraCompanyCode
                            update orderId
                                [ OrderAvalaraTransactionCode =.
                                    AvalaraTransactionCode companyCode
                                        <$> Avalara.tCode transaction
                                ]
        CommitTransaction transaction -> do
            let companyCode = getAvalaraCompanyCode cfg
            transactionCode <- case Avalara.tCode transaction of
                Nothing ->
                    throwM NoTransactionCode
                Just tCode ->
                    return tCode
            let request = Avalara.commitTransaction companyCode transactionCode
                    $ CommitTransactionRequest { ctsrCommit = True }
            runReaderT (avalaraRequest request) cfg >>= \case
                Avalara.SuccessfulResponse _ ->
                    return ()
                e ->
                    throwM $ RequestFailed e
        VoidTransaction transaction -> do
            let companyCode = getAvalaraCompanyCode cfg
            transactionCode <- case Avalara.tCode transaction of
                Nothing ->
                    throwM NoTransactionCode
                Just tCode ->
                    return tCode
            let request =
                    Avalara.voidTransaction companyCode transactionCode
                        $ VoidTransactionRequest { vtrCode = DocDeleted }
            runReaderT (avalaraRequest request) cfg >>= \case
                Avalara.SuccessfulResponse _ ->
                    return ()
                e ->
                    throwM $ RequestFailed e


-- | Remove Expired Carts & PasswordResets, Deactivate Expired Coupons.
cleanDatabase :: SqlPersistT IO ()
cleanDatabase = do
    currentTime <- lift getCurrentTime
    deleteWhere [PasswordResetExpirationTime <. currentTime]
    deleteWhere [CartExpirationTime <. Just currentTime]
    deactivateCoupons currentTime
    enqueueTask (Just $ addUTCTime 3600 currentTime) CleanDatabase
  where
    -- De-activate coupons whose expiration date has passed and coupons
    -- that have reached their maximum number of uses.
    deactivateCoupons :: UTCTime -> SqlPersistT IO ()
    deactivateCoupons currentTime =
        E.update $ \c -> do
            E.set c [ CouponIsActive E.=. E.val False ]
            let orderCount = E.sub_select $ E.from $ \o -> do
                    E.where_ $ o E.^. OrderCouponId E.==. E.just (c E.^. CouponId)
                    return E.countRows
            E.where_ $
                (orderCount E.>=. c E.^. CouponTotalUses E.&&. c E.^. CouponTotalUses E.!=. E.val 0)
                E.||.  E.val currentTime E.>. c E.^. CouponExpirationDate
