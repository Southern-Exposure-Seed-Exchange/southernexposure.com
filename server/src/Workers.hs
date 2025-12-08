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
our low volume of messages. It uses the @immortal-queue@ package to expose
a queue consumer with a pool of worker threads.

-}
module Workers
    ( taskQueueConfig
    , Task(..)
    , AvalaraTask(..)
    , enqueueTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import UnliftIO.Exception (Exception(..), throwIO)
import Control.Immortal.Queue (ImmortalQueue(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT, asks, lift)
import Data.Aeson (ToJSON(toJSON), FromJSON, Result(..), fromJSON)
import Data.Foldable (asum)
import Data.Maybe (listToMaybe)
import Data.Scientific (Scientific)
import Data.Time
    ( UTCTime, TimeZone, Day, LocalTime(..), getCurrentTime, getCurrentTimeZone
    , addUTCTime, utcToLocalTime, localTimeToUTC, fromGregorian, toGregorian
    , midnight, addDays
    )
import Database.Persist.Sql
    ( (=.), (==.), (<=.), (<.), (<-.), Entity(..), SelectOpt(..), ToBackendKey
    , SqlBackend, SqlPersistT, runSqlPool, selectList, selectFirst, delete
    , insert_, fromSqlKey, deleteWhere, update, get
    )
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import UnliftIO (MonadUnliftIO)

import Avalara
    ( RefundTransactionRequest(..), VoidTransactionRequest(..), VoidReason(..)
    , CommitTransactionRequest(..)
    )
import Cache (Caches(..), SalesReports(..), SalesData(..))
import Config (Config(..), AvalaraStatus(AvalaraTesting), timedLogStr)
import Emails (EmailType, getEmailData)
import Images (makeImageConfig, scaleExistingImage, optimizeImage)
import Models
import Models.PersistJSON (JSONValue(..))
import Models.Fields (AvalaraTransactionCode(..), Cents(..), plusCents)
import Routes.AvalaraUtils (createAvalaraTransaction)
import Server (avalaraRequest)

import qualified Avalara
import qualified Data.Text as T
import qualified Database.Esqueleto.Experimental as E
import qualified Emails


-- | The possible tasks our async worker queue can process.
data Task
    = OptimizeImage
        FilePath
        -- FilePath
        FilePath
        -- Destination Directory
    | SendEmail EmailType
    | CleanDatabase
    | Avalara AvalaraTask
    | UpdateSalesCache OrderId
    -- ^ Record a new Order in the SalesReports Cache
    | AddSalesReportDay
    -- ^ Add today to the SalesReports Cache if it does not exist
    | RemoveSoldOutProducts OrderId
    -- ^ Remove any products in the Order from Carts if they are now sold
    -- out.
    deriving (Show, Generic, Eq)

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
    deriving (Show, Generic, Eq)

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
taskQueueConfig threadCount cfg@Config { getPool, getServerLogger, getCaches } =
    ImmortalQueue
        { qThreadCount = threadCount
        , qPollWorkerTime = 1000
        , qPop = getNextItem
        , qPush = runSql . insert_ . entityVal
        , qHandler = performTask
        , qFailure = handleError
        }
  where
    runSql :: MonadUnliftIO m => SqlPersistT m a -> m a
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
            when (jobRetries job <= 5) $
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
        getServerLogger . timedLogStr

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
            Emails.EmailVerification cId _ ->
                "Customer #" <> showSqlKey cId <> " Email Verification Email"
            Emails.OrderPlaced oId ->
                "Order #" <> showSqlKey oId <> " Placed Email"
            Emails.OrderStatusUpdated oId _ ->
                "Order #" <> showSqlKey oId <> " Status Updated Email"
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
        UpdateSalesCache orderId ->
            "Update Sales Reports Cache with Order #" <> showSqlKey orderId
        AddSalesReportDay ->
            "Add Sales Report Day"
        RemoveSoldOutProducts orderId ->
            "Remove Sold Out Products in Order #" <> showSqlKey orderId <> " From Carts"

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
                        Emails.sendWithRetries cfg emailData >>= wait
            Success CleanDatabase ->
                runSql cleanDatabase
            Success (Avalara task) ->
                performAvalaraTask task
            Success (UpdateSalesCache orderId) ->
                runSql $ updateSalesCache getCaches orderId
            Success AddSalesReportDay ->
                runSql $ addNewReportDate getCaches
            Success (RemoveSoldOutProducts orderId) ->
                runSql $ removeSoldOutVariants orderId

    -- Perform an Avalara-specific action.
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
                    throwIO $ RequestFailed e
        CreateTransaction orderId -> flip runReaderT cfg $ do
            result <- fmap listToMaybe . runSql $ E.select $ do
                (o E.:& sa E.:& ba E.:& c) <- E.from $ E.table
                    `E.innerJoin` E.table
                        `E.on` (\(o E.:& sa) -> o E.^. OrderShippingAddressId E.==. sa E.^. AddressId)
                    `E.leftJoin` E.table
                        `E.on` (\(o E.:& _ E.:& ba) -> o E.^. OrderBillingAddressId E.==. ba E.?. AddressId)
                    `E.innerJoin` E.table
                        `E.on` (\(o E.:& _ E.:& _ E.:& c) -> c E.^. CustomerId E.==. o E.^. OrderCustomerId)
                E.where_ $ o E.^. OrderId E.==. E.val orderId
                return (o, sa, ba, c)
            case result of
                Nothing ->
                    throwIO OrderNotFound
                Just (o, sa, ba, c) -> runSql $
                    createAvalaraTransaction o sa ba c True >>= \case
                        Nothing ->
                            throwIO TransactionCreationFailed
                        Just transaction -> do
                            when (getAvalaraStatus cfg == AvalaraTesting) $
                                enqueueTask Nothing . Avalara
                                    $ VoidTransaction transaction
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
                    throwIO NoTransactionCode
                Just tCode ->
                    return tCode
            let request = Avalara.commitTransaction companyCode transactionCode
                    $ CommitTransactionRequest { ctsrCommit = True }
            runReaderT (avalaraRequest request) cfg >>= \case
                Avalara.SuccessfulResponse _ ->
                    return ()
                e ->
                    throwIO $ RequestFailed e
        VoidTransaction transaction -> do
            let companyCode = getAvalaraCompanyCode cfg
            transactionCode <- case Avalara.tCode transaction of
                Nothing ->
                    throwIO NoTransactionCode
                Just tCode ->
                    return tCode
            let request =
                    Avalara.voidTransaction companyCode transactionCode
                        $ VoidTransactionRequest { vtrCode = DocDeleted }
            runReaderT (avalaraRequest request) cfg >>= \case
                Avalara.SuccessfulResponse _ ->
                    return ()
                e ->
                    throwIO $ RequestFailed e


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
            let orderCount = E.subSelectCount $ E.from E.table >>= \o -> do
                    E.where_ $ o E.^. OrderCouponId E.==. E.just (c E.^. CouponId)
            E.where_ $
                (orderCount E.>=. c E.^. CouponTotalUses E.&&. c E.^. CouponTotalUses E.!=. E.val 0)
                E.||.  E.val currentTime E.>. c E.^. CouponExpirationDate

-- | Remove Cart Items for sold-out variants from the order.
removeSoldOutVariants :: OrderId -> SqlPersistT IO ()
removeSoldOutVariants orderId = do
    soldOut <- E.select $ do
        (op E.:& pv) <- E.from $ E.table `E.innerJoin` E.table
            `E.on`  \(op E.:& pv) -> pv E.^. ProductVariantId E.==. op E.^. OrderProductProductVariantId
        E.where_ $ pv E.^. ProductVariantQuantity E.<=. E.val 0
            E.&&. op E.^. OrderProductOrderId E.==. E.val orderId
        return pv
    deleteWhere [CartItemProductVariantId <-. map entityKey soldOut]

-- | Add the given Order's total to the Daily & Monthly sales caches.
--
-- Adds a new SalesData if there is no SalesData for the Order's time
-- period.
updateSalesCache :: TVar Caches -> OrderId -> SqlPersistT IO ()
updateSalesCache cacheTVar orderId = do
    mOrderDate <- fmap orderCreatedAt <$> get orderId
    orderTotal <- getOrderTotal
        <$> (map entityVal <$> selectList [OrderLineItemOrderId ==. orderId] [])
        <*> (map entityVal <$> selectList [OrderProductOrderId ==. orderId] [])
    zone <- liftIO getCurrentTimeZone
    case mOrderDate of
        Nothing ->
            error "Could not fetch Order & date."
        Just orderDate ->
            lift . atomically $ do
                cache <- readTVar cacheTVar
                writeTVar cacheTVar $ cache
                    { getSalesReportCache =
                        updateSalesReports (getSalesReportCache cache) zone orderDate
                            (updateReport orderTotal orderDate)
                    }
  where
    updateReport :: Cents -> UTCTime -> [SalesData] -> SalesData -> [SalesData]
    updateReport total date sales newReport =
        case sales of
            [] ->
                [ newReport { sdTotal = total } ]
            [sale] ->
                if sdDay newReport == sdDay sale then
                    [ sale { sdTotal = total `plusCents` sdTotal sale } ]
                else
                    [ sale, newReport { sdTotal = total `plusCents` sdTotal sale } ]
            sale : nextSale : rest ->
                if date >= sdDay sale && date < sdDay nextSale then
                    sale { sdTotal = total `plusCents` sdTotal sale } : rest
                else
                    sale : updateReport total date (nextSale : rest) newReport

-- | Add a new SalesData entry for today to the SalesReports cache.
--
-- Does nothing if the entries already exist.
addNewReportDate :: TVar Caches -> SqlPersistT IO ()
addNewReportDate cacheTVar = do
    today <- liftIO getCurrentTime
    zone <- liftIO getCurrentTimeZone
    lift . atomically $ do
        cache <- readTVar cacheTVar
        writeTVar cacheTVar $ cache
            { getSalesReportCache =
                updateSalesReports (getSalesReportCache cache) zone today updateReport
            }
    enqueueTask (Just $ tomorrow zone today) AddSalesReportDay
  where
    tomorrow :: TimeZone -> UTCTime -> UTCTime
    tomorrow zone today =
        let day = localDay $ utcToLocalTime zone today
        in
        toTime zone $ addDays 1 day
    updateReport :: [SalesData] -> SalesData -> [SalesData]
    updateReport sales newReport =
        if sdDay newReport `notElem` map sdDay sales then
            drop 1 sales ++ [newReport]
        else
            sales

-- | Update the Monthly & Daily sales reports using a generic updater
-- function. The function is passed the reports and a Zero-total SalesData
-- corresponding to the passed UTCTime.
--
-- The length of the daily report will be limited to 31 items and the
-- monthly limited to 12 items.
updateSalesReports
    :: SalesReports -> TimeZone -> UTCTime -> ([SalesData] -> SalesData -> [SalesData]) -> SalesReports
updateSalesReports reports zone reportDate reportUpdater =
    reports
        { srDailySales =
            limitLength 31 $ reportUpdater (srDailySales reports) dailyReport
        , srMonthlySales =
            limitLength 12 $ reportUpdater (srMonthlySales reports) monthlyReport
        }
  where
    limitLength :: Int -> [a] -> [a]
    limitLength maxLength items =
        if length items > maxLength then
            drop (length items - maxLength) items
        else
            items
    -- Build a Daily SalesData item for the given date, starting at
    -- midnight of the day, local time.
    dailyReport :: SalesData
    dailyReport =
        let day = localDay $ utcToLocalTime zone reportDate
        in
        SalesData
            { sdDay = toTime zone day
            , sdTotal = Cents 0
            }
    -- Build a Monthly SalesData item for the given date, starting at the
    -- first of the month, local time.
    monthlyReport :: SalesData
    monthlyReport =
        let day = localDay $ utcToLocalTime zone reportDate
            startOfMonth =
                (\(y, m, _) -> fromGregorian y m 1) $ toGregorian day
        in
        SalesData
            { sdDay = toTime zone startOfMonth
            , sdTotal = Cents 0
            }

-- Convert a Day to a UTCTime representing midnight in the local timezone.
toTime :: TimeZone -> Day -> UTCTime
toTime zone day =
    localTimeToUTC zone $ LocalTime day midnight
