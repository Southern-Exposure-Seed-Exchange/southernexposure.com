{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| This module is responsible for designating the various caches we use to
improve server performance, as well as functions for initializing
& querying them.
-}
module Cache
    ( Caches(..)
    , emptyCache
    , initializeCaches
    , CategoryPredecessorCache
    , syncCategoryPredecessorCache
    , queryCategoryPredecessorCache
    , SalesReports(..)
    , SalesData(..)
    )
    where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time
    ( UTCTime, LocalTime(..), Day, TimeZone, getCurrentTimeZone, midnight
    , getCurrentTime, addDays, utcToLocalTime, localTimeToUTC, toGregorian
    , fromGregorian, addGregorianMonthsClip
    )
import Database.Persist ((>=.), (<.), (<-.), Entity(..), selectList)
import Database.Persist.Sql (SqlPersistT)

import Models.DB
import Models.Fields (Cents(..), mkCents, creditLineItemTypes)

import qualified Data.Map.Strict as M

-- | Each cache is available as a field of the Caches type.
data Caches
    = Caches
        { getCategoryPredecessorCache :: CategoryPredecessorCache
        , getSettingsCache :: Settings
        , getSalesReportCache :: SalesReports
        }

-- | An empty set of caches.
emptyCache :: Caches
emptyCache =
    Caches (CategoryPredecessorCache M.empty) defaultSettings emptyReports

-- | Build all the caches.
initializeCaches :: MonadIO m => SqlPersistT m Caches
initializeCaches =
    Caches
        <$> syncCategoryPredecessorCache
        <*> (entityVal <$> getSettings)
        <*> generateSalesReports



-- | A map from 'CategoryId' to a list of all predecessor 'Category' entities.
-- This prevents the excessive/recursive SQL queries required for fetching
-- the parent tree for a Category.
newtype CategoryPredecessorCache
    = CategoryPredecessorCache (M.Map CategoryId [Entity Category])

-- | Build the cache from a single SQL query.
syncCategoryPredecessorCache :: MonadIO m => SqlPersistT m CategoryPredecessorCache
syncCategoryPredecessorCache = do
    categories <- selectList [] []
    let categoryMap = foldl (\m e@(Entity key _) -> M.insert key e m) M.empty categories
    return . CategoryPredecessorCache $ makeCacheMap categoryMap categories
    where
        makeCacheMap
            :: M.Map CategoryId (Entity Category)
            -> [Entity Category]
            -> M.Map CategoryId [Entity Category]
        makeCacheMap categoryMap =
            foldl
                (\m (Entity categoryId category) ->
                    M.insert categoryId
                        (getParents categoryMap $ maybeToList $ categoryParentId category)
                        m
                ) M.empty
        getParents :: M.Map CategoryId (Entity Category) -> [CategoryId] -> [Entity Category]
        getParents categoryMap parents = case parents of
            [] ->
                []
            nextParentId : rest ->
                case M.lookup nextParentId categoryMap of
                    Nothing ->
                        getParents categoryMap rest
                    Just e@(Entity _ parent) ->
                        e : getParents categoryMap (rest ++ maybeToList (categoryParentId parent))

-- | Query the cache for all predecessors(parents, grandparents, etc.) of
-- a 'Category'.
--
-- Note: The returned list will be ordered by increasing depth. I.e.,
-- @[parent, grandparent, great-grandparent, etc.]@
queryCategoryPredecessorCache :: CategoryId -> CategoryPredecessorCache -> [Entity Category]
queryCategoryPredecessorCache categoryId (CategoryPredecessorCache cache) =
    fromMaybe [] $ M.lookup categoryId cache



data SalesReports =
    SalesReports
        { srDailySales :: [SalesData]
        , srMonthlySales :: [SalesData]
        } deriving (Show)


data SalesData =
    SalesData
        { sdDay :: UTCTime
        , sdTotal :: Cents
        } deriving (Show)

instance ToJSON SalesData where
    toJSON SalesData {..} =
        object
            [ "day" .= sdDay
            , "total" .= sdTotal
            ]

emptyReports :: SalesReports
emptyReports =
    SalesReports [] []

generateSalesReports :: MonadIO m => SqlPersistT m SalesReports
generateSalesReports = do
    srDailySales <- getDailySalesReport
    srMonthlySales <- getMonthlySalesReport
    return SalesReports {..}

getDailySalesReport :: MonadIO m => SqlPersistT m [SalesData]
getDailySalesReport =
    getDays >>= mapM (uncurry getTotalForTimePeriod)
  where
    getDays :: MonadIO m => m [(UTCTime, UTCTime)]
    getDays = do
        zone <- liftIO getCurrentTimeZone
        today <- localDay . utcToLocalTime zone <$> liftIO getCurrentTime
        let days =  [addDays (-31) today .. today]
        return $ map (\d -> (toTime zone d, toTime zone $ addDays 1 d)) days

getMonthlySalesReport :: MonadIO m => SqlPersistT m [SalesData]
getMonthlySalesReport =
    getMonths >>= mapM (uncurry getTotalForTimePeriod)
  where
    getMonths :: MonadIO m => m [(UTCTime, UTCTime)]
    getMonths = do
        zone <- liftIO getCurrentTimeZone
        today <- localDay . utcToLocalTime zone <$> liftIO getCurrentTime
        let startOfMonth = (\(y, m, _) -> fromGregorian y m 1) $ toGregorian today
            months = map (`addGregorianMonthsClip` startOfMonth) [-11 .. 0]
        return $ map
            (\d ->
                ( toTime zone d
                , toTime zone $ addGregorianMonthsClip 1 d
                )
            ) months

toTime :: TimeZone -> Day -> UTCTime
toTime zone day =
    localTimeToUTC zone $ LocalTime day midnight

getTotalForTimePeriod :: MonadIO m => UTCTime -> UTCTime -> SqlPersistT m SalesData
getTotalForTimePeriod startTime endTime = do
    orders <- selectList [OrderCreatedAt >=. startTime, OrderCreatedAt <. endTime] []
    products <- selectList [OrderProductOrderId <-. map entityKey orders] []
    items <- selectList [OrderLineItemOrderId <-. map entityKey orders] []
    let total = getOrderTotal items products
    return $ SalesData startTime total
  where
    getOrderTotal items products = Cents . fromIntegral $
        integerCents (sum $ map productTotal products) + sum (map lineTotal items)
    productTotal :: Entity OrderProduct -> Cents
    productTotal (Entity _ p) =
        mkCents (orderProductQuantity p) * orderProductPrice p
    lineTotal :: Entity OrderLineItem -> Integer
    lineTotal (Entity _ l) =
        let amount = integerCents $ orderLineItemAmount l
        in
        if orderLineItemType l `elem` creditLineItemTypes then
            (-1) * amount
        else
            amount
    integerCents :: Cents -> Integer
    integerCents (Cents c) =
        fromIntegral c
