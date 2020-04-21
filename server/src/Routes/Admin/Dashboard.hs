{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Dashboard
    ( DashboardAPI
    , dashboardRoutes
    ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Time
    ( UTCTime, LocalTime(..), Day, TimeZone, getCurrentTimeZone, midnight
    , getCurrentTime, addDays, utcToLocalTime, localTimeToUTC, toGregorian
    , fromGregorian, addGregorianMonthsClip
    )
import Database.Persist
    ( (==.), (>=.), (<.), (<-.), Entity(..), SelectOpt(..), selectList
    )
import Database.Persist.Sql (SqlPersistT)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models
import Models.Fields (Region, Cents)
import Server (App, runDB)

import qualified Data.Text as T
import qualified Database.Esqueleto as E


type DashboardAPI =
         "reports" :> DashboardReportsRoute

type DashboardRoutes =
         (WrappedAuthToken -> App (Cookied DashboardReportsData))

dashboardRoutes :: DashboardRoutes
dashboardRoutes =
         dashboardReportsRoute


-- REPORTS

type DashboardReportsRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied DashboardReportsData)

data DashboardReportsData =
    DashboardReportsData
        { drdOrders :: [DashboardOrder]
        , drdCustomers :: [DashboardCustomer]
        , drdDailySales :: [SalesData]
        , drdMonthlySales :: [SalesData]
        } deriving (Show)

instance ToJSON DashboardReportsData where
    toJSON DashboardReportsData {..} =
        object
            [ "orders" .= drdOrders
            , "customers" .= drdCustomers
            , "dailySales" .= drdDailySales
            , "monthlySales" .= drdMonthlySales
            ]

data DashboardOrder =
    DashboardOrder
        { doId :: OrderId
        , doDate :: UTCTime
        , doCustomer :: T.Text
        , doState :: Region
        , doTotal :: Cents
        } deriving (Show)

instance ToJSON DashboardOrder where
    toJSON DashboardOrder {..} =
        object
            [ "id" .= doId
            , "date" .= doDate
            , "customer" .= doCustomer
            , "state" .= doState
            , "total" .= doTotal
            ]

data DashboardCustomer =
    DashboardCustomer
        { dcId :: CustomerId
        , dcEmail :: T.Text
        } deriving (Show)

instance ToJSON DashboardCustomer where
    toJSON DashboardCustomer {..} =
        object
            [ "id" .= dcId
            , "email" .= dcEmail
            ]

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


dashboardReportsRoute :: WrappedAuthToken -> App (Cookied DashboardReportsData)
dashboardReportsRoute = flip withAdminCookie $ \_ -> runDB $ do
    rawOrders <- E.select $ E.from $ \(o `E.InnerJoin` sa) -> do
        E.on $ sa E.^. AddressId E.==. o E.^. OrderShippingAddressId
        E.limit 10
        E.orderBy [E.desc $ o E.^. OrderId]
        let customerName =
                sa E.^. AddressFirstName E.++. E.val " " E.++. sa E.^. AddressLastName
        return (o, customerName, sa E.^. AddressState)
    orders <- forM rawOrders $ \(Entity oId order, name, region) -> do
        total <- getOrderTotal
            <$> (fmap entityVal <$> selectList [OrderLineItemOrderId ==. oId] [])
            <*> (fmap entityVal <$> selectList [OrderProductOrderId ==. oId] [])
        return DashboardOrder
            { doId = oId
            , doDate = orderCreatedAt order
            , doCustomer = E.unValue name
            , doState = E.unValue region
            , doTotal = total
            }
    customers <- map makeCustomer <$> selectList [] [LimitTo 10, Desc CustomerId]
    DashboardReportsData orders customers <$> getDailySalesReport <*> getMonthlySalesReport
  where
    makeCustomer :: Entity Customer -> DashboardCustomer
    makeCustomer (Entity cId c) =
        DashboardCustomer
            { dcId = cId
            , dcEmail = customerEmail c
            }

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
    let total = getOrderTotal (map entityVal items) (map entityVal products)
    return $ SalesData startTime total
