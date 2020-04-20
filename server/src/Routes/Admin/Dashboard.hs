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
    , getCurrentTime, addDays, utcToLocalTime, localTimeToUTC
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
        , drdDailySales :: [DailySalesData]
        } deriving (Show)

instance ToJSON DashboardReportsData where
    toJSON DashboardReportsData {..} =
        object
            [ "orders" .= drdOrders
            , "customers" .= drdCustomers
            , "dailySales" .= drdDailySales
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

data DailySalesData =
    DailySalesData
        { dsdDay :: UTCTime
        , dsdTotal :: Cents
        } deriving (Show)

instance ToJSON DailySalesData where
    toJSON DailySalesData {..} =
        object
            [ "day" .= dsdDay
            , "total" .= dsdTotal
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
    DashboardReportsData orders customers <$> getDailySalesReport
  where
    makeCustomer :: Entity Customer -> DashboardCustomer
    makeCustomer (Entity cId c) =
        DashboardCustomer
            { dcId = cId
            , dcEmail = customerEmail c
            }

getDailySalesReport :: MonadIO m => SqlPersistT m [DailySalesData]
getDailySalesReport = do
    daysToQuery <- getDays
    forM daysToQuery $ \(startTime, endTime) -> do
        orders <- selectList [OrderCreatedAt >=. startTime, OrderCreatedAt <. endTime] []
        products <- selectList [OrderProductOrderId <-. map entityKey orders] []
        items <- selectList [OrderLineItemOrderId <-. map entityKey orders] []
        let total = getOrderTotal (map entityVal items) (map entityVal products)
        return $ DailySalesData startTime total
  where
    getDays :: MonadIO m => m [(UTCTime, UTCTime)]
    getDays = do
        zone <- liftIO getCurrentTimeZone
        today <- localDay . utcToLocalTime zone <$> liftIO getCurrentTime
        let days =  [addDays (-31) today .. today]
        return $ map (\d -> (toTime zone d, toTime zone $ addDays 1 d)) days
    toTime :: TimeZone -> Day -> UTCTime
    toTime zone day =
        localTimeToUTC zone $ LocalTime day midnight
