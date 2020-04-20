{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Dashboard
    ( DashboardAPI
    , dashboardRoutes
    ) where

import Control.Monad (forM)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Time (UTCTime)
import Database.Persist ((==.), Entity(..), SelectOpt(..), selectList)
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
        } deriving (Show)

instance ToJSON DashboardReportsData where
    toJSON DashboardReportsData {..} =
        object
            [ "orders" .= drdOrders
            , "customers" .= drdCustomers
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
    return $ DashboardReportsData orders customers
  where
    makeCustomer :: Entity Customer -> DashboardCustomer
    makeCustomer (Entity cId c) =
        DashboardCustomer
            { dcId = cId
            , dcEmail = customerEmail c
            }
