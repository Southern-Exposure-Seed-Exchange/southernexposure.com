{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Orders
    ( OrderAPI
    , orderRoutes
    ) where

import Control.Monad (forM)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Database.Persist ((==.), Entity(..), Filter, SelectOpt(..), count, get, selectList)
import Servant ((:>), AuthProtect, QueryParam, Get, JSON)

import Auth (withAdminCookie, WrappedAuthToken, Cookied)
import Models (OrderId, Order(..), OrderProduct(..), OrderLineItem(..), Customer(customerEmail), Address(..), EntityField(..))
import Models.Fields (OrderStatus, Region, Cents(..), creditLineItemTypes)
import Server (App, runDB)

import qualified Data.List as L
import qualified Data.Text as T


type OrderAPI =
         "list" :> OrderListRoute

type OrderRoutes =
         (WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied OrderListData))

orderRoutes :: OrderRoutes
orderRoutes =
         orderListRoute


-- LIST


type OrderListRoute =
       AuthProtect "cookie-auth"
    :> QueryParam "page" Int
    :> QueryParam "perPage" Int
    :> QueryParam "query" T.Text
    :> Get '[JSON] (Cookied OrderListData)

data OrderListData =
    OrderListData
        { oldOrders :: [ListOrder]
        , oldTotalOrders :: Int
        } deriving (Show)

instance ToJSON OrderListData where
    toJSON OrderListData {..} =
        object
            [ "orders" .= oldOrders
            , "total" .= oldTotalOrders
            ]

data ListOrder =
    ListOrder
        { loId :: OrderId
        , loDate :: UTCTime
        , loCustomerName :: T.Text
        , loCustomerEmail :: T.Text
        , loShippingStreet :: T.Text
        , loShippingRegion :: Maybe Region
        , loOrderStatus :: OrderStatus
        , loOrderTotal :: Integer
        } deriving (Show)

instance ToJSON ListOrder where
    toJSON ListOrder {..} =
        object
            [ "id" .= loId
            , "date" .= loDate
            , "customerName" .= loCustomerName
            , "customerEmail" .= loCustomerEmail
            , "shippingStreet" .= loShippingStreet
            , "shippingRegion" .= loShippingRegion
            , "orderStatus" .= loOrderStatus
            , "orderTotal" .= loOrderTotal
            ]

orderListRoute :: WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied OrderListData)
orderListRoute token maybePage maybePerPage _ = withAdminCookie token $ \_ -> do
    let perPage = fromMaybe 50 maybePerPage
        page = fromMaybe 1 maybePage
        offset = perPage * (page - 1)
    (orders, orderCount) <- runDB $ do
        -- TODO: build where clause from query. How to handle w/o
        -- performance destroying joins while keeping accurate order counts?
        orderCount <- count ([] :: [Filter Order])
        orders <- do
            orders <- selectList []
                [ Desc OrderCreatedAt
                , LimitTo perPage
                , OffsetBy $ fromIntegral offset
                ]
            forM orders $ \o@(Entity orderId order) -> do
                products <- selectList [OrderProductOrderId ==. orderId] []
                lineItems <- selectList [OrderLineItemOrderId ==. orderId] []
                let (credits, debits) =
                        L.partition isCreditLine lineItems
                    productTotal =
                        sum $ map finalProductPrice products
                    creditTotal =
                        sum $ map (orderLineItemAmount . entityVal) credits
                    debitTotal =
                        sum $ map (orderLineItemAmount . entityVal) debits
                    total =
                        centsInt productTotal + centsInt debitTotal - centsInt creditTotal
                c <- get $ orderCustomerId order
                sa <- get $ orderShippingAddressId order
                return (o, c, sa, total)
        return (orders, orderCount)
    return OrderListData
        { oldOrders = map convertOrder orders
        , oldTotalOrders = orderCount
        }
  where
    -- | Is the line item a credit?
    isCreditLine :: Entity OrderLineItem -> Bool
    isCreditLine =
        (`elem` creditLineItemTypes) . orderLineItemType . entityVal
    -- | Caclulate the total price + tax for a Product.
    finalProductPrice :: Entity OrderProduct -> Cents
    finalProductPrice (Entity _ p) =
        (fromIntegral (orderProductQuantity p) * orderProductPrice p) + orderProductTax p
    -- | Convert Cents to an Integer so it can handle negative numbers.
    centsInt :: Cents -> Integer
    centsInt (Cents c) =
        toInteger c
    -- | Build the ListOrder from the queried Order data.
    convertOrder :: (Entity Order, Maybe Customer, Maybe Address, Integer) -> ListOrder
    convertOrder (Entity orderId order, mCustomer, mShipping, orderTotal) =
        let name =
                maybe "" (\shipping -> addressFirstName shipping <> " " <> addressLastName shipping)
                    mShipping
        in  ListOrder
            { loId = orderId
            , loDate = orderCreatedAt order
            , loCustomerName = name
            , loCustomerEmail = maybe "" customerEmail mCustomer
            , loShippingStreet = maybe "" addressAddressOne mShipping
            , loShippingRegion = addressState <$> mShipping
            , loOrderStatus = orderStatus order
            , loOrderTotal = orderTotal
            }
