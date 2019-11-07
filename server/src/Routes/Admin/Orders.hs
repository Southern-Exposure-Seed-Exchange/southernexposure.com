{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Orders
    ( OrderAPI
    , orderRoutes
    ) where

import Control.Monad (forM)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Database.Persist ((==.), Entity(..), Filter, SelectOpt(..), count, get, selectList)
import Servant ((:>), AuthProtect, QueryParam, Get, JSON)
import Text.Read (readMaybe)

import Auth (withAdminCookie, WrappedAuthToken, Cookied)
import Models (OrderId, Order(..), OrderProduct(..), OrderLineItem(..), Customer(customerEmail), Address(..), EntityField(..))
import Models.Fields (OrderStatus, Region, Cents(..), creditLineItemTypes)
import Server (App, AppSQL, runDB)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.Esqueleto as E


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
orderListRoute token maybePage maybePerPage maybeQuery = withAdminCookie token $ \_ -> do
    let perPage = fromMaybe 50 maybePerPage
        page = fromMaybe 1 maybePage
        offset = perPage * (page - 1)
        queryParts = splitQuery maybeQuery
    (orders, orderCount) <- runDB $ do
        orderCount <-
            if null queryParts then
                count ([] :: [Filter Order])
            else
                extractRowCount . E.select $ E.from $ \(o `E.LeftOuterJoin` c `E.LeftOuterJoin` sa) -> do
                    E.on $ E.just (o E.^. OrderShippingAddressId) E.==. sa E.?. AddressId
                    E.on $ E.just (o E.^. OrderCustomerId) E.==. c E.?. CustomerId
                    E.where_ $ makeQuery o c sa queryParts
                    return E.countRows
        orders <- do
            orders <-
                if null queryParts then
                    map (,Nothing, Nothing)
                        <$> selectList []
                            [ Desc OrderCreatedAt
                            , LimitTo perPage
                            , OffsetBy $ fromIntegral offset
                            ]
                else
                    E.select $ E.from $ \(o `E.LeftOuterJoin` c `E.LeftOuterJoin` sa) -> do
                        E.on $ E.just (o E.^. OrderShippingAddressId) E.==. sa E.?. AddressId
                        E.on $ E.just (o E.^. OrderCustomerId) E.==. c E.?. CustomerId
                        E.limit $ fromIntegral perPage
                        E.offset $ fromIntegral offset
                        E.orderBy [E.desc $ o E.^. OrderCreatedAt]
                        E.where_ $ makeQuery o c sa queryParts
                        return (o, c, sa)
            forM orders $ \(o@(Entity orderId order), c, sa) -> do
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
                customer <- getIfNothing (orderCustomerId order) c
                shipAddr <- getIfNothing (orderShippingAddressId order) sa
                return (o, customer, shipAddr, total)
        return (orders, orderCount)
    return OrderListData
        { oldOrders = map convertOrder orders
        , oldTotalOrders = orderCount
        }
  where
    -- | Split the query into discrete tokens.
    splitQuery :: Maybe T.Text -> [T.Text]
    splitQuery =
        maybe [] T.words
    -- | Build the where query by folding over the query tokens.
    makeQuery
        :: E.SqlExpr (Entity Order)
        -> E.SqlExpr (Maybe (Entity Customer))
        -> E.SqlExpr (Maybe (Entity Address))
        -> [T.Text]
        -> E.SqlExpr (E.Value Bool)
    makeQuery o c sa =
        foldr (\qToken expr -> expr E.&&. makeQueryPart qToken) (E.val True)
      where
        -- | Build part of the where query for a single token of the search query.
        makeQueryPart :: T.Text -> E.SqlExpr (E.Value Bool)
        makeQueryPart query =
            let wildQuery = E.just $ E.concat_ [(E.%), E.val query, (E.%)]
                idQuery = case readMaybe (T.unpack query) of
                    Nothing -> E.val False
                    Just num -> o E.^. OrderId E.==. E.valkey num
            in foldr (E.||.) idQuery
                [ c E.?. CustomerEmail `E.ilike` wildQuery
                , sa E.?. AddressFirstName `E.ilike` wildQuery
                , sa E.?. AddressLastName `E.ilike` wildQuery
                , sa E.?. AddressAddressOne `E.ilike` wildQuery
                , sa E.?. AddressZipCode `E.ilike` wildQuery
                ]
    -- | Extract a row count by defaulting to 0 if no rows are returned.
    extractRowCount :: AppSQL [E.Value Int] -> AppSQL Int
    extractRowCount = fmap $ maybe 0 E.unValue . listToMaybe
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
    -- | Try fetching a row if we haven't yet.
    getIfNothing :: (E.PersistEntityBackend a ~ E.SqlBackend, E.PersistEntity a)
        => E.Key a -> Maybe (Entity a) -> AppSQL (Maybe a)
    getIfNothing key =
        maybe (get key) (return . Just . entityVal)
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
