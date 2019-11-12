{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Customers
    ( CustomerAPI
    , customerRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist ((==.), Entity(..), Filter, count, selectFirst)
import Servant ((:>), AuthProtect, QueryParam, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models (CustomerId, Customer(..), Address(..), EntityField(..))
import Routes.Utils (extractRowCount, buildWhereQuery)
import Server (App, AppSQL, runDB)

import qualified Data.Text as T
import qualified Database.Esqueleto as E


type CustomerAPI =
         "list" :> CustomerListRoute

type CustomerRoutes =
         (WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied CustomerListData))

customerRoutes :: CustomerRoutes
customerRoutes =
         customerListRoute


-- LIST


type CustomerListRoute =
       AuthProtect "cookie-auth"
    :> QueryParam "page" Int
    :> QueryParam "perPage" Int
    :> QueryParam "query" T.Text
    :> Get '[JSON] (Cookied CustomerListData)

data CustomerListData =
    CustomerListData
        { cldCustomers :: [ListCustomer]
        , cldTotalCustomers :: Int
        } deriving (Show)

instance ToJSON CustomerListData where
    toJSON CustomerListData {..} =
        object
            [ "customers" .= cldCustomers
            , "total" .= cldTotalCustomers
            ]

data ListCustomer =
    ListCustomer
        { lcId :: CustomerId
        , lcEmail :: T.Text
        , lcName :: T.Text
        } deriving (Show)

instance ToJSON ListCustomer where
    toJSON ListCustomer {..} =
        object
            [ "id" .= lcId
            , "email" .= lcEmail
            , "name" .= lcName
            ]

customerListRoute :: WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied CustomerListData)
customerListRoute t mPage mPerPage maybeQuery = withAdminCookie t $ \_ -> do
    let perPage = fromMaybe 50 mPerPage
        page = fromMaybe 1 mPage
        offset = perPage * (page - 1)
        query = fromMaybe "" maybeQuery
    (customers, customerCount) <- runDB $ do
        customerCount <-
            if T.null query then
                count ([] :: [Filter Customer])
            else
                extractRowCount . E.select $ E.from $ \(c `E.LeftOuterJoin` a) -> do
                    E.on $ E.just (c E.^. CustomerId) E.==. a E.?. AddressCustomerId
                        E.&&. E.just (E.val True) E.==. a E.?. AddressIsActive
                    E.where_ $ whereQuery c a query
                    return $ E.countDistinct $ c E.^. CustomerId
        customerResult <- E.select $ E.from $ \(c `E.LeftOuterJoin` a) ->
            E.distinctOnOrderBy [E.asc $ c E.^. CustomerEmail] $ do
                let activeAddressFilter =
                        if T.null query then
                            E.just (E.val True) E.==. a E.?. AddressIsDefault
                        else
                            E.val True
                E.on $ E.just (c E.^. CustomerId) E.==. a E.?. AddressCustomerId
                    E.&&. E.just (E.val True) E.==. a E.?. AddressIsActive
                    E.&&. activeAddressFilter
                E.limit $ fromIntegral perPage
                E.offset $ fromIntegral offset
                E.where_ $ whereQuery c a query
                return (c, a)
        customers <- mapM getAddressIfMissing customerResult
        return (customers, customerCount)
    return $ CustomerListData customers customerCount
  where
    -- | Search the Email, Name, & Street Line 1.
    whereQuery
        :: E.SqlExpr (Entity Customer)
        -> E.SqlExpr (Maybe (Entity Address))
        -> T.Text
        -> E.SqlExpr (E.Value Bool)
    whereQuery c a =
        buildWhereQuery $ \term ->
            let wildQuery = E.just $ E.concat_ [(E.%), E.val term, (E.%)]
            in  [ E.just (c E.^. CustomerEmail) `E.ilike` wildQuery
                , a E.?. AddressFirstName `E.ilike` wildQuery
                , a E.?. AddressLastName `E.ilike` wildQuery
                , a E.?. AddressAddressOne `E.ilike` wildQuery
                ]
    -- | Try fetching any of the Customer's Addresses if one does not
    -- exist. Then make the 'ListCustomer'.
    getAddressIfMissing :: (Entity Customer, Maybe (Entity Address)) -> AppSQL ListCustomer
    getAddressIfMissing (c@(Entity customerId _), maybeAddr) =
        case maybeAddr of
            Just _ ->
                return $ makeCustomer c maybeAddr
            Nothing ->
                makeCustomer c <$> selectFirst
                    [ AddressCustomerId ==. customerId
                    , AddressIsActive ==. True
                    ] []
    -- | Make a 'ListCustomer', using empty Text values if an Address was
    -- not found.
    makeCustomer :: Entity Customer -> Maybe (Entity Address) -> ListCustomer
    makeCustomer (Entity customerId customer) maybeAddress =
        let
            name =
                case maybeAddress of
                    Just (Entity _ address) ->
                        addressFirstName address <> " " <> addressLastName address
                    Nothing ->
                        ""
        in ListCustomer
            { lcId = customerId
            , lcEmail = customerEmail customer
            , lcName = name
            }
