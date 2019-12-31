{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Customers
    ( CustomerAPI
    , customerRoutes
    ) where

import Control.Monad (unless)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Database.Persist
    ( (=.), (==.), Entity(..), Filter, Update, count, selectFirst, get, update
    )
import Servant
    ( (:<|>)(..), (:>), AuthProtect, QueryParam, Capture, ReqBody, Get, Patch
    , JSON, err404
    )

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models (CustomerId, Customer(..), Address(..), EntityField(..), Unique(..))
import Models.Fields (Cents)
import Routes.Utils (extractRowCount, buildWhereQuery, hashPassword, generateUniqueToken, mapUpdate)
import Server (App, AppSQL, runDB, serverError)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


type CustomerAPI =
         "list" :> CustomerListRoute
    :<|> "edit" :> CustomerEditDataRoute
    :<|> "edit" :> CustomerEditRoute

type CustomerRoutes =
         (WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied CustomerListData))
    :<|> (WrappedAuthToken -> CustomerId -> App (Cookied CustomerEditData))
    :<|> (WrappedAuthToken -> CustomerEditParameters -> App (Cookied CustomerId))

customerRoutes :: CustomerRoutes
customerRoutes =
         customerListRoute
    :<|> customerEditDataRoute
    :<|> customerEditRoute


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


-- EDIT


type CustomerEditDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" CustomerId
    :> Get '[JSON] (Cookied CustomerEditData)

data CustomerEditData =
    CustomerEditData
        { cedId :: CustomerId
        , cedEmail :: T.Text
        , cedStoreCredit :: Cents
        , cedIsAdmin :: Bool
        } deriving (Show)

instance ToJSON CustomerEditData where
    toJSON CustomerEditData {..} =
        object
            [ "id" .= cedId
            , "email" .= cedEmail
            , "storeCredit" .= cedStoreCredit
            , "isAdmin" .= cedIsAdmin
            ]

customerEditDataRoute :: WrappedAuthToken -> CustomerId -> App (Cookied CustomerEditData)
customerEditDataRoute t customerId = withAdminCookie t $ \_ ->
    runDB (get customerId) >>= \case
        Nothing ->
            serverError err404
        Just customer ->
            return CustomerEditData
                { cedId = customerId
                , cedEmail = customerEmail customer
                , cedStoreCredit = customerStoreCredit customer
                , cedIsAdmin = customerIsAdmin customer
                }


type CustomerEditRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerEditParameters
    :> Patch '[JSON] (Cookied CustomerId)

data CustomerEditParameters =
    CustomerEditParameters
        { cepId :: CustomerId
        , cepEmail :: Maybe T.Text
        , cepStoreCredit :: Maybe Cents
        , cepIsAdmin :: Maybe Bool
        , cepPassword :: Maybe T.Text
        , cepPasswordConfirm :: Maybe T.Text
        } deriving (Show)

instance FromJSON CustomerEditParameters where
    parseJSON = withObject "CustomerEditParameters" $ \v -> do
        cepId <- v .: "id"
        cepEmail <- v .: "email"
        cepStoreCredit <- v .: "storeCredit"
        cepIsAdmin <- v .: "isAdmin"
        cepPassword <- v .: "password"
        cepPasswordConfirm <- v .: "passwordConfirm"
        return CustomerEditParameters {..}


instance Validation CustomerEditParameters where
    validators CustomerEditParameters {..} = do
        customerExists <- V.exists cepId
        uniqueEmail <- case cepEmail of
            Nothing -> return []
            Just email -> do
                -- TODO: Don't make this case insensitive?
                doesntExist <- V.uniqueCustomer email
                return
                    [ ( "email"
                      , [ V.required email
                        , ( "A Customer with this Email already exists."
                          , doesntExist
                          )
                        ]
                      )
                    ]

        return $ catMaybes
            [ V.mapCheck ((,) <$> cepPassword <*> cepPasswordConfirm) $ \(pass, confirm) ->
                ( "password"
                , [ V.required pass
                  , ( "Passwords do not match.", pass /= confirm )
                  ]
                )
            ]
            ++ uniqueEmail
            ++
            [ ( ""
              , [ ( "Could not find this Customer in the database."
                  , customerExists
                  )
                ]
              )
            ]

customerEditRoute :: WrappedAuthToken -> CustomerEditParameters -> App (Cookied CustomerId)
customerEditRoute = validateAdminAndParameters $ \_ parameters -> do
    passwordUpdate <- case (,) <$> cepPassword parameters <*> cepPasswordConfirm parameters of
        Nothing ->
            return []
        Just (password, confirm) ->
            if password == confirm then do
                passwordHash <- hashPassword password
                newToken <- runDB $ generateUniqueToken UniqueToken
                return
                    [ CustomerEncryptedPassword =. passwordHash
                    , CustomerAuthToken =. newToken
                    ]
            else
                return []
    let updates = makeUpdates parameters ++ passwordUpdate
    unless (null updates) $ runDB $ update (cepId parameters) updates
    return $ cepId parameters
  where
    makeUpdates :: CustomerEditParameters -> [Update Customer]
    makeUpdates CustomerEditParameters {..} =
        catMaybes
            [ mapUpdate CustomerEmail cepEmail
            , mapUpdate CustomerStoreCredit cepStoreCredit
            , mapUpdate CustomerIsAdmin cepIsAdmin
            ]
