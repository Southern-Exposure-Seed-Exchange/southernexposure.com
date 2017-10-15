{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Checkout
    ( CheckoutAPI
    , checkoutRoutes
    ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad ((>=>), (<=<), when)
import Control.Monad.Catch (throwM, Exception, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.List (partition, find)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), (=.), Entity(..), insert, insert_, insertEntity, get, getBy
    , selectList, update, updateWhere, deleteBy, deleteWhere
    )
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, JSON, Post, ReqBody, err404)

import Auth
import Models
import Models.Fields
import Server
import Routes.CommonData (CartItemData(..), CartCharges(..), CartCharge(..), getCartItems, getCharges)
import Validation (Validation(..))

import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


type CheckoutAPI =
         "customer-details" :> CustomerDetailsRoute
    :<|> "customer-place-order" :> CustomerPlaceOrderRoute
    :<|> "anonymous-details" :> AnonymousDetailsRoute
    :<|> "success" :> SuccessRoute

type CheckoutRoutes =
         (AuthToken -> CustomerDetailsParameters -> App CheckoutDetailsData)
    :<|> (AuthToken -> CustomerPlaceOrderParameters -> App PlaceOrderData)
    :<|> (AnonymousDetailsParameters -> App CheckoutDetailsData)
    :<|> (AuthToken -> SuccessParameters -> App OrderDetails)

checkoutRoutes :: CheckoutRoutes
checkoutRoutes =
         customerDetailsRoute
    :<|> customerPlaceOrderRoute
    :<|> anonymousDetailsRoute
    :<|> customerSuccessRoute


-- COMMON DATA


data CheckoutAddress =
    CheckoutAddress
        { caId :: Maybe AddressId
        , caFirstName :: T.Text
        , caLastName :: T.Text
        , caCompanyName :: T.Text
        , caAddressOne :: T.Text
        , caAddressTwo :: T.Text
        , caCity :: T.Text
        , caState :: Region
        , caZipCode :: T.Text
        , caCountry :: Country
        , caIsDefault :: Bool
        }

instance FromJSON CheckoutAddress where
    parseJSON = withObject "CheckoutAddress" $ \v ->
        CheckoutAddress
            <$> v .:? "id"
            <*> v .: "firstName"
            <*> v .: "lastName"
            <*> v .: "companyName"
            <*> v .: "addressOne"
            <*> v .: "addressTwo"
            <*> v .: "city"
            <*> v .: "state"
            <*> v .: "zipCode"
            <*> v .: "country"
            <*> v .: "isDefault"

instance ToJSON CheckoutAddress where
    toJSON address =
        object
            [ "id" .= caId address
            , "firstName" .= caFirstName address
            , "lastName" .= caLastName address
            , "companyName" .= caCompanyName address
            , "addressOne" .= caAddressOne address
            , "addressTwo" .= caAddressTwo address
            , "city" .= caCity address
            , "state" .= caState address
            , "zipCode" .= caZipCode address
            , "country" .= caCountry address
            , "isDefault" .= caIsDefault address
            ]

instance Validation CheckoutAddress where
    validators address =
        let
            invalidRegionForCountry =
                case (fromCountry (caCountry address), caState address) of
                    (CountryCodes.US, USState _) ->
                        False
                    (CountryCodes.US, USArmedForces _) ->
                        False
                    (CountryCodes.CA, CAProvince _) ->
                        False
                    (_, CustomRegion _) ->
                        False
                    _ ->
                        True
        in
            return
                [ ( "firstName", [ V.required $ caFirstName address ])
                , ( "lastName", [ V.required $ caLastName address ])
                , ( "addressOne", [ V.required $ caAddressOne address ])
                , ( "city", [ V.required $ caCity address ])
                , ( "state"
                  , [ ("Invalid region for the selected Country.", invalidRegionForCountry) ]
                  )
                , ( "zipCode", [ V.required $ caZipCode address ])
                ]

fromCheckoutAddress :: AddressType -> CustomerId -> CheckoutAddress -> Address
fromCheckoutAddress type_ customerId address =
    Address
        { addressFirstName = caFirstName address
        , addressLastName = caLastName address
        , addressCompanyName = caCompanyName address
        , addressAddressOne = caAddressOne address
        , addressAddressTwo = caAddressTwo address
        , addressCity = caCity address
        , addressState = caState address
        , addressZipCode = caZipCode address
        , addressCountry = caCountry address
        , addressIsDefault = caIsDefault address
        , addressType = type_
        , addressCustomerId = customerId
        , addressIsActive = True
        }

toCheckoutAddress :: Entity Address -> CheckoutAddress
toCheckoutAddress (Entity addressId address) =
    CheckoutAddress
        { caId = Just addressId
        , caFirstName = addressFirstName address
        , caLastName = addressLastName address
        , caCompanyName = addressCompanyName address
        , caAddressOne = addressAddressOne address
        , caAddressTwo = addressAddressTwo address
        , caCity = addressCity address
        , caState = addressState address
        , caZipCode = addressZipCode address
        , caCountry = addressCountry address
        , caIsDefault = addressIsDefault address
        }


-- CART/ADDRESS DETAILS


data CustomerDetailsParameters =
    CustomerDetailsParameters
        { cdpShippingRegion :: Maybe Region
        , cdpShippingCountry :: Maybe Country
        , cdpExistingAddress :: Maybe AddressId
        }

instance FromJSON CustomerDetailsParameters where
    parseJSON = withObject "CustomerDetailsParameters" $ \v ->
        CustomerDetailsParameters
            <$> v .:? "region"
            <*> v .:? "country"
            <*> v .:? "addressId"

data CheckoutDetailsData =
    CheckoutDetailsData
        { cddShippingAddresses :: [CheckoutAddress]
        , cddBillingAddresses :: [CheckoutAddress]
        , cddItems :: [CartItemData]
        , cddCharges :: CartCharges
        }

instance ToJSON CheckoutDetailsData where
    toJSON details =
        object
            [ "shippingAddresses" .= cddShippingAddresses details
            , "billingAddresses" .= cddBillingAddresses details
            , "items" .= cddItems details
            , "charges" .= cddCharges details
            ]

type CustomerDetailsRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

customerDetailsRoute :: AuthToken -> CustomerDetailsParameters -> App CheckoutDetailsData
customerDetailsRoute token parameters = do
    (Entity customerId _) <- validateToken token
    runDB $ do
        customerAddresses <- selectList
            [AddressCustomerId ==. customerId, AddressIsActive ==. True] []
        let (shippingAddresses, billingAddresses) =
                partition (\a -> addressType (entityVal a) == Shipping)
                    customerAddresses
            (maybeCountry, maybeRegion) =
                getShippingArea parameters shippingAddresses
        maybeTaxRate <- getTaxRate maybeCountry maybeRegion
        items <- getCartItems maybeTaxRate $ \c ->
            c E.^. CartCustomerId E.==. E.just (E.val customerId)
        charges <- getCharges maybeTaxRate maybeCountry items
        return CheckoutDetailsData
            { cddShippingAddresses = map toCheckoutAddress shippingAddresses
            , cddBillingAddresses = map toCheckoutAddress billingAddresses
            , cddItems = items
            , cddCharges = charges
            }
    where getShippingArea ps addrs =
            let
                maybeFromAddress =
                    fmap fromAddress
                    $ (\a -> find (\(Entity addrId _) -> addrId == a) addrs)
                    =<< cdpExistingAddress ps
                maybeFromParams =
                    case (cdpShippingCountry ps, cdpShippingRegion ps) of
                        (Nothing, Nothing) ->
                            Nothing
                        (c, r) ->
                            Just (c, r)
                maybeFromDefault =
                    fromAddress <$> find (addressIsDefault . entityVal) addrs
                fromAddress (Entity _ addr) =
                    (Just $ addressCountry addr, Just $ addressState addr)
            in
                fromMaybe (Nothing, Nothing)
                    $ asum [maybeFromAddress, maybeFromParams, maybeFromDefault]


data AnonymousDetailsParameters =
    AnonymousDetailsParameters
        { adpShippingCountry :: Maybe Country
        , adpShippingRegion :: Maybe Region
        , adpCartToken :: T.Text
        }

instance FromJSON AnonymousDetailsParameters where
    parseJSON = withObject "AnonymousDetailsParameters" $ \v ->
        AnonymousDetailsParameters
            <$> v .:? "country"
            <*> v .:? "region"
            <*> v .: "sessionToken"

type AnonymousDetailsRoute =
       ReqBody '[JSON] AnonymousDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

anonymousDetailsRoute :: AnonymousDetailsParameters -> App CheckoutDetailsData
anonymousDetailsRoute parameters =
    let
        maybeCountry = adpShippingCountry parameters
    in
        runDB $ do
            maybeTaxRate <- getTaxRate maybeCountry $ adpShippingRegion parameters
            items <- getCartItems maybeTaxRate $ \c ->
                c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
            charges <- getCharges maybeTaxRate maybeCountry items
            return CheckoutDetailsData
                { cddShippingAddresses = []
                , cddBillingAddresses = []
                , cddItems = items
                , cddCharges = charges
                }


-- PLACE ORDER


data CustomerAddress
    = NewAddress CheckoutAddress
    | ExistingAddress AddressId Bool

instance FromJSON CustomerAddress where
    parseJSON v =
            parseExisting v
        <|> parseNew v
        where parseExisting =
                withObject "ExistingAddress" $ \w ->
                    ExistingAddress
                        <$> w .: "id"
                        <*> w .: "makeDefault"
              parseNew =
                fmap NewAddress . parseJSON


data CustomerPlaceOrderParameters =
    CustomerPlaceOrderParameters
        { cpopShippingAddress :: CustomerAddress
        , cpopBillingAddress :: CustomerAddress
        , cpopComment :: T.Text
        }

instance FromJSON CustomerPlaceOrderParameters where
    parseJSON = withObject "CustomerPlaceOrderParameters" $ \v ->
        CustomerPlaceOrderParameters
            <$> v .: "shippingAddress"
            <*> v .: "billingAddress"
            <*> v .: "comment"

instance Validation CustomerPlaceOrderParameters where
    validators parameters = do
        shippingValidators <- validateAddress $ cpopShippingAddress parameters
        billingValidators <- validateAddress $ cpopBillingAddress parameters
        return $
            map (first $ T.append "shipping-") shippingValidators
                ++ map (first $ T.append "billing-") billingValidators
        where validateAddress a =
                case a of
                    NewAddress f ->
                        validators f
                    ExistingAddress addrId _ -> do
                        invalidAddress <- V.exists addrId
                        return
                            [ ( ""
                              , [ ( "Please re-select your address or try adding a new one."
                                  , invalidAddress
                                  )
                                ]
                              )
                            ]

newtype PlaceOrderData =
    PlaceOrderData
        { podOrderId :: OrderId
        }

instance ToJSON PlaceOrderData where
    toJSON order =
        object [ "orderId" .= podOrderId order ]

type CustomerPlaceOrderRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerPlaceOrderParameters
    :> Post '[JSON] PlaceOrderData


customerPlaceOrderRoute :: AuthToken -> CustomerPlaceOrderParameters -> App PlaceOrderData
customerPlaceOrderRoute token = validate >=> \parameters -> do
    (Entity customerId _) <- validateToken token
    let comment = cpopComment parameters
    currentTime <- liftIO getCurrentTime
    eitherM handleError <=< try . runDB $ do
        shippingAddress <- getOrInsertAddress Shipping customerId
            $ cpopShippingAddress parameters
        billingAddress <- getOrInsertAddress Billing customerId
            $ cpopBillingAddress parameters
        maybeTaxRate <- getTaxRate (Just . addressCountry $ entityVal shippingAddress)
            (Just . addressState $ entityVal shippingAddress)
        (cartId, items) <- getBy (UniqueCustomerCart $ Just customerId) >>=
            maybe (throwM CartNotFound)
                (\(Entity cartId _) ->
                    fmap (cartId, ) . getCartItems maybeTaxRate
                        $ \c -> c E.^. CartId E.==. E.val cartId
                )
        orderId <- insert Order
            { orderCustomerId = customerId
            , orderStatus = Processing
            , orderBillingAddressId = entityKey billingAddress
            , orderShippingAddressId = entityKey shippingAddress
            , orderCustomerComment = comment
            , orderTaxDescription = maybe "" taxRateDescription maybeTaxRate
            , orderCreatedAt = currentTime
            }
        createLineItems maybeTaxRate (entityVal shippingAddress) items orderId
            >> createProducts maybeTaxRate items orderId
            >> deleteWhere [CartItemCartId ==. cartId]
            >> deleteBy (UniqueCustomerCart $ Just customerId)
        return $ PlaceOrderData orderId
    where handleError = \case
            CartNotFound ->
                serverError err404
            NoShippingMethod ->
                V.singleError "Sorry, we only ship to North America."
            AddressNotFound Shipping ->
                V.singleFieldError "shipping-" "Please choose again or try adding a new address."
            AddressNotFound Billing ->
                V.singleFieldError "billing-" "Please choose again or try adding a new address."
          getOrInsertAddress :: AddressType -> CustomerId -> CustomerAddress -> AppSQL (Entity Address)
          getOrInsertAddress addrType customerId = \case
            ExistingAddress addrId makeDefault -> do
                let noAddress = throwM $ AddressNotFound addrType
                address <- get addrId >>= maybe noAddress return
                when (addressCustomerId address /= customerId) noAddress
                when makeDefault $
                    updateWhere [AddressCustomerId ==. customerId, AddressType ==. addrType]
                        [AddressIsDefault =. False]
                    >> update addrId [AddressIsDefault =. True]
                return $ Entity addrId address
            NewAddress ca ->
                let
                    newAddress = fromCheckoutAddress addrType customerId ca
                in do
                    when (addressIsDefault newAddress)
                        $ updateWhere
                            [ AddressType ==. addrType
                            , AddressCustomerId ==. customerId ]
                            [ AddressIsDefault =. False ]
                    addrId <- insert newAddress
                    return $ Entity addrId newAddress

data PlaceOrderError
    = CartNotFound
    | AddressNotFound AddressType
    | NoShippingMethod
    deriving (Show, Typeable)

instance Exception PlaceOrderError


createLineItems :: Maybe TaxRate -> Address -> [CartItemData] -> OrderId -> AppSQL ()
createLineItems maybeTaxRate shippingAddress items orderId = do
    charges <- getCharges maybeTaxRate
        (Just $ addressCountry shippingAddress) items
    shippingCharge <-
        case ccShippingMethods charges of
            [] ->
                throwM NoShippingMethod
            charge : _ ->
                return charge
    insert_ $ makeLine ShippingLine shippingCharge
    mapM_ (insertEntity . makeLine SurchargeLine) $ ccSurcharges charges
    where makeLine lineType charge =
            OrderLineItem
                { orderLineItemOrderId = orderId
                , orderLineItemType = lineType
                , orderLineItemDescription = ccDescription charge
                , orderLineItemAmount = ccAmount charge
                }

createProducts :: Maybe TaxRate -> [CartItemData] -> OrderId -> AppSQL [Entity OrderProduct]
createProducts maybeTaxRate items orderId =
    mapM (insertEntity . makeProduct) items
    where makeProduct CartItemData { cidVariant, cidQuantity } =
            let
                (Entity variantId variant) = cidVariant
                price = productVariantPrice variant
                productTotal = Cents $ fromCents price * cidQuantity
            in
                OrderProduct
                    { orderProductOrderId = orderId
                    , orderProductProductVariantId = variantId
                    , orderProductQuantity = cidQuantity
                    , orderProductPrice = productVariantPrice variant
                    , orderProductTax =
                        applyTax productTotal (productVariantProductId variant)
                    }
          applyTax productTotal productId =
              maybe (Cents 0) (applyTaxRate productTotal productId) maybeTaxRate


-- SUCCESS DETAILS


data OrderDetails =
    OrderDetails
        { odOrder :: CheckoutOrder
        , odLineItems :: [Entity OrderLineItem]
        , odProducts :: [CheckoutProduct]
        , odShippingAddress :: CheckoutAddress
        , odBillingAddress :: CheckoutAddress
        }

instance ToJSON OrderDetails where
    toJSON details =
        object
            [ "order" .= odOrder details
            , "lineItems" .= odLineItems details
            , "products" .= odProducts details
            , "shippingAddress" .= odShippingAddress details
            , "billingAddress" .= odBillingAddress details
            ]

data CheckoutOrder =
    CheckoutOrder
        { coStatus :: OrderStatus
        , coComment :: T.Text
        , coTaxDescription :: T.Text
        , coCreatedAt :: UTCTime
        }

instance ToJSON CheckoutOrder where
    toJSON order =
        object
            [ "status" .= coStatus order
            , "comment" .= coComment order
            , "taxDescription" .= coTaxDescription order
            , "createdAt" .= coCreatedAt order
            ]

toCheckoutOrder :: Order -> CheckoutOrder
toCheckoutOrder order =
    CheckoutOrder
        { coStatus = orderStatus order
        , coComment = orderCustomerComment order
        , coTaxDescription = orderTaxDescription order
        , coCreatedAt = orderCreatedAt order
        }

data CheckoutProduct =
    CheckoutProduct
        { cpName :: T.Text
        , cpWeight :: Milligrams
        , cpQuantity :: Natural
        , cpPrice :: Cents
        , cpTax :: Cents
        }

instance ToJSON CheckoutProduct where
    toJSON prod =
        object
            [ "name" .= cpName prod
            , "weight" .= cpWeight prod
            , "quantity" .= cpQuantity prod
            , "price" .= cpPrice prod
            , "tax" .= cpTax prod
            ]

newtype SuccessParameters =
    SuccessParameters
        { cspOrderId :: Int64 }

instance FromJSON SuccessParameters where
    parseJSON = withObject "SuccessParameters" $ \v ->
        SuccessParameters
            <$> v .: "orderId"

type SuccessRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] SuccessParameters
    :> Post '[JSON] OrderDetails

customerSuccessRoute :: AuthToken -> SuccessParameters -> App OrderDetails
customerSuccessRoute token parameters = do
    (Entity customerId _) <- validateToken token
    eitherM handleError <=< try . runDB $ do
        (Entity orderId order, shipping, billing) <-
            getOrderAndAddress customerId (E.toSqlKey $ cspOrderId parameters)
                >>= maybe (throwM OrderNotFound) return
        lineItems <-
            selectList [OrderLineItemOrderId ==. orderId] []
        products <- getCheckoutProducts orderId
        return OrderDetails
            { odOrder = toCheckoutOrder order
            , odLineItems = lineItems
            , odProducts = products
            , odShippingAddress = toCheckoutAddress shipping
            , odBillingAddress = toCheckoutAddress billing
            }
    where handleError = \case
            OrderNotFound ->
                serverError err404

data SuccessError
    = OrderNotFound
    deriving (Show, Typeable)

instance Exception SuccessError


getOrderAndAddress :: CustomerId -> OrderId -> AppSQL (Maybe (Entity Order, Entity Address, Entity Address))
getOrderAndAddress customerId orderId =
    fmap listToMaybe . E.select . E.from
        $ \(o `E.InnerJoin` s `E.InnerJoin` b) -> do
            E.on $ o E.^. OrderBillingAddressId E.==. b E.^. AddressId
            E.on $ o E.^. OrderShippingAddressId E.==. s E.^. AddressId
            E.where_ $
                o E.^. OrderId E.==. E.val orderId E.&&.
                o E.^. OrderCustomerId E.==. E.val customerId
            return (o, s, b)

getCheckoutProducts :: OrderId -> AppSQL [CheckoutProduct]
getCheckoutProducts orderId = do
    orderProducts <- E.select $ E.from $
        \(op `E.InnerJoin` v `E.InnerJoin` p) -> do
            E.on $ p E.^. ProductId E.==. v E.^. ProductVariantProductId
            E.on $ v E.^. ProductVariantId E.==. op E.^. OrderProductProductVariantId
            E.where_ $ op E.^. OrderProductOrderId E.==. E.val orderId
            return (op, v E.^. ProductVariantWeight, p E.^. ProductName)
    return $ map makeCheckoutProduct orderProducts
    where makeCheckoutProduct (Entity _ orderProd, variantWeight, productName) =
            CheckoutProduct
                { cpName = E.unValue productName
                , cpWeight = E.unValue variantWeight
                , cpQuantity = orderProductQuantity orderProd
                , cpPrice = orderProductPrice orderProd
                , cpTax = orderProductTax orderProd
                }


-- Utils


eitherM :: Monad m => (b -> m a) -> Either b a -> m a
eitherM handler =
    either handler return
