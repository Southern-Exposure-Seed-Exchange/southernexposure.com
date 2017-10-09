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

import Control.Arrow (first)
import Control.Monad ((>=>), (<=<))
import Control.Monad.Catch (throwM, Exception, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), Entity(..), insert, insert_, insertEntity, getBy, selectList, deleteBy
    , deleteWhere
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
         "customer-place-order" :> CustomerPlaceOrderRoute
    :<|> "success" :> SuccessRoute

type CheckoutRoutes =
         (AuthToken -> CustomerPlaceOrderParameters -> App PlaceOrderData)
    :<|> (AuthToken -> SuccessParameters -> App OrderDetails)

checkoutRoutes :: CheckoutRoutes
checkoutRoutes =
         customerPlaceOrderRoute
    :<|> customerSuccessRoute


-- COMMON DATA


data CheckoutAddress =
    CheckoutAddress
        { caFirstName :: T.Text
        , caLastName :: T.Text
        , caCompanyName :: T.Text
        , caAddressOne :: T.Text
        , caAddressTwo :: T.Text
        , caCity :: T.Text
        , caState :: Region
        , caZipCode :: T.Text
        , caCountry :: Country
        }

instance FromJSON CheckoutAddress where
    parseJSON = withObject "CheckoutAddress" $ \v ->
        CheckoutAddress
            <$> v .: "firstName"
            <*> v .: "lastName"
            <*> v .: "companyName"
            <*> v .: "addressOne"
            <*> v .: "addressTwo"
            <*> v .: "city"
            <*> v .: "state"
            <*> v .: "zipCode"
            <*> v .: "country"

instance ToJSON CheckoutAddress where
    toJSON address =
        object
            [ "firstName" .= caFirstName address
            , "lastName" .= caLastName address
            , "companyName" .= caCompanyName address
            , "addressOne" .= caAddressOne address
            , "addressTwo" .= caAddressTwo address
            , "city" .= caCity address
            , "state" .= caState address
            , "zipCode" .= caZipCode address
            , "country" .= caCountry address
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
        , addressType = type_
        , addressCustomerId = customerId
        , addressIsActive = True
        , addressIsDefault = False
        }

toCheckoutAddress :: Address -> CheckoutAddress
toCheckoutAddress address =
    CheckoutAddress
        { caFirstName = addressFirstName address
        , caLastName = addressLastName address
        , caCompanyName = addressCompanyName address
        , caAddressOne = addressAddressOne address
        , caAddressTwo = addressAddressTwo address
        , caCity = addressCity address
        , caState = addressState address
        , caZipCode = addressZipCode address
        , caCountry = addressCountry address
        }


-- PLACE ORDER


data CustomerPlaceOrderParameters =
    CustomerPlaceOrderParameters
        { cpopShippingAddress :: CheckoutAddress
        , cpopBillingAddress :: CheckoutAddress
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
        shippingValidators <- validators $ cpopShippingAddress parameters
        billingValidators <- validators $ cpopBillingAddress parameters
        return $
            map (first $ T.append "shipping-") shippingValidators
                ++ map (first $ T.append "billing-") billingValidators

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
    let shippingAddress = fromCheckoutAddress Shipping customerId
            $ cpopShippingAddress parameters
        billingAddress = fromCheckoutAddress Billing customerId
            $ cpopBillingAddress parameters
        comment = cpopComment parameters
    currentTime <- liftIO getCurrentTime
    eitherM handleError <=< try . runDB $ do
        maybeTaxRate <- getTaxRate (Just $ addressCountry shippingAddress)
            (Just $ addressState shippingAddress)
        (cartId, items) <- getBy (UniqueCustomerCart $ Just customerId)
            >>= maybe (throwM CartNotFound)
                    (\(Entity cartId _) ->
                        fmap (cartId, ) . getCartItems maybeTaxRate
                            $ \c -> c E.^. CartId E.==. E.val cartId
                    )
        shippingId <- insert shippingAddress
        billingId <- insert billingAddress
        orderId <- insert Order
            { orderCustomerId = customerId
            , orderStatus = Processing
            , orderBillingAddressId = billingId
            , orderShippingAddressId = shippingId
            , orderCustomerComment = comment
            , orderTaxDescription = maybe "" taxRateDescription maybeTaxRate
            , orderCreatedAt = currentTime
            }
        createLineItems maybeTaxRate shippingAddress items orderId
            >> createProducts maybeTaxRate items orderId
            >> deleteWhere [CartItemCartId ==. cartId]
            >> deleteBy (UniqueCustomerCart $ Just customerId)
        return $ PlaceOrderData orderId
    where handleError = \case
            CartNotFound ->
                serverError err404
            NoShippingMethod ->
                V.singleError "Sorry, we only ship to North America."

data PlaceOrderError
    = CartNotFound
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
        (Entity orderId order, Entity _ shipping, Entity _ billing) <-
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
