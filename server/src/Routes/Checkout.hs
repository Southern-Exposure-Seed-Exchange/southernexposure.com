{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , selectList, update, updateWhere, delete, deleteWhere
    )
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, JSON, Post, ReqBody, err404)

import Auth
import Models
import Models.Fields
import Server
import Routes.CommonData
    ( AuthorizationData, toAuthorizationData, CartItemData(..), CartCharges(..)
    , CartCharge(..), getCartItems, getCharges, AddressData(..), toAddressData
    , fromAddressData
    )
import Routes.Utils (hashPassword, generateUniqueToken)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


type CheckoutAPI =
         "customer-details" :> CustomerDetailsRoute
    :<|> "customer-place-order" :> CustomerPlaceOrderRoute
    :<|> "anonymous-details" :> AnonymousDetailsRoute
    :<|> "anonymous-place-order" :> AnonymousPlaceOrderRoute
    :<|> "success" :> SuccessRoute

type CheckoutRoutes =
         (AuthToken -> CustomerDetailsParameters -> App CheckoutDetailsData)
    :<|> (AuthToken -> CustomerPlaceOrderParameters -> App PlaceOrderData)
    :<|> (AnonymousDetailsParameters -> App CheckoutDetailsData)
    :<|> (AnonymousPlaceOrderParameters -> App AnonymousPlaceOrderData)
    :<|> (AuthToken -> SuccessParameters -> App OrderDetails)

checkoutRoutes :: CheckoutRoutes
checkoutRoutes =
         customerDetailsRoute
    :<|> customerPlaceOrderRoute
    :<|> anonymousDetailsRoute
    :<|> anonymousPlaceOrderRoute
    :<|> customerSuccessRoute


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
        { cddShippingAddresses :: [AddressData]
        , cddBillingAddresses :: [AddressData]
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
            { cddShippingAddresses = map toAddressData shippingAddresses
            , cddBillingAddresses = map toAddressData billingAddresses
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


data PlaceOrderError
    = CartNotFound
    | AddressNotFound AddressType
    | NoShippingMethod
    deriving (Show, Typeable)

instance Exception PlaceOrderError

withPlaceOrderErrors :: CustomerAddress -> App a -> App a
withPlaceOrderErrors shippingAddress =
    eitherM handle <=< try
    where handle = \case
            CartNotFound ->
                serverError err404
            NoShippingMethod ->
                case shippingAddress of
                    ExistingAddress _ _ ->
                        V.singleFieldError "shipping-" "Sorry, we only ship to North America."
                    NewAddress _ ->
                        V.singleFieldError "shipping-country" "Sorry, we only ship to North America."
            AddressNotFound Shipping ->
                V.singleFieldError "shipping-"
                    "Please choose again or try adding a new address."
            AddressNotFound Billing ->
                V.singleFieldError "billing-"
                    "Please choose again or try adding a new address."


data CustomerAddress
    = NewAddress AddressData
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
    currentTime <- liftIO getCurrentTime
    withPlaceOrderErrors (cpopShippingAddress parameters) . runDB $ do
        shippingAddress <- getOrInsertAddress Shipping customerId
            $ cpopShippingAddress parameters
        billingAddress <- getOrInsertAddress Billing customerId
            $ cpopBillingAddress parameters
        (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId) >>=
            maybe (throwM CartNotFound) return
        orderId <- createOrder customerId cartId shippingAddress
            (entityKey billingAddress) (cpopComment parameters) currentTime
        return $ PlaceOrderData orderId
    where getOrInsertAddress :: AddressType -> CustomerId -> CustomerAddress -> AppSQL (Entity Address)
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
                    newAddress = fromAddressData addrType customerId ca
                in do
                    when (addressIsDefault newAddress)
                        $ updateWhere
                            [ AddressType ==. addrType
                            , AddressCustomerId ==. customerId ]
                            [ AddressIsDefault =. False ]
                    addrId <- insert newAddress
                    return $ Entity addrId newAddress


data AnonymousPlaceOrderParameters =
    AnonymousPlaceOrderParameters
        { apopEmail :: T.Text
        , apopPassword :: T.Text
        , apopShippingAddress :: AddressData
        , apopBillingAddress :: AddressData
        , apopComment :: T.Text
        , apopCartToken :: T.Text
        }

instance FromJSON AnonymousPlaceOrderParameters where
    parseJSON = withObject "AnonymousPlaceOrderParameters" $ \v ->
        AnonymousPlaceOrderParameters
            <$> v .: "email"
            <*> v .: "password"
            <*> v .: "shippingAddress"
            <*> v .: "billingAddress"
            <*> v .: "comment"
            <*> v .: "sessionToken"

instance Validation AnonymousPlaceOrderParameters where
    validators parameters = do
        emailDoesntExist <- V.doesntExist . UniqueEmail $ apopEmail parameters
        shippingValidators <- validators $ apopShippingAddress parameters
        billingValidators <- validators $ apopBillingAddress parameters
        return $
            [ ( "email"
              , [ V.required $ apopEmail parameters
                , ( "An Account with this Email already exists."
                  , emailDoesntExist
                  )
                ]
              )
            , ( "password"
              , [ V.required $ apopPassword parameters
                , V.minimumLength 8 $ apopPassword parameters
                ]
              )
            ]
            ++ map (first $ T.append "shipping-") shippingValidators
            ++ map (first $ T.append "billing-") billingValidators

data AnonymousPlaceOrderData =
    AnonymousPlaceOrderData
        { apodOrderId :: OrderId
        , apodAuthorizationData :: AuthorizationData
        }

instance ToJSON AnonymousPlaceOrderData where
    toJSON orderData =
        object
            [ "orderId" .= apodOrderId orderData
            , "authData" .= apodAuthorizationData orderData
            ]

type AnonymousPlaceOrderRoute =
       ReqBody '[JSON] AnonymousPlaceOrderParameters
    :> Post '[JSON] AnonymousPlaceOrderData

anonymousPlaceOrderRoute :: AnonymousPlaceOrderParameters -> App AnonymousPlaceOrderData
anonymousPlaceOrderRoute = validate >=> \parameters -> do
    encryptedPass <- hashPassword $ apopPassword parameters
    authToken <- generateUniqueToken UniqueToken
    currentTime <- liftIO getCurrentTime
    let customer = Customer
            { customerEmail = apopEmail parameters
            , customerEncryptedPassword = encryptedPass
            , customerAuthToken = authToken
            , customerIsAdmin = False
            }
    withPlaceOrderErrors (NewAddress $ apopShippingAddress parameters) . runDB $ do
        customerId <- insert customer
        let shippingAddress = fromAddressData Shipping customerId
                $ apopShippingAddress parameters
        mergeCarts (apopCartToken parameters) customerId
        (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
            >>= maybe (throwM CartNotFound) return
        shippingId <- insert shippingAddress
        billingId <- insert . fromAddressData Billing customerId
            $ apopBillingAddress parameters
        orderId <- createOrder customerId cartId (Entity shippingId shippingAddress)
            billingId (apopComment parameters) currentTime
        return . AnonymousPlaceOrderData orderId
            . toAuthorizationData
            $ Entity customerId customer


createOrder :: CustomerId -> CartId -> Entity Address -> AddressId -> T.Text -> UTCTime -> AppSQL OrderId
createOrder customerId cartId shippingEntity billingId comment currentTime = do
    let (Entity shippingId shippingAddress) = shippingEntity
    maybeTaxRate <- getTaxRate (Just $ addressCountry shippingAddress)
        (Just $ addressState shippingAddress)
    items <- getCartItems maybeTaxRate $ \c -> c E.^. CartId E.==. E.val cartId
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
        >> delete cartId
    return orderId

createLineItems :: Maybe TaxRate -> Address -> [CartItemData] -> OrderId -> AppSQL ()
createLineItems maybeTaxRate shippingAddress items orderId = do
    charges <- getCharges maybeTaxRate (Just $ addressCountry shippingAddress) items
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
-- TODO: Move to Customers Module or make Orders Module
-- (used for My Account Order Details page as well)


data OrderDetails =
    OrderDetails
        { odOrder :: CheckoutOrder
        , odLineItems :: [Entity OrderLineItem]
        , odProducts :: [CheckoutProduct]
        , odShippingAddress :: AddressData
        , odBillingAddress :: AddressData
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
            , odShippingAddress = toAddressData shipping
            , odBillingAddress = toAddressData billing
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
