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
import Control.Monad ((>=>), (<=<), when, void)
import Control.Monad.Catch (throwM, Exception, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.List (partition, find)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), (=.), (-=.), Entity(..), insert, insert_, insertEntity, get, getBy
    , selectList, update, updateWhere, delete, deleteWhere
    )
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, JSON, Post, ReqBody, err404)
import Web.Stripe ((-&-), errorMsg)
import Web.Stripe.Card (createCustomerCardByToken, updateCustomerCard, getCustomerCards)
import Web.Stripe.Charge (createCharge)
import Web.Stripe.Customer (Email(..), createCustomer, updateCustomer)

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
import qualified Emails
import qualified Emails.OrderPlaced as OrderPlaced
import qualified Validation as V
import qualified Web.Stripe.Types as Stripe


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
        , cddStoreCredit :: Cents
        }

instance ToJSON CheckoutDetailsData where
    toJSON details =
        object
            [ "shippingAddresses" .= cddShippingAddresses details
            , "billingAddresses" .= cddBillingAddresses details
            , "items" .= cddItems details
            , "charges" .= cddCharges details
            , "storeCredit" .= cddStoreCredit details
            ]

type CustomerDetailsRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

customerDetailsRoute :: AuthToken -> CustomerDetailsParameters -> App CheckoutDetailsData
customerDetailsRoute token parameters = do
    (Entity customerId customer) <- validateToken token
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
            , cddStoreCredit = customerStoreCredit customer
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
                , cddStoreCredit = 0
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
    try >=> eitherM handle
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
        , cpopStoreCredit :: Maybe Cents
        , cpopComment :: T.Text
        , cpopStripeToken :: T.Text
        }

instance FromJSON CustomerPlaceOrderParameters where
    parseJSON = withObject "CustomerPlaceOrderParameters" $ \v ->
        CustomerPlaceOrderParameters
            <$> v .: "shippingAddress"
            <*> v .: "billingAddress"
            <*> v .:? "storeCredit"
            <*> v .: "comment"
            <*> v .: "stripeToken"

instance Validation CustomerPlaceOrderParameters where
    validators parameters = do
        shippingValidators <- validateAddress $ cpopShippingAddress parameters
        billingValidators <- validateAddress $ cpopBillingAddress parameters
        return $
            ( "", [ ( "There was an error processing your payment, please try again."
                    , T.null $ cpopStripeToken parameters )
                  ] )
                : map (first $ T.append "shipping-") shippingValidators
                ++ map (first $ T.append "billing-") billingValidators
        where validateAddress a =
                case a of
                    NewAddress f ->
                        validators f
                    ExistingAddress addrId _ -> do
                        invalidAddress <- V.exists addrId
                        return
                            [ ( ""
                              , [ ( "Please refresh the page or try adding a new address instead."
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


-- | Place an Order using a Customer's Cart.
--
-- If the Customer has a StripeId, add a new Card & set it as their
-- Default. Otherwise create a new Stripe Customer for them. Then charge
-- the Stripe Customer.
customerPlaceOrderRoute :: AuthToken -> CustomerPlaceOrderParameters -> App PlaceOrderData
customerPlaceOrderRoute token = validate >=> \parameters -> do
    ce@(Entity customerId customer) <- validateToken token
    let shippingParameter = cpopShippingAddress parameters
        stripeToken = cpopStripeToken parameters
    stripeCustomerId <- getOrCreateStripeCustomer ce stripeToken
    currentTime <- liftIO getCurrentTime
    (billingAddress, orderId, orderTotal, cartId, appliedCredit) <-
        createAddressesAndOrder ce shippingParameter (cpopBillingAddress parameters)
            (cpopStoreCredit parameters) (cpopComment parameters) currentTime
    setCustomerCard customer stripeCustomerId billingAddress stripeToken
    when (orderTotal > 0)
        $ chargeCustomer stripeCustomerId orderId orderTotal
    runDB $ deleteCart cartId
    when (appliedCredit > 0) $ runDB
        $ update customerId [CustomerStoreCredit -=. appliedCredit]
    runDB (OrderPlaced.fetchData orderId)
        >>= maybe (return ()) (void . Emails.send . Emails.OrderPlaced)
    return $ PlaceOrderData orderId
    where getOrCreateStripeCustomer (Entity customerId customer) stripeToken =
            case customerStripeId customer of
                Just stripeId ->
                    return stripeId
                Nothing -> do
                    newId <- createStripeCustomer (customerEmail customer) stripeToken
                    runDB $ update customerId [CustomerStripeId =. Just newId]
                    return newId
          createAddressesAndOrder customer@(Entity customerId _) shippingParam billingParam maybeStoreCredit comment currentTime =
            withPlaceOrderErrors shippingParam . runDB $ do
                shippingAddress <- getOrInsertAddress Shipping customerId shippingParam
                billingAddress <- getOrInsertAddress Billing customerId billingParam
                (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId) >>=
                    maybe (throwM CartNotFound) return
                (orderId, orderTotal, appliedCredit) <- createOrder
                    customer cartId shippingAddress (entityKey billingAddress)
                    maybeStoreCredit comment currentTime
                return (entityVal billingAddress, orderId, orderTotal, cartId, appliedCredit)
          getOrInsertAddress :: AddressType -> CustomerId -> CustomerAddress -> AppSQL (Entity Address)
          getOrInsertAddress addrType customerId = \case
            ExistingAddress addrId makeDefault -> do
                let noAddress = throwM $ AddressNotFound addrType
                address <- get addrId >>= maybe noAddress return
                when (addressCustomerId address /= customerId) noAddress
                when makeDefault $ do
                    updateWhere [AddressCustomerId ==. customerId, AddressType ==. addrType]
                        [AddressIsDefault =. False]
                    if addressType address == addrType then
                        update addrId [AddressIsDefault =. True]
                    else
                        insert_ (address { addressIsDefault = True, addressType = addrType })
                return $ Entity addrId address
            NewAddress ca -> do
                    let newAddress = fromAddressData addrType customerId ca
                    when (addressIsDefault newAddress)
                        $ updateWhere
                            [ AddressType ==. addrType
                            , AddressCustomerId ==. customerId ]
                            [ AddressIsDefault =. False ]
                    insertOrActivateAddress newAddress
          setCustomerCard customer stripeCustomerId billingAddress stripeToken = do
            stripeCardId <- case customerStripeId customer of
                Nothing ->
                    getFirstStripeCard stripeCustomerId
                Just _ -> do
                    -- Already Had Customer, Create New Card w/ Token
                    requestResult <- stripeRequest $
                        createCustomerCardByToken (fromStripeCustomerId stripeCustomerId)
                            (Stripe.TokenId stripeToken)
                    either (V.singleError . errorMsg) (return . Stripe.cardId) requestResult
            updateCardAddress stripeCustomerId stripeCardId billingAddress
            stripeRequest (updateCustomer (fromStripeCustomerId stripeCustomerId)
                -&- Stripe.DefaultCard stripeCardId)
                >>= either (V.singleError . errorMsg) (void . return)


data AnonymousPlaceOrderParameters =
    AnonymousPlaceOrderParameters
        { apopEmail :: T.Text
        , apopPassword :: T.Text
        , apopShippingAddress :: AddressData
        , apopBillingAddress :: AddressData
        , apopComment :: T.Text
        , apopCartToken :: T.Text
        , apopStripeToken :: T.Text
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
            <*> v .: "stripeToken"

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

-- | Place an Order using an Anonymous Cart.
--
-- A new Customer & Order is created & the Customer is charged.
anonymousPlaceOrderRoute :: AnonymousPlaceOrderParameters -> App AnonymousPlaceOrderData
anonymousPlaceOrderRoute = validate >=> \parameters -> do
    encryptedPass <- hashPassword $ apopPassword parameters
    authToken <- generateUniqueToken UniqueToken
    currentTime <- liftIO getCurrentTime
    stripeCustomerId <- createStripeCustomer (apopEmail parameters)
        $ apopStripeToken parameters
    let customer = Customer
            { customerEmail = apopEmail parameters
            , customerStoreCredit = Cents 0
            , customerEncryptedPassword = encryptedPass
            , customerAuthToken = authToken
            , customerStripeId = Just stripeCustomerId
            , customerIsAdmin = False
            }
    (orderId, billingAddress, cartId, orderTotal, orderData) <- withPlaceOrderErrors
        (NewAddress $ apopShippingAddress parameters) . runDB $ do
            customerId <- insert customer
            mergeCarts (apopCartToken parameters) customerId
            (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
                >>= maybe (throwM CartNotFound) return
            let shippingAddress = fromAddressData Shipping customerId
                    $ apopShippingAddress parameters
                billingAddress = fromAddressData Billing customerId
                    $ apopBillingAddress parameters
            shippingId <- insert shippingAddress
            billingId <- insert billingAddress
            (orderId, orderTotal, _) <- createOrder (Entity customerId customer)
                cartId (Entity shippingId shippingAddress) billingId Nothing
                (apopComment parameters) currentTime
            return
                ( orderId
                , billingAddress
                , cartId
                , orderTotal
                , AnonymousPlaceOrderData orderId . toAuthorizationData
                    $ Entity customerId customer
                )
    stripeCardId <- getFirstStripeCard stripeCustomerId
    updateCardAddress stripeCustomerId stripeCardId billingAddress
    chargeCustomer stripeCustomerId (apodOrderId orderData) orderTotal
    runDB $ deleteCart cartId
    runDB (OrderPlaced.fetchData orderId)
        >>= maybe (return ()) (void . Emails.send . Emails.OrderPlaced)
    return orderData


-- | Create a Stripe Customer with an Email & Token.
createStripeCustomer :: T.Text -> T.Text -> App StripeCustomerId
createStripeCustomer email stripeToken =
    stripeRequest (createCustomer -&- Email email -&- Stripe.TokenId stripeToken)
        >>= either (V.singleError . errorMsg)
            (return . StripeCustomerId . Stripe.customerId)


-- | Update the Name & Address of a Stripe Card.
updateCardAddress :: StripeCustomerId -> Stripe.CardId -> Address -> App ()
updateCardAddress stripeCustomerId stripeCardId billingAddress =
    void . stripeRequest $
        updateCustomerCard (fromStripeCustomerId stripeCustomerId) stripeCardId
            -&- Stripe.Name (addressFirstName billingAddress <> " " <> addressLastName billingAddress)
            -&- Stripe.AddressLine1 (addressAddressOne billingAddress)
            -&- Stripe.AddressLine2 (addressAddressTwo billingAddress)
            -&- Stripe.AddressCity (addressCity billingAddress)
            -&- Stripe.AddressState (regionName $ addressState billingAddress)


-- | Charge a Stripe Customer for an Order or throw a validation error.
chargeCustomer :: StripeCustomerId -> OrderId -> Cents -> App ()
chargeCustomer stripeCustomerId orderId orderTotal = do
    chargeResult <- stripeRequest
        (createCharge (toStripeAmount orderTotal) Stripe.USD
            -&- fromStripeCustomerId stripeCustomerId)
    case chargeResult of
        Left err ->
            runDB (update orderId [OrderStatus =. PaymentFailed])
                >> V.singleError (errorMsg err)
        Right stripeCharge ->
            let
                stripeChargeId = StripeChargeId $ Stripe.chargeId stripeCharge
            in
                runDB $ update orderId [OrderStripeChargeId =. Just stripeChargeId]


-- | Return the first Stripe Card Id for a Stripe Customer. This is useful
-- for getting new CardId after creating a Stripe Customer from a Stripe
-- Token.
getFirstStripeCard :: StripeCustomerId -> App Stripe.CardId
getFirstStripeCard stripeCustomerId =
    let
        noCardErrorText =
            "An error occured while charging your card. Please try again or "
                <> "contact us for help."
    in
        stripeRequest (getCustomerCards (fromStripeCustomerId stripeCustomerId)
                -&- Stripe.Limit 1)
            >>= either (V.singleError . errorMsg)
                (return . fmap Stripe.cardId . listToMaybe . Stripe.list)
            >>= maybe (V.singleError noCardErrorText) return


-- | Create an Order and it's Line Items & Products, returning the ID, Total,
-- & Applied Store Credit.
createOrder :: Entity Customer -> CartId -> Entity Address -> AddressId -> Maybe Cents -> T.Text -> UTCTime -> AppSQL (OrderId, Cents, Cents)
createOrder (Entity customerId customer) cartId shippingEntity billingId maybeStoreCredit comment currentTime = do
    let (Entity shippingId shippingAddress) = shippingEntity
    maybeTaxRate <- getTaxRate (Just $ addressCountry shippingAddress)
        (Just $ addressState shippingAddress)
    items <- getCartItems maybeTaxRate $ \c -> c E.^. CartId E.==. E.val cartId
    orderId <- insert Order
        { orderCustomerId = customerId
        , orderStatus = OrderReceived
        , orderBillingAddressId = billingId
        , orderShippingAddressId = shippingId
        , orderCustomerComment = comment
        , orderTaxDescription = maybe "" taxRateDescription maybeTaxRate
        , orderStripeChargeId = Nothing
        , orderCreatedAt = currentTime
        }
    lineTotal <- createLineItems maybeTaxRate shippingAddress items orderId
    productTotals <- createProducts maybeTaxRate items orderId
    let totalCharges = lineTotal + sum productTotals
    case maybeStoreCredit of
        Nothing ->
            return (orderId, totalCharges, 0)
        Just c ->
            if c > 0 && totalCharges > 0 then do
                let storeCredit = min (customerStoreCredit customer) $ min totalCharges c
                insert_ OrderLineItem
                    { orderLineItemOrderId = orderId
                    , orderLineItemType = StoreCreditLine
                    , orderLineItemDescription = "Store Credit"
                    , orderLineItemAmount = storeCredit
                    }
                return (orderId, totalCharges - storeCredit, storeCredit)
            else
                return (orderId, totalCharges, 0)


-- | Delete a Cart & all it's Items.
deleteCart :: CartId -> AppSQL ()
deleteCart cartId =
    deleteWhere [CartItemCartId ==. cartId] >> delete cartId

createLineItems :: Maybe TaxRate -> Address -> [CartItemData] -> OrderId -> AppSQL Cents
createLineItems maybeTaxRate shippingAddress items orderId = do
    charges <- getCharges maybeTaxRate (Just $ addressCountry shippingAddress) items
    shippingCharge <-
        case ccShippingMethods charges of
            [] ->
                throwM NoShippingMethod
            charge : _ ->
                return charge
    shippingLine <- insertEntity $ makeLine ShippingLine shippingCharge
    surcharges <- mapM (insertEntity . makeLine SurchargeLine) $ ccSurcharges charges
    return $
        sum (map (orderLineItemAmount . entityVal) surcharges)
        + orderLineItemAmount (entityVal shippingLine)
    where makeLine lineType charge =
            OrderLineItem
                { orderLineItemOrderId = orderId
                , orderLineItemType = lineType
                , orderLineItemDescription = ccDescription charge
                , orderLineItemAmount = ccAmount charge
                }

createProducts :: Maybe TaxRate -> [CartItemData] -> OrderId -> AppSQL [Cents]
createProducts maybeTaxRate items orderId =
    mapM (fmap calculateTotal <$> insertEntity . makeProduct) items
    where calculateTotal (Entity _ prod) =
            orderProductPrice prod * (Cents $ orderProductQuantity prod)
                + orderProductTax prod
          makeProduct CartItemData { cidVariant, cidQuantity } =
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
