{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Checkout
    ( CheckoutAPI
    , checkoutRoutes
    ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Exception.Safe (MonadThrow, MonadCatch, Exception, throwM, try)
import Control.Monad ((>=>), (<=<), when, unless, void)
import Control.Monad.Reader (asks, ask, lift, liftIO)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.List (partition, find)
import Data.Maybe (listToMaybe, isJust, isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), (=.), (-=.), (+=.), Entity(..), insert, insert_, insertEntity, get
    , getBy, selectList, update, updateWhere, delete, deleteWhere, count
    , getEntity, selectFirst
    )
import Servant ((:<|>)(..), (:>), AuthProtect, JSON, Post, ReqBody, err404)
import Web.Stripe ((-&-))
import Web.Stripe.Card (createCustomerCardByToken, updateCustomerCard, getCustomerCards)
import Web.Stripe.Charge (createCharge)
import Web.Stripe.Customer (Email(..), createCustomer, updateCustomer)

import Auth
import Avalara
    ( CommitTransactionRequest(..), VoidTransactionRequest(..), VoidReason(..)
    )
import Config
import Models
import Models.Fields
import Server
import Routes.CommonData
    ( AuthorizationData, toAuthorizationData, CartItemData(..), CartCharges(..)
    , CartCharge(..), getCartItems, getCharges, AddressData(..), toAddressData
    , fromAddressData, ShippingCharge(..), VariantData(..), getVariantPrice
    , OrderDetails(..), toCheckoutOrder, getCheckoutProducts, CheckoutProduct
    )
import Routes.AvalaraUtils (createAvalaraTransaction, createAvalaraCustomer)
import Routes.Utils (hashPassword, generateUniqueToken)
import Validation (Validation(..))
import Workers (Task(Avalara), AvalaraTask(..), enqueueTask)

import qualified Avalara
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Emails
import qualified Validation as V
import qualified Web.Stripe as Stripe
import qualified Web.Stripe.Types as Stripe


type CheckoutAPI =
         "customer-details" :> CustomerDetailsRoute
    :<|> "customer-place-order" :> CustomerPlaceOrderRoute
    :<|> "anonymous-details" :> AnonymousDetailsRoute
    :<|> "anonymous-place-order" :> AnonymousPlaceOrderRoute
    :<|> "success" :> SuccessRoute

type CheckoutRoutes =
         (WrappedAuthToken -> CustomerDetailsParameters -> App (Cookied CheckoutDetailsData))
    :<|> (WrappedAuthToken -> CustomerPlaceOrderParameters -> App (Cookied PlaceOrderData))
    :<|> (AnonymousDetailsParameters -> App CheckoutDetailsData)
    :<|> (AnonymousPlaceOrderParameters -> App (Cookied AnonymousPlaceOrderData))
    :<|> (WrappedAuthToken -> SuccessParameters -> App (Cookied OrderDetails))

checkoutRoutes :: CheckoutRoutes
checkoutRoutes =
         customerDetailsRoute
    :<|> customerPlaceOrderRoute
    :<|> anonymousDetailsRoute
    :<|> anonymousPlaceOrderRoute
    :<|> customerSuccessRoute


-- COUPON ERRORS

data CouponError
    = CouponNotFound
    | CouponInactive
    | CouponExpired
    | CouponMaxUses
    | CouponCustomerMaxUses
    | CouponBelowOrderMinimum Cents
    deriving (Show, Typeable)

instance Exception CouponError

handleCouponErrors :: CouponError -> App a
handleCouponErrors = V.singleFieldError "coupon" . \case
    CouponNotFound ->
        "Sorry, we couldn't find a coupon with that code."
    CouponExpired ->
        "Sorry, that coupon has expired."
    CouponInactive ->
        "Sorry, that coupon is no longer active."
    CouponMaxUses ->
        "Sorry, that coupon has reached it's maximum number of uses."
    CouponCustomerMaxUses ->
        "Sorry, you've used that coupon the maximum amount of times."
    CouponBelowOrderMinimum minimumAmount ->
        "Sorry, a minimum subtotal of " <> formatCents minimumAmount
            <> " is required to use this coupon."

-- PRIORITY SHIPPING/HANDLING ERRORS
--
data PrioritySHError
    = PriorityShippingNotAvailable
    deriving (Show, Typeable)

instance Exception PrioritySHError

handlePriorityShippingErrors :: PrioritySHError -> App a
handlePriorityShippingErrors = V.singleFieldError "priority-shipping" . \case
    PriorityShippingNotAvailable ->
        "Sorry, priority shipping & handling is not available with the items "
            <> "currently in your cart."


-- CART/ADDRESS DETAILS


data CheckoutDetailsError
    = DetailsPriorityError PrioritySHError
    | DetailsCouponError CouponError
    deriving (Show, Typeable)

instance Exception CheckoutDetailsError

withCheckoutDetailsErrors :: App a -> App a
withCheckoutDetailsErrors =
    try >=> eitherM handle
    where
        handle :: CheckoutDetailsError -> App a
        handle = \case
            DetailsPriorityError priorityError ->
                handlePriorityShippingErrors priorityError
            DetailsCouponError couponError ->
                handleCouponErrors couponError



data CustomerDetailsParameters =
    CustomerDetailsParameters
        { cdpShipping :: Maybe CustomerAddress
        , cdpExistingAddress :: Maybe AddressId
        , cdpCouponCode :: Maybe T.Text
        , cdpPriorityShipping :: Bool
        }

instance FromJSON CustomerDetailsParameters where
    parseJSON = withObject "CustomerDetailsParameters" $ \v ->
        CustomerDetailsParameters
            <$> v .:? "address"
            <*> v .:? "addressId"
            <*> v .:? "couponCode"
            <*> v .:  "priorityShipping"

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
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerDetailsParameters
    :> Post '[JSON] (Cookied CheckoutDetailsData)

customerDetailsRoute :: WrappedAuthToken -> CustomerDetailsParameters -> App (Cookied CheckoutDetailsData)
customerDetailsRoute token parameters = withValidatedCookie token $ \(Entity customerId customer) -> do
    let priorityShipping =
            cdpPriorityShipping parameters
    currentTime <- liftIO getCurrentTime
    withCheckoutDetailsErrors . runDB $ do
        customerAddresses <- selectList
            [AddressCustomerId ==. customerId, AddressIsActive ==. True] []
        let (shippingAddresses, billingAddresses) =
                partition (\a -> addressType (entityVal a) == Shipping)
                    customerAddresses
        maybeShipping <- getShippingAddress parameters shippingAddresses
        maybeCoupon <- mapException DetailsCouponError $ sequence
            $ getCoupon currentTime (Just customerId) <$> cdpCouponCode parameters
        items <- getCartItems $ \c ->
            c E.^. CartCustomerId E.==. E.just (E.val customerId)
        charges <- detailsCharges maybeShipping items maybeCoupon priorityShipping
        return CheckoutDetailsData
            { cddShippingAddresses = map toAddressData shippingAddresses
            , cddBillingAddresses = map toAddressData billingAddresses
            , cddItems = items
            , cddCharges = charges
            , cddStoreCredit = customerStoreCredit customer
            }
    where
        getShippingAddress :: CustomerDetailsParameters -> [Entity Address] -> AppSQL (Maybe AddressData)
        getShippingAddress ps addrs =
            let
                maybeFromAddress =
                    fmap toAddressData
                    $ (\a -> find (\(Entity addrId _) -> addrId == a) addrs)
                    =<< cdpExistingAddress ps
                maybeFromDefault =
                    toAddressData <$> find (addressIsDefault . entityVal) addrs
            in do
            maybeFromParams <- case cdpShipping ps of
                Nothing ->
                    return Nothing
                Just c ->
                    getCustomerAddressData c
            return $ asum [maybeFromAddress, maybeFromParams, maybeFromDefault]
        getCustomerAddressData :: CustomerAddress -> AppSQL (Maybe AddressData)
        getCustomerAddressData = \case
            NewAddress addr ->
                return $ Just addr
            ExistingAddress addrId _ ->
                fmap toAddressData <$> getEntity addrId


data AnonymousDetailsParameters =
    AnonymousDetailsParameters
        { adpShipping :: Maybe AddressData
        , adpCouponCode :: Maybe T.Text
        , adpPriorityShipping :: Bool
        , adpCartToken :: T.Text
        }

instance FromJSON AnonymousDetailsParameters where
    parseJSON = withObject "AnonymousDetailsParameters" $ \v ->
        AnonymousDetailsParameters
            <$> v .:? "address"
            <*> v .:? "couponCode"
            <*> v .:  "priorityShipping"
            <*> v .:  "sessionToken"

type AnonymousDetailsRoute =
       ReqBody '[JSON] AnonymousDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

anonymousDetailsRoute :: AnonymousDetailsParameters -> App CheckoutDetailsData
anonymousDetailsRoute parameters =
    let
        maybeAddress =
            adpShipping parameters
        priorityShipping =
            adpPriorityShipping parameters
    in
        withCheckoutDetailsErrors . runDB $ do
            currentTime <- liftIO getCurrentTime
            maybeCoupon <- mapException DetailsCouponError $ sequence
                $ getCoupon currentTime Nothing <$> adpCouponCode parameters
            items <- getCartItems $ \c ->
                c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
            charges <- detailsCharges maybeAddress items maybeCoupon priorityShipping
            return CheckoutDetailsData
                { cddShippingAddresses = []
                , cddBillingAddresses = []
                , cddItems = items
                , cddCharges = charges
                , cddStoreCredit = 0
                }


-- | Get the Coupon from a code, if it is valid. Does not validate
-- the minimum order size, since the CartCharges are needed for
-- that. Throws CouponError.
getCoupon :: UTCTime -> Maybe CustomerId -> T.Text -> AppSQL (Entity Coupon)
getCoupon currentTime maybeCustomerId couponCode = do
    maybeCoupon <- getBy $ UniqueCoupon couponCode
    case maybeCoupon of
        Nothing ->
            throwM CouponNotFound
        Just e@(Entity couponId coupon) -> do
            unless (couponIsActive coupon)
                $ throwM CouponInactive
            when (currentTime > couponExpirationDate coupon)
                $ throwM CouponExpired
            when (couponTotalUses coupon /= 0) $ do
                totalUses <- count [OrderCouponId ==. Just couponId]
                when (totalUses >= fromIntegral (couponTotalUses coupon))
                    $ throwM CouponMaxUses
            when (couponUsesPerCustomer coupon /= 0) $
                checkCustomerUses e
            return e
    where
        checkCustomerUses :: Entity Coupon -> AppSQL ()
        checkCustomerUses (Entity couponId coupon) =
            case maybeCustomerId of
                Just customerId -> do
                    customerUses <- count
                        [ OrderCouponId ==. Just couponId
                        , OrderCustomerId ==. customerId
                        ]
                    when (customerUses >= fromIntegral (couponUsesPerCustomer coupon))
                        $ throwM CouponCustomerMaxUses
                Nothing -> return ()

-- | Get the CartCharges for the details routes & check the coupon minimum
-- & priority shipping availability. Throws 'CheckoutDetailsError'.
detailsCharges :: Maybe AddressData -> [CartItemData] -> Maybe (Entity Coupon) -> Bool -> AppSQL CartCharges
detailsCharges maybeShipping items maybeCoupon priorityShipping = do
    charges <- getCharges maybeShipping items maybeCoupon priorityShipping True
    mapException DetailsCouponError
        $ checkCouponMeetsMinimum maybeCoupon charges
    mapException DetailsPriorityError
        $ checkPriorityShippingAvailable priorityShipping charges
    return charges

-- | Ensure the product total equals or exceeds the Coupon
-- minimum by seeing if it was removed from the CartCharges. If
-- it does not meet the minimum, throw a CouponError.
checkCouponMeetsMinimum :: MonadThrow m => Maybe (Entity Coupon) -> CartCharges -> m ()
checkCouponMeetsMinimum maybeCoupon CartCharges { ccCouponDiscount } =
    case (maybeCoupon, ccCouponDiscount) of
        (Just (Entity _ coupon), Nothing) ->
            throwM $ CouponBelowOrderMinimum $ couponMinimumOrder coupon
        _ ->
            return ()

-- | Throw a PriorityShippingNotAvailable error if priority shipping is
-- selected & the shipping method does not have priority shipping
-- available.
--
-- Does nothing if no shipping methods are present.
checkPriorityShippingAvailable :: MonadThrow m => Bool -> CartCharges -> m ()
checkPriorityShippingAvailable hasPriority charges =
    when hasPriority $
        case ccShippingMethods charges of
            ShippingCharge _ Nothing:_ ->
                throwM PriorityShippingNotAvailable
            _ ->
                return ()


-- PLACE ORDER


data PlaceOrderError
    = CartNotFound
    | AddressNotFound AddressType
    | BillingAddressRequired
    | StripeTokenRequired
    | NotEnoughStoreCredit
    | NoShippingMethod
    | StripeError Stripe.StripeError
    | CardChargeError
    | PlaceOrderCouponError CouponError
    | AvalaraNoTransactionCode
    deriving (Show, Typeable)

instance Exception PlaceOrderError

withPlaceOrderErrors :: CustomerAddress -> App a -> App a
withPlaceOrderErrors shippingAddress =
    try >=> eitherM handle
    where
        handle :: PlaceOrderError -> App a
        handle = \case
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
            BillingAddressRequired ->
                V.singleFieldError "billing-"
                    "A billing address is required with non-free orders."
            StripeTokenRequired ->
                V.singleFieldError "" $
                    "An error occured when verifying your order total. " <>
                    "Please refresh the page or contact us."
            NotEnoughStoreCredit ->
                V.singleFieldError "store-credit"
                    "You cannot apply more store credit than you have."
            StripeError stripeError ->
                V.singleError $ Stripe.errorMsg stripeError
            CardChargeError ->
                V.singleError
                    $ "An error occured while charging your card. Please try again or "
                    <> "contact us for help."
            PlaceOrderCouponError couponError ->
                handleCouponErrors couponError
            AvalaraNoTransactionCode ->
                V.singleError
                    $ "An error occured while calculating the sales tax due. Please try again or "
                    <> "contact us for help. (Error ANTC: No Transction Code Returned)"



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
        , cpopBillingAddress :: Maybe CustomerAddress
        , cpopStoreCredit :: Maybe Cents
        , cpopPriorityShipping :: Bool
        , cpopCouponCode :: Maybe T.Text
        , cpopComment :: T.Text
        , cpopStripeToken :: Maybe T.Text
        }

instance FromJSON CustomerPlaceOrderParameters where
    parseJSON = withObject "CustomerPlaceOrderParameters" $ \v ->
        CustomerPlaceOrderParameters
            <$> v .: "shippingAddress"
            <*> v .:? "billingAddress"
            <*> v .:? "storeCredit"
            <*> v .: "priorityShipping"
            <*> v .:? "couponCode"
            <*> v .: "comment"
            <*> v .:? "stripeToken"

instance Validation CustomerPlaceOrderParameters where
    validators parameters = do
        shippingValidators <- validateAddress $ cpopShippingAddress parameters
        billingValidators <- maybeValidate validateAddress $ cpopBillingAddress parameters
        return $
            ( "", [ ( "There was an error processing your payment, please try again."
                    , whenJust T.null $ cpopStripeToken parameters )
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
              maybeValidate =
                maybe (return [])
              whenJust =
                maybe False

data PlaceOrderData =
    PlaceOrderData
        { podOrderId :: OrderId
        , podLines :: [Entity OrderLineItem]
        , podProducts :: [CheckoutProduct]
        }

instance ToJSON PlaceOrderData where
    toJSON order =
        object
            [ "orderId" .= podOrderId order
            , "lines" .= podLines order
            , "products" .= podProducts order
            ]

type CustomerPlaceOrderRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerPlaceOrderParameters
    :> Post '[JSON] (Cookied PlaceOrderData)


-- | Place an Order using a Customer's Cart.
--
-- If the Customer has a StripeId, add a new Card & set it as their
-- Default. Otherwise create a new Stripe Customer for them. Then charge
-- the Stripe Customer.
customerPlaceOrderRoute :: WrappedAuthToken -> CustomerPlaceOrderParameters -> App (Cookied PlaceOrderData)
customerPlaceOrderRoute = validateCookieAndParameters $ \ce@(Entity customerId customer) parameters -> do
    let shippingParameter = cpopShippingAddress parameters
        maybeStripeToken = cpopStripeToken parameters
    currentTime <- liftIO getCurrentTime
    orderId <- withPlaceOrderErrors shippingParameter . runDB $ do
        (maybeBillingAddress, shippingAddress, order@(Entity orderId _), preTaxTotal, cartId, appliedCredit) <-
            createAddressesAndOrder ce shippingParameter (cpopBillingAddress parameters)
                (cpopStoreCredit parameters) (cpopPriorityShipping parameters)
                (cpopCouponCode parameters) (cpopComment parameters) currentTime
        when (appliedCredit > customerStoreCredit customer) $
            throwM NotEnoughStoreCredit
        when (appliedCredit > 0) $
            update customerId [CustomerStoreCredit -=. appliedCredit]
        let remainingCredit = maybe 0 (\c -> c - appliedCredit) $ cpopStoreCredit parameters
        when (preTaxTotal > 0 || preTaxTotal + appliedCredit > 0 ) $
            withAvalaraTransaction order preTaxTotal remainingCredit ce shippingAddress maybeBillingAddress $ \orderTotal ->
                when (orderTotal >= 50) $ case (maybeBillingAddress, maybeStripeToken) of
                    (Just (Entity _ billingAddress), Just stripeToken) -> do
                        stripeCustomerId <- getOrCreateStripeCustomer ce stripeToken
                        setCustomerCard customer stripeCustomerId billingAddress stripeToken
                        chargeCustomer stripeCustomerId orderId orderTotal
                    (Nothing, _) ->
                        throwM BillingAddressRequired
                    (_, Nothing) ->
                        throwM StripeTokenRequired
        deleteCart cartId
        return orderId
    cfg <- ask
    runDB (Emails.getEmailData $ Emails.OrderPlaced orderId)
        >>= either (const $ return ()) (liftIO . Emails.sendWithRetries cfg)
    (orderLines, products) <- runDB $ (,)
        <$> selectList [OrderLineItemOrderId ==. orderId] []
        <*> getCheckoutProducts orderId
    return $ PlaceOrderData
        { podOrderId = orderId
        , podLines = orderLines
        , podProducts = products
        }
    where
          getOrCreateStripeCustomer :: Entity Customer -> T.Text -> AppSQL StripeCustomerId
          getOrCreateStripeCustomer (Entity customerId customer) stripeToken =
            case customerStripeId customer of
                Just stripeId ->
                    return stripeId
                Nothing -> do
                    newId <- createStripeCustomer (customerEmail customer) stripeToken
                    update customerId [CustomerStripeId =. Just newId]
                    return newId
          createAddressesAndOrder
            :: Entity Customer -> CustomerAddress -> Maybe CustomerAddress
            -> Maybe Cents -> Bool -> Maybe T.Text -> T.Text -> UTCTime
            -> AppSQL (Maybe (Entity Address), Entity Address, Entity Order, Cents, CartId, Cents)
          createAddressesAndOrder customer@(Entity customerId _) shippingParam billingParam maybeStoreCredit priorityShipping maybeCouponCode comment currentTime = do
            shippingAddress <- getOrInsertAddress Shipping customerId shippingParam
            billingAddress <- sequence $ getOrInsertAddress Billing customerId <$> billingParam
            (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
                >>= maybe (throwM CartNotFound) return
            (order, orderTotal, appliedCredit) <- createOrder
                customer cartId shippingAddress (entityKey <$> billingAddress)
                maybeStoreCredit priorityShipping maybeCouponCode comment currentTime
            return (billingAddress, shippingAddress, order, orderTotal, cartId, appliedCredit)
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
          setCustomerCard :: Customer -> StripeCustomerId -> Address -> T.Text -> AppSQL ()
          setCustomerCard customer stripeCustomerId billingAddress stripeToken = do
            stripeCardId <- case customerStripeId customer of
                Nothing ->
                    getFirstStripeCard stripeCustomerId
                Just _ -> do
                    -- Already Had Customer, Create New Card w/ Token
                    requestResult <- lift . stripeRequest $
                        createCustomerCardByToken (fromStripeCustomerId stripeCustomerId)
                            (Stripe.TokenId stripeToken)
                    either (throwM . StripeError) (return . Stripe.cardId) requestResult
            updateCardAddress stripeCustomerId stripeCardId billingAddress
            lift (stripeRequest (updateCustomer (fromStripeCustomerId stripeCustomerId)
                -&- Stripe.DefaultCard stripeCardId))
                >>= either (throwM . StripeError) (void . return)


data AnonymousPlaceOrderParameters =
    AnonymousPlaceOrderParameters
        { apopEmail :: T.Text
        , apopPassword :: T.Text
        , apopShippingAddress :: AddressData
        , apopBillingAddress :: Maybe AddressData
        , apopPriorityShipping :: Bool
        , apopCouponCode :: Maybe T.Text
        , apopComment :: T.Text
        , apopCartToken :: T.Text
        , apopStripeToken :: Maybe T.Text
        }

instance FromJSON AnonymousPlaceOrderParameters where
    parseJSON = withObject "AnonymousPlaceOrderParameters" $ \v ->
        AnonymousPlaceOrderParameters
            <$> v .: "email"
            <*> v .: "password"
            <*> v .: "shippingAddress"
            <*> v .: "billingAddress"
            <*> v .: "priorityShipping"
            <*> v .:? "couponCode"
            <*> v .: "comment"
            <*> v .: "sessionToken"
            <*> v .: "stripeToken"

instance Validation AnonymousPlaceOrderParameters where
    validators parameters = do
        emailDoesntExist <- V.uniqueCustomer $ apopEmail parameters
        shippingValidators <- validators $ apopShippingAddress parameters
        billingValidators <- maybe (return []) validators
            $ apopBillingAddress parameters
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
        , apodLines :: [Entity OrderLineItem]
        , apodProducts :: [CheckoutProduct]
        , apodAuthorizationData :: AuthorizationData
        }

instance ToJSON AnonymousPlaceOrderData where
    toJSON orderData =
        object
            [ "orderId" .= apodOrderId orderData
            , "lines" .= apodLines orderData
            , "products" .= apodProducts orderData
            , "authData" .= apodAuthorizationData orderData
            ]

type AnonymousPlaceOrderRoute =
       ReqBody '[JSON] AnonymousPlaceOrderParameters
    :> Post '[JSON] (Cookied AnonymousPlaceOrderData)

-- | Place an Order using an Anonymous Cart.
--
-- A new Customer & Order is created & the Customer is charged.
anonymousPlaceOrderRoute :: AnonymousPlaceOrderParameters -> App (Cookied AnonymousPlaceOrderData)
anonymousPlaceOrderRoute = validate >=> \parameters -> do
    encryptedPass <- hashPassword $ apopPassword parameters
    authToken <- runDB $ generateUniqueToken UniqueToken
    currentTime <- liftIO getCurrentTime
    let shippingParameter = NewAddress $ apopShippingAddress parameters
    (orderId, orderData) <- withPlaceOrderErrors shippingParameter . runDB $ do
        avalaraCustomerCode <- createAvalaraCustomer (apopEmail parameters)
            $ fromMaybe (apopShippingAddress parameters)
            $ apopBillingAddress parameters
        let customer = Customer
                { customerEmail = apopEmail parameters
                , customerStoreCredit = Cents 0
                , customerMemberNumber = ""
                , customerEncryptedPassword = encryptedPass
                , customerAuthToken = authToken
                , customerStripeId = Nothing
                , customerAvalaraCode = avalaraCustomerCode
                , customerIsAdmin = False
                }
        customerId <- insert customer
        mergeAnonymousCart (apopCartToken parameters) customerId
        (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
            >>= maybe (throwM CartNotFound) return
        let shippingAddress = fromAddressData Shipping customerId
                $ apopShippingAddress parameters
            maybeBillingAddress = fromAddressData Billing customerId
                <$> apopBillingAddress parameters
        shippingId <- insert shippingAddress
        billingId <- sequence $ insert <$> maybeBillingAddress
        let shippingEntity = Entity shippingId shippingAddress
            billingEntity = Entity <$> billingId <*> maybeBillingAddress
            customerEntity = Entity customerId customer
        (order@(Entity orderId _), preTaxTotal, _) <- createOrder
            (Entity customerId customer) cartId shippingEntity billingId Nothing
            (apopPriorityShipping parameters) (apopCouponCode parameters)
            (apopComment parameters) currentTime
        when (preTaxTotal > 0)
            $ withAvalaraTransaction order preTaxTotal 0 customerEntity shippingEntity billingEntity
            $ \orderTotal ->
                case (maybeBillingAddress, apopStripeToken parameters) of
                    (Just billingAddress, Just stripeToken) -> do
                        stripeCustomerId <- createStripeCustomer (apopEmail parameters)
                            stripeToken
                        update customerId [CustomerStripeId =. Just stripeCustomerId]
                        stripeCardId <- getFirstStripeCard stripeCustomerId
                        updateCardAddress stripeCustomerId stripeCardId billingAddress
                        chargeCustomer stripeCustomerId orderId orderTotal
                    (Nothing, _) ->
                        throwM BillingAddressRequired
                    (_, Nothing) ->
                        throwM StripeTokenRequired
        deleteCart cartId
        orderLines <- selectList [OrderLineItemOrderId ==. orderId] []
        products <- getCheckoutProducts orderId
        return
            ( orderId
            , AnonymousPlaceOrderData
                { apodOrderId = orderId
                , apodLines = orderLines
                , apodProducts = products
                , apodAuthorizationData = toAuthorizationData $ Entity customerId customer
                }
            )
    cfg <- ask
    runDB (Emails.getEmailData $ Emails.OrderPlaced orderId)
        >>= either (const $ return ()) (liftIO . Emails.sendWithRetries cfg)
    addSessionCookie temporarySession (AuthToken authToken) orderData


-- | Create a Stripe Customer with an Email & Token.
createStripeCustomer :: T.Text -> T.Text -> AppSQL StripeCustomerId
createStripeCustomer email stripeToken =
    lift (stripeRequest $ createCustomer -&- Email email -&- Stripe.TokenId stripeToken)
        >>= either (throwM . StripeError)
            (return . StripeCustomerId . Stripe.customerId)


-- | Update the Name & Address of a Stripe Card.
updateCardAddress :: StripeCustomerId -> Stripe.CardId -> Address -> AppSQL ()
updateCardAddress stripeCustomerId stripeCardId billingAddress =
    lift . void . stripeRequest $
        updateCustomerCard (fromStripeCustomerId stripeCustomerId) stripeCardId
            -&- Stripe.Name (addressFirstName billingAddress <> " " <> addressLastName billingAddress)
            -&- Stripe.AddressLine1 (addressAddressOne billingAddress)
            -&- Stripe.AddressLine2 (addressAddressTwo billingAddress)
            -&- Stripe.AddressCity (addressCity billingAddress)
            -&- Stripe.AddressState (regionName $ addressState billingAddress)


-- | Commit an uncommited Avalara Sales Tax Transaction. Throws
-- a 'PlaceOrderError' on failure.
commitAvalaraTransaction :: Avalara.Transaction -> AppSQL (Maybe ())
commitAvalaraTransaction transaction = do
    companyCode <- lift $ asks getAvalaraCompanyCode
    transactionCode <- case Avalara.tCode transaction of
        Just tCode ->
            return tCode
        Nothing ->
            throwM AvalaraNoTransactionCode
    let request =
            Avalara.commitTransaction companyCode transactionCode
                $ CommitTransactionRequest { ctsrCommit = True }
    lift (avalaraRequest request) >>= \case
        Avalara.SuccessfulResponse _ ->
            return $ Just ()
        _ ->
            return Nothing


-- | Void the Transaction by marking it as 'DocDeleted'. Throws
-- a 'PlaceOrderError' on failure.
voidAvalaraTransaction :: Avalara.Transaction -> AppSQL (Maybe ())
voidAvalaraTransaction transction = do
    companyCode <- lift $ asks getAvalaraCompanyCode
    transactionCode <- case Avalara.tCode transction of
        Just tCode ->
            return tCode
        Nothing ->
            throwM AvalaraNoTransactionCode
    let request =
            Avalara.voidTransaction companyCode transactionCode
                $ VoidTransactionRequest { vtrCode = DocDeleted }
    lift (avalaraRequest request) >>= \case
        Avalara.SuccessfulResponse _ ->
            return $ Just ()
        _ ->
            return Nothing


-- | Create an uncommitted Transaction for the Order, then run an action
-- that requires the OrderTotal.
--
-- If the action completes successfully, commit the Transction, set the
-- Order's TransactionCode and add a TaxLine for the Order.
--
-- If the action throws a 'PlaceOrderError', void the created Transction
-- and re-throw the error.
--
-- Note that the Avalara API is only hit when the Config's 'AvalaraStatus'
-- is set to 'AvalaraEnabled' or 'AvalaraTesting'. The TaxLine is only
-- created if the 'AvalaraStatus' is 'AvalaraEnabled'.
withAvalaraTransaction
    :: Entity Order
    -> Cents
    -> Cents
    -> Entity Customer
    -> Entity Address
    -> Maybe (Entity Address)
    -> (Cents -> AppSQL ())
    -> AppSQL ()
withAvalaraTransaction order@(Entity orderId _) preTaxTotal storeCredit c@(Entity customerId customer) shipping billing innerAction =
    lift (asks getAvalaraStatus) >>= \case
        AvalaraDisabled ->
            innerAction preTaxTotal
        status ->
            createAvalaraTransaction order shipping billing c False >>= \case
                Nothing ->
                    try (innerAction preTaxTotal) >>= \case
                        Right () ->
                            enqueueTask Nothing . Avalara $ CreateTransaction orderId
                        Left (err :: PlaceOrderError) ->
                            throwM err
                Just taxTransaction -> do
                    let taxTotal =
                            if status == AvalaraEnabled then
                                maybe 0 fromDollars (Avalara.tTotalTax taxTransaction)
                            else
                                0
                        appliedCredit = min storeCredit taxTotal
                        orderTotal = taxTotal + preTaxTotal - appliedCredit
                    when (appliedCredit > customerStoreCredit customer) $
                        throwM NotEnoughStoreCredit
                    when (appliedCredit > 0) $
                            update customerId [CustomerStoreCredit -=. appliedCredit]
                    try (innerAction orderTotal) >>= \case
                        Right () -> do
                            commitResult <- commitAvalaraTransaction taxTransaction
                            when (isNothing commitResult) $
                                enqueueTask Nothing . Avalara $ CommitTransaction taxTransaction
                            companyCode <- lift $ asks getAvalaraCompanyCode
                            let transactionCode = AvalaraTransactionCode companyCode
                                    <$> Avalara.tCode taxTransaction
                            update orderId [OrderAvalaraTransactionCode =. transactionCode]
                            when (status == AvalaraEnabled) $
                                insert_ OrderLineItem
                                    { orderLineItemOrderId = orderId
                                    , orderLineItemType = TaxLine
                                    , orderLineItemAmount = taxTotal
                                    , orderLineItemDescription = "Sales Tax"
                                    }
                            when (status == AvalaraTesting) $
                                voidOrEnqueueTransaction taxTransaction
                            when (appliedCredit > 0) $
                                updateStoreCredit appliedCredit
                        Left (err :: PlaceOrderError) -> do
                            voidOrEnqueueTransaction taxTransaction
                            throwM err
  where
    -- Void a transaction, enqueueing a VoidTransaction task if the request
    -- fails.
    voidOrEnqueueTransaction :: Avalara.Transaction -> AppSQL ()
    voidOrEnqueueTransaction taxTransaction = do
        voidResult <- voidAvalaraTransaction taxTransaction
        when (isNothing voidResult) $
            enqueueTask Nothing . Avalara
                $ VoidTransaction taxTransaction
    -- Add the amount to an existing StoreCreditLine or create one if one
    -- doesn't exist.
    updateStoreCredit :: Cents -> AppSQL ()
    updateStoreCredit appliedCredit =
        selectFirst [OrderLineItemOrderId ==. orderId, OrderLineItemType ==. StoreCreditLine] []
            >>= \case
            Nothing ->
                insert_ OrderLineItem
                    { orderLineItemOrderId = orderId
                    , orderLineItemType = StoreCreditLine
                    , orderLineItemAmount = appliedCredit
                    , orderLineItemDescription = "Store Credit"
                    }
            Just (Entity creditId _) ->
                update creditId
                    [ OrderLineItemAmount +=. appliedCredit ]



-- | Charge a Stripe Customer for an Order or throw a validation error.
chargeCustomer :: StripeCustomerId -> OrderId -> Cents -> AppSQL ()
chargeCustomer stripeCustomerId orderId orderTotal = do
    chargeResult <- lift $ stripeRequest
        (createCharge (toStripeAmount orderTotal) Stripe.USD
            -&- fromStripeCustomerId stripeCustomerId)
    case chargeResult of
        Left err ->
            update orderId [OrderStatus =. PaymentFailed]
                >> throwM (StripeError err)
        Right stripeCharge ->
            let
                stripeChargeId =
                    StripeChargeId $ Stripe.chargeId stripeCharge
                stripeLastFour =
                    Stripe.cardLastFour <$> Stripe.chargeCreditCard stripeCharge
                stripeIssuer =
                    T.pack . show . Stripe.cardBrand <$> Stripe.chargeCreditCard stripeCharge
            in
                update orderId
                    [ OrderStatus =. PaymentReceived
                    , OrderStripeChargeId =. Just stripeChargeId
                    , OrderStripeLastFour =. stripeLastFour
                    , OrderStripeIssuer =. stripeIssuer
                    ]


-- | Return the first Stripe Card Id for a Stripe Customer. This is useful
-- for getting new CardId after creating a Stripe Customer from a Stripe
-- Token.
getFirstStripeCard :: StripeCustomerId -> AppSQL Stripe.CardId
getFirstStripeCard stripeCustomerId =
    lift (stripeRequest (getCustomerCards (fromStripeCustomerId stripeCustomerId)
            -&- Stripe.Limit 1))
        >>= either (throwM . StripeError)
            (return . fmap Stripe.cardId . listToMaybe . Stripe.list)
        >>= maybe (throwM CardChargeError) return


-- | Create an Order and it's Line Items & Products, returning the ID, Total,
-- & Applied Store Credit.
--
-- TODO: Turn all the parameters into a `CreateOrderParams` data type so we
-- can pass them by field name.
createOrder
    :: Entity Customer
    -- ^ Customer placing the Order
    -> CartId
    -- ^ Cart ID that should be turned into an Order
    -> Entity Address
    -- ^ Shipping Address
    -> Maybe AddressId
    -- ^ Billing Address ID
    -> Maybe Cents
    -- ^ Store Credit
    -> Bool
    -- ^ Enable Priority Shipping
    -> Maybe T.Text
    -- ^ Coupon Code
    -> T.Text
    -- ^ Order Comment
    -> UTCTime
    -- ^ Current Time
    -> AppSQL (Entity Order, Cents, Cents)
createOrder (Entity customerId customer) cartId shippingEntity billingId maybeStoreCredit priorityShipping maybeCouponCode comment currentTime = do
    items <- getCartItems $ \c -> c E.^. CartId E.==. E.val cartId
    let order = Order
            { orderCustomerId = customerId
            , orderStatus = OrderReceived
            , orderBillingAddressId = billingId
            , orderShippingAddressId = entityKey shippingEntity
            , orderCustomerComment = comment
            , orderAdminComments = []
            , orderAvalaraTransactionCode = Nothing
            , orderStripeChargeId = Nothing
            , orderStripeLastFour = Nothing
            , orderStripeIssuer = Nothing
            , orderCouponId = Nothing
            , orderCreatedAt = currentTime
            }
    orderId <- insert order
    (lineTotal, maybeCouponId) <- createLineItems currentTime customerId
        shippingEntity items orderId priorityShipping maybeCouponCode
    when (isJust maybeCouponId) $
        update orderId [OrderCouponId =. maybeCouponId]
    productTotals <- createProducts items orderId
    let totalCharges = lineTotal + fromIntegral (fromCents $ sum productTotals)
        orderEntity = Entity orderId order
    if totalCharges < 0 then
        -- TODO: Throw an error? This means credits > charges which shouldn't happen...
        return (orderEntity, 0, 0)
    else
        let totalInCents = Cents $ fromIntegral totalCharges in
        case maybeStoreCredit of
            Nothing ->
                return (orderEntity, totalInCents, 0)
            Just c ->
                if c > 0 && totalInCents > 0 then do
                    let storeCredit = min (customerStoreCredit customer) $ min totalInCents c
                    insert_ OrderLineItem
                        { orderLineItemOrderId = orderId
                        , orderLineItemType = StoreCreditLine
                        , orderLineItemDescription = "Store Credit"
                        , orderLineItemAmount = storeCredit
                        }
                    return (orderEntity, totalInCents - storeCredit, storeCredit)
                else
                    return (orderEntity, totalInCents, 0)


-- | Delete a Cart & all it's Items.
deleteCart :: CartId -> AppSQL ()
deleteCart cartId =
    deleteWhere [CartItemCartId ==. cartId] >> delete cartId

-- | Create the Shipping, Surcharge, & CouponDiscount OrderLineItems for an
-- Order, returning the total amount of all items & the ID of a Coupon(if
-- applied).
--
-- Note that this returns an Integer instead of Cents for the lineItem
-- total since the credit lines may be larger than the charge lines.
--
-- May throw a PlaceOrderError exception.
createLineItems
    :: UTCTime
    -- ^ Current Time
    -> CustomerId
    -- ^ Customer Id
    -> Entity Address
    -- ^ Shipping Address
    -> [CartItemData]
    -- ^ Cart Items
    -> OrderId
    -- ^ Order ID to add Line Items to
    -> Bool
    -- ^ Use Priority Shipping
    -> Maybe T.Text
    -- ^ Coupon Code
    -> AppSQL (Integer, Maybe CouponId)
createLineItems currentTime customerId shippingAddress items orderId priorityShipping maybeCouponCode = do
    maybeCoupon <- mapException PlaceOrderCouponError $ sequence
        $ getCoupon currentTime (Just customerId) <$> maybeCouponCode
    charges <- getCharges (Just $ toAddressData shippingAddress) items
        maybeCoupon priorityShipping False
    mapException PlaceOrderCouponError $ checkCouponMeetsMinimum maybeCoupon charges
    shippingCharge <-
        case ccShippingMethods charges of
            [] ->
                throwM NoShippingMethod
            charge : _ ->
                return charge
    shippingLine <- insertEntity . makeLine ShippingLine $ scCharge shippingCharge
    surcharges <- mapM (insertEntity . makeLine SurchargeLine) $ ccSurcharges charges
    maybePriorityShippingCharge <-
        maybe (return Nothing) (fmap Just . insertEntity . makeLine PriorityShippingLine)
            $ ccPriorityShippingFee charges
    maybeCouponDiscount <-
        maybe (return Nothing) (fmap Just . insertEntity . makeLine CouponDiscountLine)
            $ ccCouponDiscount charges
    maybeTaxLine <-
        if ccAmount (ccTax charges) > 0 then
            fmap Just . insertEntity . makeLine TaxLine $ ccTax charges
        else
            return Nothing
    return
        ( sum (map (centsToInteger . orderLineItemAmount . entityVal) surcharges)
            + centsToInteger (orderLineItemAmount $ entityVal shippingLine)
            + maybeLineAmount maybePriorityShippingCharge
            + maybeLineAmount maybeTaxLine
            - maybeLineAmount maybeCouponDiscount
        , entityKey <$> maybeCoupon
        )
    where
        centsToInteger :: Cents -> Integer
        centsToInteger =
            fromIntegral . fromCents
        makeLine :: LineItemType -> CartCharge -> OrderLineItem
        makeLine lineType charge =
            OrderLineItem
                { orderLineItemOrderId = orderId
                , orderLineItemType = lineType
                , orderLineItemDescription = ccDescription charge
                , orderLineItemAmount = ccAmount charge
                }
        maybeLineAmount :: Maybe (Entity OrderLineItem) -> Integer
        maybeLineAmount =
            maybe 0 (centsToInteger . orderLineItemAmount . entityVal)

createProducts :: [CartItemData] -> OrderId -> AppSQL [Cents]
createProducts items orderId =
    mapM (fmap calculateTotal <$> insertEntity . makeProduct) items
    where calculateTotal (Entity _ prod) =
            orderProductPrice prod * (Cents $ orderProductQuantity prod)
          makeProduct CartItemData { cidVariant, cidQuantity } =
            OrderProduct
                { orderProductOrderId = orderId
                , orderProductProductVariantId = vdId cidVariant
                , orderProductQuantity = cidQuantity
                , orderProductPrice = getVariantPrice cidVariant
                }


-- SUCCESS DETAILS
-- TODO: Move to Customers Module or make Orders Module
-- (used for My Account Order Details page as well)


newtype SuccessParameters =
    SuccessParameters
        { cspOrderId :: Int64 }

instance FromJSON SuccessParameters where
    parseJSON = withObject "SuccessParameters" $ \v ->
        SuccessParameters
            <$> v .: "orderId"

type SuccessRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] SuccessParameters
    :> Post '[JSON] (Cookied OrderDetails)

customerSuccessRoute :: WrappedAuthToken -> SuccessParameters -> App (Cookied OrderDetails)
customerSuccessRoute token parameters = withValidatedCookie token $ \(Entity customerId _) ->
    eitherM handleError <=< try . runDB $ do
        (e@(Entity orderId _), shipping, billing) <-
            getOrderAndAddress customerId (E.toSqlKey $ cspOrderId parameters)
                >>= maybe (throwM OrderNotFound) return
        lineItems <-
            selectList [OrderLineItemOrderId ==. orderId] []
        products <- getCheckoutProducts orderId
        return OrderDetails
            { odOrder = toCheckoutOrder e
            , odLineItems = lineItems
            , odProducts = products
            , odShippingAddress = toAddressData shipping
            , odBillingAddress = toAddressData <$> billing
            }
    where handleError = \case
            OrderNotFound ->
                serverError err404

data SuccessError
    = OrderNotFound
    deriving (Show, Typeable)

instance Exception SuccessError


getOrderAndAddress :: CustomerId -> OrderId -> AppSQL (Maybe (Entity Order, Entity Address, Maybe (Entity Address)))
getOrderAndAddress customerId orderId =
    fmap listToMaybe . E.select . E.from
        $ \(o `E.InnerJoin` s `E.LeftOuterJoin` b) -> do
            E.on $ o E.^. OrderBillingAddressId E.==. b E.?. AddressId
            E.on $ o E.^. OrderShippingAddressId E.==. s E.^. AddressId
            E.where_ $
                o E.^. OrderId E.==. E.val orderId E.&&.
                o E.^. OrderCustomerId E.==. E.val customerId
            return (o, s, b)


-- Utils


eitherM :: Monad m => (b -> m a) -> Either b a -> m a
eitherM handler =
    either handler return


mapException
    :: (Exception e1, Exception e2, MonadThrow m, MonadCatch m)
    => (e1 -> e2) -> m a -> m a
mapException transform = try >=> eitherM (throwM . transform)
