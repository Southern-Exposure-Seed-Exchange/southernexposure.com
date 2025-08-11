{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wno-deprecations #-}
module Routes.Checkout
    ( CheckoutAPI
    , checkoutRoutes
    ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import UnliftIO (MonadUnliftIO, MonadIO)
import UnliftIO.Exception (Exception, throwIO, try, handle)
import Control.Monad ((>=>), when, unless, void, forM_, guard)
import Control.Monad.Reader (asks, lift, liftIO)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.List (partition, find)
import Data.Maybe (listToMaybe, isJust, isNothing, fromMaybe, mapMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), (=.), (-=.), (+=.), Entity(..), insert, insert_, insertEntity, get
    , getBy, selectList, update, updateWhere, delete, deleteWhere, count
    , getEntity, selectFirst
    )
import Network.Socket (SockAddr(..))
import Servant ((:<|>)(..), (:>), AuthProtect, Header, JSON, Post, RemoteHost, ReqBody, err404, ServerT)

import Auth
import Avalara
    ( CommitTransactionRequest(..), VoidTransactionRequest(..), VoidReason(..)
    , CustomerCode
    )
import Config
import Helcim (createVerifyCheckout, getCustomer, getCustomers, purchase, updateCustomer)
import Helcim.API (HelcimError(..), ApiError(..))
import Helcim.API.Types.Checkout (CheckoutToken(..), CheckoutCreateResponse (..))
import Helcim.API.Types.Common (CardToken(..))
import Helcim.API.Types.Customer (CustomerResponse(..), CreateCustomerRequest(..), GetCustomersRequest(..))
import qualified Helcim.API.Types.Customer as Helcim
import Helcim.API.Types.Payment (CardData(..), TransactionResponse(..), PurchaseRequest(..), Status(..))
import Helcim.Utils (addressToHelcimAddress)
import Models
import Models.Fields
import Server
import Routes.CommonData
    ( CartItemData(..), CartCharges(..)
    , CartCharge(..), getCartItems, getCharges, AddressData(..), toAddressData
    , fromAddressData, ShippingCharge(..), VariantData(..), getVariantPrice
    , OrderDetails(..), toCheckoutOrder, getCheckoutProducts, CheckoutProduct, toDeliveryData

    , BaseProductData(..)
    )
import Routes.AvalaraUtils (createAvalaraTransaction, createAvalaraCustomer)
import Routes.Utils (getDisabledCheckoutDetails, getClientIP, generateUniqueToken)
import Routes.Carts (ValidateCartParameters(..))
import Routes.Customers (RegistrationParametersWith(..), registerNewCustomer)
import Validation (Validation(..))
import Workers (Task(..), AvalaraTask(..), enqueueTask)

import qualified Avalara
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto.Experimental as E
import qualified Emails
import qualified Validation as V
import qualified Data.UUID as UUID
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )


type CheckoutAPI =
         "customer-details" :> CustomerDetailsRoute
    :<|> "customer-place-order" :> CustomerPlaceOrderRoute
    :<|> "anonymous-details" :> AnonymousDetailsRoute
    :<|> "anonymous-place-order" :> AnonymousPlaceOrderRoute
    :<|> "success" :> SuccessRoute
    :<|> "anonymous-success" :> AnonymousSuccessRoute
    :<|> "helcim-checkout-token" :> CheckoutTokenRoute
    :<|> "anonymous-helcim-checkout-token" :> AnonymousCheckoutTokenRoute

type CheckoutRoutes = ServerT CheckoutAPI App

checkoutRoutes :: CheckoutRoutes
checkoutRoutes =
         customerDetailsRoute
    :<|> customerPlaceOrderRoute
    :<|> anonymousDetailsRoute
    :<|> anonymousPlaceOrderRoute
    :<|> customerSuccessRoute
    :<|> anonymousSuccessRoute
    :<|> getCheckoutTokenRoute
    :<|> anonymousGetCheckoutTokenRoute


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

data PrioritySHError
    = PriorityShippingNotAvailable
    deriving (Show, Typeable)

instance Exception PrioritySHError

handlePriorityShippingErrors :: PrioritySHError -> App a
handlePriorityShippingErrors = V.singleFieldError "priority-shipping" . \case
    PriorityShippingNotAvailable ->
        "Sorry, priority shipping & handling is not available with the items "
            <> "currently in your cart."

-- SHIPPING RESTRICTION ERROR

data ShippingRestrictionError
    = CannotShipTo Region [BaseProductData]
    deriving (Show, Typeable)

instance Exception ShippingRestrictionError

renderShippingRestrictionError :: ShippingRestrictionError -> T.Text
renderShippingRestrictionError = \case
    CannotShipTo region products ->
        let names = T.intercalate ", " $ map bpdName products
        in
            "Sorry we can not ship the following products to "
            <> regionName region
            <> ": "
            <> names
            <> "."

handleShippingRestrictionError :: ShippingRestrictionError -> App a
handleShippingRestrictionError = V.singleError . renderShippingRestrictionError


-- CART/ADDRESS DETAILS


data CheckoutDetailsError
    = DetailsPriorityError PrioritySHError
    | DetailsCouponError CouponError
    deriving (Show, Typeable)

instance Exception CheckoutDetailsError

withCheckoutDetailsErrors :: App a -> App a
withCheckoutDetailsErrors =
    handle $ \case
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
        , cddIsDisabled :: Bool
        , cddDisabledMessage :: T.Text
        , cddRestrictionsError :: Maybe T.Text
        }

instance ToJSON CheckoutDetailsData where
    toJSON details =
        object
            [ "shippingAddresses" .= cddShippingAddresses details
            , "billingAddresses" .= cddBillingAddresses details
            , "items" .= cddItems details
            , "charges" .= cddCharges details
            , "storeCredit" .= cddStoreCredit details
            , "disabled" .= cddIsDisabled details
            , "disabledMessage" .= cddDisabledMessage details
            , "restrictionsError" .= cddRestrictionsError details
            ]

type CustomerDetailsRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerDetailsParameters
    :> Post '[JSON] (Cookied CheckoutDetailsData)

customerDetailsRoute :: WrappedAuthToken -> CustomerDetailsParameters -> App (Cookied CheckoutDetailsData)
customerDetailsRoute token parameters = withValidatedCookie token $ \customer ->
    withCheckoutDetailsErrors . runDB $ getCustomerDetails customer parameters

getCustomerDetails :: Entity Customer -> CustomerDetailsParameters -> AppSQL CheckoutDetailsData
getCustomerDetails (Entity customerId customer) parameters = do
    let priorityShipping =
            cdpPriorityShipping parameters
    (isDisabled, disabledMsg) <- lift getDisabledCheckoutDetails
    currentTime <- liftIO getCurrentTime
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
    let avalaraCode = fromAvalaraCustomerCode <$> customerAvalaraCode customer
    (restrictions, charges) <- detailsCharges maybeShipping avalaraCode
        items maybeCoupon priorityShipping
    return CheckoutDetailsData
        { cddShippingAddresses = map toAddressData shippingAddresses
        , cddBillingAddresses = map toAddressData billingAddresses
        , cddItems = items
        , cddCharges = charges
        , cddStoreCredit = customerStoreCredit customer
        , cddIsDisabled = isDisabled
        , cddDisabledMessage = disabledMsg
        , cddRestrictionsError = restrictions
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
            (isDisabled, disabledMsg) <- lift getDisabledCheckoutDetails
            currentTime <- liftIO getCurrentTime
            maybeCoupon <- mapException DetailsCouponError $ sequence
                $ getCoupon currentTime Nothing <$> adpCouponCode parameters
            items <- getCartItems $ \c ->
                c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
            (restrictions, charges) <- detailsCharges maybeAddress Nothing items maybeCoupon priorityShipping
            return CheckoutDetailsData
                { cddShippingAddresses = []
                , cddBillingAddresses = []
                , cddItems = items
                , cddCharges = charges
                , cddStoreCredit = mkCents 0
                , cddIsDisabled = isDisabled
                , cddDisabledMessage = disabledMsg
                , cddRestrictionsError = restrictions
                }


-- | Get the Coupon from a code, if it is valid. Does not validate
-- the minimum order size, since the CartCharges are needed for
-- that. Throws CouponError.
getCoupon :: UTCTime -> Maybe CustomerId -> T.Text -> AppSQL (Entity Coupon)
getCoupon currentTime maybeCustomerId couponCode = do
    maybeCoupon <- getBy $ UniqueCoupon couponCode
    case maybeCoupon of
        Nothing ->
            throwIO CouponNotFound
        Just e@(Entity couponId coupon) -> do
            unless (couponIsActive coupon)
                $ throwIO CouponInactive
            when (currentTime > couponExpirationDate coupon)
                $ throwIO CouponExpired
            when (couponTotalUses coupon /= 0) $ do
                totalUses <- count [OrderCouponId ==. Just couponId]
                when (totalUses >= fromIntegral (couponTotalUses coupon))
                    $ throwIO CouponMaxUses
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
                        $ throwIO CouponCustomerMaxUses
                Nothing -> return ()

-- | Get the CartCharges & a potential ShippingRestrictionErrorfor the
-- details routes & check the coupon minimum & priority shipping
-- availability.
--
-- Throws 'CheckoutDetailsError'.
detailsCharges :: Maybe AddressData -> Maybe CustomerCode -> [CartItemData] -> Maybe (Entity Coupon) -> Bool
    -> AppSQL (Maybe T.Text, CartCharges)
detailsCharges maybeShipping avalaraCode items maybeCoupon priorityShipping = do
    restrictions <- try $ checkProductRestrictions maybeShipping items
    charges <- getCharges maybeShipping avalaraCode items maybeCoupon priorityShipping True
    mapException DetailsCouponError
        $ checkCouponMeetsMinimum maybeCoupon charges
    mapException DetailsPriorityError
        $ checkPriorityShippingAvailable priorityShipping charges
    return
        ( either (Just . renderShippingRestrictionError) (const Nothing) restrictions
        , charges)

-- | Ensure that all of the Products are allowed to be shipped to the state.
--
-- Throws a ShippingRestrictionError when some products are not allowed to
-- be shipped to the given shipping address.
checkProductRestrictions :: MonadIO m => Maybe AddressData -> [CartItemData] -> m ()
checkProductRestrictions maybeShipping items =
    forM_ maybeShipping $ \shipping -> do
        let restrictedProducts = flip mapMaybe items $ \CartItemData { cidProduct } ->
                if adState shipping `elem` bpdShippingRestrictions cidProduct then
                    Just cidProduct
                else
                    Nothing
        unless (null restrictedProducts) $
            throwIO $ CannotShipTo (adState shipping) restrictedProducts

-- | Ensure the product total equals or exceeds the Coupon
-- minimum by seeing if it was removed from the CartCharges. If
-- it does not meet the minimum, throw a CouponError.
checkCouponMeetsMinimum :: MonadIO m => Maybe (Entity Coupon) -> CartCharges -> m ()
checkCouponMeetsMinimum maybeCoupon CartCharges { ccCouponDiscount } =
    case (maybeCoupon, ccCouponDiscount) of
        (Just (Entity _ coupon), Nothing) ->
            throwIO $ CouponBelowOrderMinimum $ couponMinimumOrder coupon
        _ ->
            return ()

-- | Throw a PriorityShippingNotAvailable error if priority shipping is
-- selected & the shipping method does not have priority shipping
-- available.
--
-- Does nothing if no shipping methods are present.
checkPriorityShippingAvailable :: MonadIO m => Bool -> CartCharges -> m ()
checkPriorityShippingAvailable hasPriority charges =
    when hasPriority $
        case ccShippingMethods charges of
            ShippingCharge _ Nothing _:_ ->
                throwIO PriorityShippingNotAvailable
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
    | CardChargeError
    | PlaceOrderCouponError CouponError
    | AvalaraNoTransactionCode
    | PlaceOrderCannotShip ShippingRestrictionError
    | NoHelcimCustomer
    | HelcimError HelcimError
    | HelcimPaymentDeclined
    deriving (Show, Typeable)

instance Exception PlaceOrderError

withPlaceOrderErrors :: CustomerAddress -> App a -> App a
withPlaceOrderErrors shippingAddress =
    handle $ \case
        CartNotFound ->
            V.singleError
                $ "We couldn't find any items in your Cart. This usually means "
                <> "that your Order has been successfully submitted but we "
                <> "encountered an error while sending your confirmation email, please "
                <> "check your Order History to verify and contact us if needed."
        NoShippingMethod ->
            case shippingAddress of
                ExistingAddress _ _ ->
                    V.singleFieldError "shipping-" "Sorry, we only ship to the United States."
                NewAddress _ ->
                    V.singleFieldError "shipping-country" "Sorry, we only ship to the United States."
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
        PlaceOrderCannotShip shippingError ->
            handleShippingRestrictionError shippingError
        NoHelcimCustomer ->
            V.singleError
                $ "An error occured while processing your payment details. Please try again or "
                <> "contact us for help."
        HelcimError helcimError -> case helcimError of
            HelcimClientError _ ->
                V.singleError
                    $ "An error occured while processing your payment. Please try again or "
                    <> "contact us for help."
            HelcimApiError ApiError { errors } ->
                V.singleError
                    $ "An error occured while processing your payment: "
                    <> T.intercalate ", " (map (T.pack . show) errors)
        HelcimPaymentDeclined ->
            V.singleError "Your payment was declined. Please try again or contact us for help."



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
        , cpopHelcimData :: Maybe (CardToken, Helcim.CustomerCode)
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
            <*> do
                v .:? "helcimToken" >>= maybe
                    (return Nothing)
                    (\cardToken -> do
                        customerCode <- v .: "helcimCustomerCode"
                        return $ Just (cardToken, customerCode))

instance Validation CustomerPlaceOrderParameters where
    validators parameters = do
        shippingValidators <- validateAddress $ cpopShippingAddress parameters
        billingValidators <- maybeValidate validateAddress $ cpopBillingAddress parameters
        checkoutDisabled <- fst <$> getDisabledCheckoutDetails
        return $
            ( "", [ ( "There was an error processing your payment, please try again."
                    , whenJust T.null $ unCardToken . fst <$> cpopHelcimData parameters )
                  , ( "Sorry, we have temporarily stopped accepting Orders. "
                        <> "Please check our Homepage for updates."
                    , checkoutDisabled
                    )
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
    RemoteHost
    :> Header "X-Forwarded-For" T.Text
    :> Header "X-Real-IP" T.Text
    :> AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerPlaceOrderParameters
    :> Post '[JSON] (Cookied PlaceOrderData)


-- | Place an Order using a Customer's Cart.
--
-- If the Customer has a StripeId, add a new Card & set it as their
-- Default. Otherwise create a new Stripe Customer for them. Then charge
-- the Stripe Customer.
customerPlaceOrderRoute :: SockAddr -> Maybe T.Text -> Maybe T.Text -> WrappedAuthToken -> CustomerPlaceOrderParameters -> App (Cookied PlaceOrderData)
customerPlaceOrderRoute remoteHost forwardedFor realIP = validateCookieAndParameters $ \ce@(Entity customerId customer) parameters -> do
    let shippingParameter = cpopShippingAddress parameters
        maybeHelcimData = cpopHelcimData parameters
    currentTime <- liftIO getCurrentTime
    orderId <- withPlaceOrderErrors shippingParameter . runDB $ do
        (maybeBillingAddress, shippingAddress, order@(Entity orderId _), preTaxTotal, cartId, appliedCredit) <-
            createAddressesAndOrder ce shippingParameter (cpopBillingAddress parameters)
                (cpopStoreCredit parameters) (cpopPriorityShipping parameters)
                (cpopCouponCode parameters) (cpopComment parameters) currentTime
        when (appliedCredit > customerStoreCredit customer) $
            throwIO NotEnoughStoreCredit
        when (appliedCredit > mkCents 0) $
            update customerId [CustomerStoreCredit -=. appliedCredit]
        let remainingCredit = maybe (mkCents 0) (`subtractCents` appliedCredit) $ cpopStoreCredit parameters
        when (preTaxTotal > mkCents 0 || preTaxTotal `plusCents` appliedCredit > mkCents 0 ) $
            withAvalaraTransaction order preTaxTotal remainingCredit ce shippingAddress maybeBillingAddress $ \orderTotal ->
                when (orderTotal >= mkCents 50) $ case (maybeBillingAddress, maybeHelcimData) of
                    (Just (Entity _ billingAddress), Just (helcimToken, customerCode)) -> do
                        getAndUpdateHelcimCustomerByCode customerId customer customerCode billingAddress (entityVal shippingAddress)
                        helcimCharge helcimToken customerCode (getClientIP remoteHost forwardedFor realIP) orderId orderTotal
                    (Nothing, _) ->
                        throwIO BillingAddressRequired
                    (_, Nothing) ->
                        throwIO StripeTokenRequired
        deleteCart cartId
        reduceQuantities orderId
        return orderId
    runDB $ enqueuePostOrderTasks orderId
    (orderLines, products) <- runDB $ (,)
        <$> selectList [OrderLineItemOrderId ==. orderId] []
        <*> getCheckoutProducts orderId
    return $ PlaceOrderData
        { podOrderId = orderId
        , podLines = orderLines
        , podProducts = products
        }
    where
          createAddressesAndOrder
            :: Entity Customer -> CustomerAddress -> Maybe CustomerAddress
            -> Maybe Cents -> Bool -> Maybe T.Text -> T.Text -> UTCTime
            -> AppSQL (Maybe (Entity Address), Entity Address, Entity Order, Cents, CartId, Cents)
          createAddressesAndOrder customer@(Entity customerId _) shippingParam billingParam maybeStoreCredit priorityShipping maybeCouponCode comment currentTime = do
            shippingAddress <- getOrInsertAddress Shipping customerId shippingParam
            billingAddress <- sequence $ getOrInsertAddress Billing customerId <$> billingParam
            (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
                >>= maybe (throwIO CartNotFound) return
            (order, orderTotal, appliedCredit) <- createOrder
                customer cartId shippingAddress (entityKey <$> billingAddress)
                maybeStoreCredit priorityShipping maybeCouponCode Nothing comment currentTime
            return (billingAddress, shippingAddress, order, orderTotal, cartId, appliedCredit)
          getOrInsertAddress :: AddressType -> CustomerId -> CustomerAddress -> AppSQL (Entity Address)
          getOrInsertAddress addrType customerId = \case
            ExistingAddress addrId makeDefault -> do
                let noAddress = throwIO $ AddressNotFound addrType
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


data AnonymousPlaceOrderParameters =
    AnonymousPlaceOrderParameters
        { apopEmail :: T.Text
        , apopShippingAddress :: AddressData
        , apopBillingAddress :: Maybe AddressData
        , apopPriorityShipping :: Bool
        , apopCouponCode :: Maybe T.Text
        , apopComment :: T.Text
        , apopCartToken :: T.Text
        , apopHelcimData :: Maybe (CardToken, Helcim.CustomerCode)
        }

instance FromJSON AnonymousPlaceOrderParameters where
    parseJSON = withObject "AnonymousPlaceOrderParameters" $ \v ->
        AnonymousPlaceOrderParameters
            <$> v .: "email"
            <*> v .: "shippingAddress"
            <*> v .: "billingAddress"
            <*> v .: "priorityShipping"
            <*> v .:? "couponCode"
            <*> v .: "comment"
            <*> v .: "sessionToken"
            <*> do
                v .:? "helcimToken" >>= maybe
                    (return Nothing)
                    (\cardToken -> do
                        customerCode <- v .: "helcimCustomerCode"
                        return $ Just (cardToken, customerCode))

instance Validation AnonymousPlaceOrderParameters where
    validators parameters = do
        shippingValidators <- validators $ apopShippingAddress parameters
        billingValidators <- maybe (return []) validators
            $ apopBillingAddress parameters
        checkoutDisabled <- fst <$> getDisabledCheckoutDetails
        return $
            [ ( "email"
              , [ V.required $ apopEmail parameters
                ]
              )
            , ( ""
              , [ ( "Sorry, we have temporarily stopped accepting Orders. "
                        <> "Please check our Homepage for updates."
                  , checkoutDisabled
                  )
                ]
              )
            ]
            ++ map (first $ T.append "shipping-") shippingValidators
            ++ map (first $ T.append "billing-") billingValidators

data AnonymousPlaceOrderData =
    AnonymousPlaceOrderData
        { apodOrderId :: OrderId
        , apodGuestToken :: T.Text
        , apodLines :: [Entity OrderLineItem]
        , apodProducts :: [CheckoutProduct]
        }

instance ToJSON AnonymousPlaceOrderData where
    toJSON orderData =
        object
            [ "orderId" .= apodOrderId orderData
            , "guestToken" .= apodGuestToken orderData
            , "lines" .= apodLines orderData
            , "products" .= apodProducts orderData
            ]

type AnonymousPlaceOrderRoute =
       RemoteHost
    :> Header "X-Forwarded-For" T.Text
    :> Header "X-Real-IP" T.Text
    :> ReqBody '[JSON] AnonymousPlaceOrderParameters
    :> Post '[JSON] AnonymousPlaceOrderData

-- | Place an Order using an Anonymous Cart.
--
-- A new Customer & Order is created & the Customer is charged.
anonymousPlaceOrderRoute :: SockAddr -> Maybe T.Text -> Maybe T.Text -> AnonymousPlaceOrderParameters -> App AnonymousPlaceOrderData
anonymousPlaceOrderRoute remoteHost forwardedFor realIP = validate >=> \parameters -> do
    let shippingParam = apopShippingAddress parameters
        billingParam = apopBillingAddress parameters
    currentTime <- liftIO getCurrentTime
    (orderId, orderData) <- withPlaceOrderErrors (NewAddress shippingParam) . runDB $ do
        Entity customerId customer <- createAnonymousCustomer parameters
        (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
            >>= maybe (throwIO CartNotFound) return
        let shippingAddress = fromAddressData Shipping customerId
                shippingParam
            maybeBillingAddress = fromAddressData Billing customerId
                <$> billingParam

        -- When customer with the same email makes multiple "anonymous" orders, we should
        -- mark all previous addresses (shipping and billing) as non-default.
        updateWhere [ AddressCustomerId ==. customerId, AddressType ==. Shipping ] [ AddressIsDefault =. False ]
        when (isJust maybeBillingAddress) $ updateWhere [ AddressCustomerId ==. customerId, AddressType ==. Billing ] [ AddressIsDefault =. False ]

        shippingId <- insert shippingAddress
        billingId <- mapM insert maybeBillingAddress
        let shippingEntity = Entity shippingId shippingAddress
            billingEntity = Entity <$> billingId <*> maybeBillingAddress
            customerEntity = Entity customerId customer
        guestToken <- generateUniqueToken (UniqueGuestToken . Just)
        (order@(Entity orderId _), preTaxTotal, _) <- createOrder
            (Entity customerId customer) cartId shippingEntity billingId Nothing
            (apopPriorityShipping parameters) (apopCouponCode parameters)
            (Just guestToken) (apopComment parameters) currentTime
        when (preTaxTotal > mkCents 0)
            $ withAvalaraTransaction order preTaxTotal (mkCents 0) customerEntity shippingEntity billingEntity
            $ \orderTotal ->
                case (billingEntity, apopHelcimData parameters) of
                    (Just (Entity _ billingAddress), Just (cardToken, customerCode)) -> do
                        getAndUpdateHelcimCustomerByCode customerId customer customerCode billingAddress shippingAddress
                        helcimCharge cardToken customerCode (getClientIP remoteHost forwardedFor realIP) orderId orderTotal
                    (Nothing, _) ->
                        throwIO BillingAddressRequired
                    (_, Nothing) ->
                        throwIO StripeTokenRequired
        deleteCart cartId
        reduceQuantities orderId
        orderLines <- selectList [OrderLineItemOrderId ==. orderId] []
        products <- getCheckoutProducts orderId
        return
            ( orderId
            , AnonymousPlaceOrderData
                { apodOrderId = orderId
                , apodGuestToken = guestToken
                , apodLines = orderLines
                , apodProducts = products
                }
            )
    runDB $ enqueuePostOrderTasks orderId
    pure orderData
    where
        createAnonymousCustomer parameters = do
            let email = apopEmail parameters
                shippingParam = apopShippingAddress parameters
                billingParam = apopBillingAddress parameters
                makeAvalaraCustomer = createAvalaraCustomer email
                    $ fromMaybe shippingParam billingParam

            (Entity cId c) <- lift $ registerNewCustomer RegistrationParameters
                    { rpEmail = email
                    , rpPassword = Nothing
                    , rpCartToken = Nothing
                    }
            overwriteCustomerCart cId . Just $ apopCartToken parameters
            avalaraCode <- case customerAvalaraCode c of
                Just avalaraCode -> pure (Just avalaraCode)
                Nothing -> do
                    avalaraCustomerCode <- makeAvalaraCustomer
                    update cId [CustomerAvalaraCode =. avalaraCustomerCode]
                    return avalaraCustomerCode
            pure $ Entity cId c { customerAvalaraCode = avalaraCode
                                }

-- | Get a Helcim Customer by their CustomerCode.
-- And update their billing & shipping addresses with the current order's values. Map the Helcim Customer id
-- with the Customer in the database.
getAndUpdateHelcimCustomerByCode :: CustomerId -> Customer -> Helcim.CustomerCode -> Address -> Address -> AppSQL ()
getAndUpdateHelcimCustomerByCode customerId customer helcimCustomerCode billingAddress shippingAddress = do
    mbHelcimCustomerId <- lift $ getCustomers GetCustomersRequest
        { gcrSearch = Nothing
        , gcrCustomerCode = Just helcimCustomerCode
        , gcrLimit = Just 1
        , gcrPage = Just 1
        , gcrIncludeCards = Nothing
        } >>= either (throwIO . HelcimError) (return . fmap creId . listToMaybe)
    case mbHelcimCustomerId of
        Just helcimCustomerId -> do
            -- Update Helcim Customer billing & shipping addresses with values from the current order
            updateHelcimCustomer helcimCustomerId customer billingAddress shippingAddress
            update customerId [CustomerHelcimCustomerId =. Just helcimCustomerId]
        -- Something went wrong
        -- CustomerCode is obtained from Helcim response, so it should exist
        Nothing -> throwIO NoHelcimCustomer

updateHelcimCustomer :: Helcim.CustomerId -> Customer -> Address -> Address -> AppSQL ()
updateHelcimCustomer helcimCustomerId customer billingAddress shippingAddress = do
    -- Update Helcim Customer billing & shipping addresses with provided values
    void $ lift $ (updateCustomer helcimCustomerId $ CreateCustomerRequest
        { ccrCustomerCode = Nothing
        , ccrContactName = Just (customerEmail customer)
        , ccrBusinessName = Just $ addressCompanyName billingAddress
        , ccrCellPhone = Nothing
        , ccrBillingAddress = Just $ addressToHelcimAddress billingAddress
        , ccrShippingAddress = Just $ addressToHelcimAddress shippingAddress
        }) >>= either (throwIO . HelcimError) return

-- | Commit an uncommited Avalara Sales Tax Transaction. Throws
-- a 'PlaceOrderError' on failure.
commitAvalaraTransaction :: Avalara.Transaction -> AppSQL (Maybe ())
commitAvalaraTransaction transaction = do
    companyCode <- lift $ asks getAvalaraCompanyCode
    transactionCode <- case Avalara.tCode transaction of
        Just tCode ->
            return tCode
        Nothing ->
            throwIO AvalaraNoTransactionCode
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
            throwIO AvalaraNoTransactionCode
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
                            throwIO err
                Just taxTransaction -> do
                    let taxTotal =
                            if status == AvalaraEnabled then
                                maybe (mkCents 0) fromDollars (Avalara.tTotalTax taxTransaction)
                            else
                                mkCents 0
                        appliedCredit = min storeCredit taxTotal
                        orderTotal = taxTotal `plusCents` preTaxTotal `subtractCents` appliedCredit
                    when (appliedCredit > customerStoreCredit customer) $
                        throwIO NotEnoughStoreCredit
                    when (appliedCredit > mkCents 0) $
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
                            when (status == AvalaraEnabled && taxTotal > mkCents 0) $
                                insert_ OrderLineItem
                                    { orderLineItemOrderId = orderId
                                    , orderLineItemType = TaxLine
                                    , orderLineItemAmount = taxTotal
                                    , orderLineItemDescription = "Sales Tax"
                                    }
                            when (status == AvalaraTesting) $
                                voidOrEnqueueTransaction taxTransaction
                            when (appliedCredit > mkCents 0) $
                                updateStoreCredit appliedCredit
                        Left (err :: PlaceOrderError) -> do
                            voidOrEnqueueTransaction taxTransaction
                            throwIO err
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


helcimCharge :: CardToken -> Helcim.CustomerCode -> T.Text -> OrderId -> Cents -> AppSQL ()
helcimCharge cardToken customerCode ipAddress orderId orderTotal = do
    purchaseResult <- lift $ Helcim.purchase PurchaseRequest
        { prqIpAddress = ipAddress
        , prqEcommerce = Nothing
        , prqTerminalId = Nothing
        , prqCurrency = "USD"
        , prqAmount = toDollars orderTotal
        , prqCustomerCode = Just customerCode
        , prqInvoiceNumber = Nothing
        , prqBillingAddress = Nothing
        , prqInvoice = Nothing
        , prqCardData = CardData { cdCardToken = cardToken }
        }
    case purchaseResult of
        Left err ->
            update orderId [OrderStatus =. PaymentFailed]
                >> throwIO (HelcimError err)
        Right purchaseResponse ->
            case trStatus purchaseResponse of
                Declined ->
                    update orderId [OrderStatus =. PaymentFailed]
                        >> throwIO HelcimPaymentDeclined
                Approved -> do
                    let helcimTransactionId = trTransactionId purchaseResponse
                        helcimCardNumber = trCardNumber purchaseResponse
                        helcimCardType = trCardType purchaseResponse
                    update orderId
                        [ OrderStatus =. PaymentReceived
                        , OrderHelcimTransactionId =. Just helcimTransactionId
                        , OrderHelcimCardNumber =. helcimCardNumber
                        , OrderHelcimCardType =. Just helcimCardType
                        ]

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
    -> Maybe T.Text
    -- ^ Optional guest token (must be unique)
    -> T.Text
    -- ^ Order Comment
    -> UTCTime
    -- ^ Current Time
    -> AppSQL (Entity Order, Cents, Cents)
createOrder ce@(Entity customerId customer) cartId shippingEntity
            billingId maybeStoreCredit priorityShipping maybeCouponCode
            maybeGuestToken comment currentTime = do
    items <- getCartItems $ \c -> c E.^. CartId E.==. E.val cartId
    let order = Order
            { orderCustomerId = customerId
            , orderStatus = OrderReceived
            , orderStoneEdgeStatus = Nothing
            , orderBillingAddressId = billingId
            , orderShippingAddressId = entityKey shippingEntity
            , orderGuestToken = maybeGuestToken
            , orderCustomerComment = comment
            , orderAdminComments = []
            , orderAvalaraTransactionCode = Nothing
            , orderStripeChargeId = Nothing
            , orderStripeLastFour = Nothing
            , orderStripeIssuer = Nothing
            , orderHelcimTransactionId = Nothing
            , orderHelcimCardNumber = Nothing
            , orderHelcimCardType = Nothing
            , orderCouponId = Nothing
            , orderCreatedAt = currentTime
            }
    let validateCartParameters = ValidateCartParameters cartId $
            M.fromList $ map (\CartItemData{..} -> (E.fromSqlKey cidItemId, fromIntegral cidQuantity)) items
    lift (validate validateCartParameters) >> do
        orderId <- insert order
        (lineTotal, maybeCouponId) <- createLineItems currentTime ce
            shippingEntity items orderId priorityShipping maybeCouponCode
        when (isJust maybeCouponId) $
            update orderId [OrderCouponId =. maybeCouponId]
        productTotals <- createProducts items orderId
        let totalCharges = lineTotal + fromIntegral (fromCents $ sumPrices productTotals)
            orderEntity = Entity orderId order
        if totalCharges < 0 then
            -- TODO: Throw an error? This means credits > charges which shouldn't happen...
            return (orderEntity, mkCents 0, mkCents 0)
        else
            let totalInCents = Cents $ fromIntegral totalCharges in
            case maybeStoreCredit of
                Nothing ->
                    return (orderEntity, totalInCents, mkCents 0)
                Just c ->
                    if c > mkCents 0 && totalInCents > mkCents 0 then do
                        let storeCredit = min (customerStoreCredit customer) $ min totalInCents c
                        insert_ OrderLineItem
                            { orderLineItemOrderId = orderId
                            , orderLineItemType = StoreCreditLine
                            , orderLineItemDescription = "Store Credit"
                            , orderLineItemAmount = storeCredit
                            }
                        return (orderEntity, totalInCents `subtractCents` storeCredit, storeCredit)
                    else
                        return (orderEntity, totalInCents, mkCents 0)


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
    -> Entity Customer
    -- ^ Customer placing the Order
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
createLineItems currentTime (Entity customerId customer) shippingAddress items orderId priorityShipping maybeCouponCode = do
    let shippingData = Just $ toAddressData shippingAddress
    mapException PlaceOrderCannotShip
        $ checkProductRestrictions shippingData items
    maybeCoupon <- mapException PlaceOrderCouponError $ sequence
        $ getCoupon currentTime (Just customerId) <$> maybeCouponCode
    let avalaraCode = fromAvalaraCustomerCode <$> customerAvalaraCode customer
    charges <- getCharges shippingData avalaraCode items maybeCoupon priorityShipping False
    mapException PlaceOrderCouponError $ checkCouponMeetsMinimum maybeCoupon charges
    shippingCharge <-
        case ccShippingMethods charges of
            [] ->
                throwIO NoShippingMethod
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
        if ccAmount (ccTax charges) > mkCents 0 then
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
            orderProductPrice prod `timesQuantity` orderProductQuantity prod
          makeProduct CartItemData { cidVariant, cidQuantity } =
            OrderProduct
                { orderProductOrderId = orderId
                , orderProductProductVariantId = vdId cidVariant
                , orderProductQuantity = cidQuantity
                , orderProductPrice = getVariantPrice cidVariant
                }


-- | Reduce the quantities of purchased products.
reduceQuantities :: OrderId -> AppSQL ()
reduceQuantities orderId =
    selectList [OrderProductOrderId ==. orderId] [] >>=
    mapM_ (\(Entity _ p) ->
        update (orderProductProductVariantId p)
            [ProductVariantQuantity -=. orderProductQuantity p]
    )


-- | Enqueue the asynchronous tasks to perform after a new Order is placed.
enqueuePostOrderTasks :: OrderId -> AppSQL ()
enqueuePostOrderTasks orderId =
    mapM_ (enqueueTask Nothing)
        [ SendEmail $ Emails.OrderPlaced orderId
        , UpdateSalesCache orderId
        , RemoveSoldOutProducts orderId
        ]

-- | Delete the Customer's existing cart and move the anonymous cart to the
-- Customer.
overwriteCustomerCart :: CustomerId -> Maybe T.Text -> AppSQL ()
overwriteCustomerCart customerId cartToken = do
    getBy (UniqueCustomerCart $ Just customerId) >>= mapM_ (E.deleteCascade . entityKey)
    -- TODO sand-witch: deleteCascade is deprecated
    mapM_ (`mergeAnonymousCart` customerId) cartToken


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
    runOrderDetails $ getOrderDetails (E.toSqlKey $ cspOrderId parameters) customerId


runOrderDetails :: AppSQL a -> App a
runOrderDetails =
    handle handleError . runDB
    where handleError = \case
            OrderNotFound ->
                serverError err404

getOrderDetails :: OrderId -> CustomerId -> AppSQL OrderDetails
getOrderDetails externalOrderId customerId = do
        (e@(Entity orderId _), shipping, billing) <-
            getOrderAndAddress customerId externalOrderId
                >>= maybe (throwIO OrderNotFound) return
        lineItems <-
            selectList [OrderLineItemOrderId ==. orderId] []
        products <- getCheckoutProducts orderId
        delivery <- selectList [OrderDeliveryOrderId ==. orderId] []
        return OrderDetails
            { odOrder = toCheckoutOrder e
            , odLineItems = lineItems
            , odProducts = products
            , odShippingAddress = toAddressData shipping
            , odBillingAddress = toAddressData <$> billing
            , odDeliveryData = toDeliveryData <$> delivery
            }
data AnonymousSuccessParameters = MkAnonymousSuccessParameters
    { aspOrderId :: Int64
    , aspSession :: UUID.UUID
    }

instance FromJSON AnonymousSuccessParameters where
    parseJSON = withObject "AnonymousSuccessParameters" $ \o ->
        MkAnonymousSuccessParameters <$> o .: "orderId" <*> o .: "token"

type AnonymousSuccessRoute =
    ReqBody '[JSON] AnonymousSuccessParameters :> Post '[JSON] OrderDetails

anonymousSuccessRoute :: AnonymousSuccessParameters -> App OrderDetails
anonymousSuccessRoute asp = do
    mbOrderAndCustomer <- runDB $ runMaybeT $ do
        Entity orderId order <-
            MaybeT $ getBy $ UniqueGuestToken $ Just $ UUID.toText $ aspSession asp
        guard (E.fromSqlKey orderId == aspOrderId asp)
        pure (orderCustomerId order, orderId)
    case mbOrderAndCustomer of
        Nothing -> serverError err404
        Just (customerId, orderId) ->
            runOrderDetails $ getOrderDetails orderId customerId

data SuccessError
    = OrderNotFound
    deriving (Show, Typeable)

instance Exception SuccessError


getOrderAndAddress :: CustomerId -> OrderId -> AppSQL (Maybe (Entity Order, Entity Address, Maybe (Entity Address)))
getOrderAndAddress customerId orderId =
    fmap listToMaybe . E.select $ do
        (o E.:& s E.:& b) <- E.from $ E.table
            `E.innerJoin` E.table
                `E.on` (\(o E.:& s) -> o E.^. OrderShippingAddressId E.==. s E.^. AddressId)
            `E.leftJoin` E.table
                `E.on` (\(o E.:& _ E.:& b) -> o E.^. OrderBillingAddressId E.==. b E.?. AddressId)
        E.where_ $
            o E.^. OrderId E.==. E.val orderId E.&&.
            o E.^. OrderCustomerId E.==. E.val customerId
        return (o, s, b)

-- CHECKOUT TOKEN

newtype CheckoutTokenData = CheckoutTokenData
    { ctdToken :: CheckoutToken }

instance ToJSON CheckoutTokenData where
    toJSON (CheckoutTokenData token) =
        object ["token" .= token]

-- When the user is authenticated,
-- we can get their corresponding Helcim Customer id (if it exists) or create a new one.
type CheckoutTokenRoute = AuthProtect "cookie-auth" :> Post '[JSON] (Cookied CheckoutTokenData)

getCheckoutTokenRoute :: WrappedAuthToken -> App (Cookied CheckoutTokenData)
getCheckoutTokenRoute token = withValidatedCookie token $ \(Entity _ customer) ->
    getCheckoutToken (customerHelcimCustomerId customer)

type AnonymousCheckoutTokenRoute = Post '[JSON] CheckoutTokenData

anonymousGetCheckoutTokenRoute :: App CheckoutTokenData
anonymousGetCheckoutTokenRoute = do
    getCheckoutToken Nothing

getCheckoutToken :: Maybe Helcim.CustomerId -> App CheckoutTokenData
getCheckoutToken mbHelcimCustomerId = do
    mbCustomerCode <- case mbHelcimCustomerId of
        Just helcimCustomerId -> do
            getCustomer helcimCustomerId >>= either (throwIO . HelcimError) (return . Just . creCustomerCode)
        Nothing -> return Nothing
    createVerifyCheckout mbCustomerCode >>= \case
        Left e -> throwIO $ HelcimError e
        Right CheckoutCreateResponse {..} ->
            return CheckoutTokenData { ctdToken = ccrCheckoutToken }

-- Utils


eitherM :: Monad m => (b -> m a) -> Either b a -> m a
eitherM handler =
    either handler return


mapException
    :: (Exception e1, Exception e2, MonadUnliftIO m)
    => (e1 -> e2) -> m a -> m a
mapException transform = try >=> eitherM (throwIO . transform)
