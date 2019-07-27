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
import Control.Exception.Safe (MonadThrow, MonadCatch, throwM, Exception, try)
import Control.Monad ((>=>), (<=<), when, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), withObject, object)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.List (partition, find)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Persist
    ( (==.), (=.), (-=.), Entity(..), insert, insert_, insertEntity, get, getBy
    , selectList, update, updateWhere, delete, deleteWhere, count
    )
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, JSON, Post, ReqBody, err404)
import Web.Stripe ((-&-), StripeConfig, stripe)
import Web.Stripe.Card (createCustomerCardByToken, updateCustomerCard, getCustomerCards)
import Web.Stripe.Charge (createCharge)
import Web.Stripe.Customer (Email(..), createCustomer, updateCustomer)

import Auth
import Config
import Models
import Models.Fields
import Server
import Routes.CommonData
    ( AuthorizationData, toAuthorizationData, CartItemData(..), CartCharges(..)
    , CartCharge(..), getCartItems, getCharges, AddressData(..), toAddressData
    , fromAddressData, ShippingCharge(..), VariantData(..), getVariantPrice
    )
import Routes.Utils (hashPassword, generateUniqueToken)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Emails
import qualified Emails.OrderPlaced as OrderPlaced
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
        { cdpShippingRegion :: Maybe Region
        , cdpShippingCountry :: Maybe Country
        , cdpExistingAddress :: Maybe AddressId
        , cdpMemberNumber :: Maybe T.Text
        , cdpCouponCode :: Maybe T.Text
        , cdpPriorityShipping :: Bool
        }

instance FromJSON CustomerDetailsParameters where
    parseJSON = withObject "CustomerDetailsParameters" $ \v ->
        CustomerDetailsParameters
            <$> v .:? "region"
            <*> v .:? "country"
            <*> v .:? "addressId"
            <*> v .:? "memberNumber"
            <*> v .:? "couponCode"
            <*> v .:  "priorityShipping"

data CheckoutDetailsData =
    CheckoutDetailsData
        { cddShippingAddresses :: [AddressData]
        , cddBillingAddresses :: [AddressData]
        , cddItems :: [CartItemData]
        , cddCharges :: CartCharges
        , cddStoreCredit :: Cents
        , cddMemberNumber :: T.Text
        }

instance ToJSON CheckoutDetailsData where
    toJSON details =
        object
            [ "shippingAddresses" .= cddShippingAddresses details
            , "billingAddresses" .= cddBillingAddresses details
            , "items" .= cddItems details
            , "charges" .= cddCharges details
            , "storeCredit" .= cddStoreCredit details
            , "memberNumber" .= cddMemberNumber details
            ]

type CustomerDetailsRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

customerDetailsRoute :: AuthToken -> CustomerDetailsParameters -> App CheckoutDetailsData
customerDetailsRoute token parameters = do
    (Entity customerId customer) <- validateToken token
    let hasValidMemberNumber =
            (> 3) . T.length . fromMaybe (customerMemberNumber customer)
                $ cdpMemberNumber parameters
        priorityShipping =
            cdpPriorityShipping parameters
    currentTime <- liftIO getCurrentTime
    withCheckoutDetailsErrors . runDB $ do
        customerAddresses <- selectList
            [AddressCustomerId ==. customerId, AddressIsActive ==. True] []
        let (shippingAddresses, billingAddresses) =
                partition (\a -> addressType (entityVal a) == Shipping)
                    customerAddresses
            (maybeCountry, maybeRegion) =
                getShippingArea parameters shippingAddresses
        maybeTaxRate <- getTaxRate maybeCountry maybeRegion
        maybeCoupon <- mapException DetailsCouponError $ sequence
            $ getCoupon currentTime (Just customerId) <$> cdpCouponCode parameters
        items <- getCartItems maybeTaxRate $ \c ->
            c E.^. CartCustomerId E.==. E.just (E.val customerId)
        charges <- getCharges maybeTaxRate maybeCountry items
            hasValidMemberNumber maybeCoupon priorityShipping
        mapException DetailsCouponError
            $ checkCouponMeetsMinimum maybeCoupon charges
        mapException DetailsPriorityError
                $ checkPriorityShippingAvailable priorityShipping charges
        return CheckoutDetailsData
            { cddShippingAddresses = map toAddressData shippingAddresses
            , cddBillingAddresses = map toAddressData billingAddresses
            , cddItems = items
            , cddCharges = charges
            , cddStoreCredit = customerStoreCredit customer
            , cddMemberNumber = customerMemberNumber customer
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
        , adpMemberNumber :: Maybe T.Text
        , adpCouponCode :: Maybe T.Text
        , adpPriorityShipping :: Bool
        , adpCartToken :: T.Text
        }

instance FromJSON AnonymousDetailsParameters where
    parseJSON = withObject "AnonymousDetailsParameters" $ \v ->
        AnonymousDetailsParameters
            <$> v .:? "country"
            <*> v .:? "region"
            <*> v .:? "memberNumber"
            <*> v .:? "couponCode"
            <*> v .:  "priorityShipping"
            <*> v .:  "sessionToken"

type AnonymousDetailsRoute =
       ReqBody '[JSON] AnonymousDetailsParameters
    :> Post '[JSON] CheckoutDetailsData

anonymousDetailsRoute :: AnonymousDetailsParameters -> App CheckoutDetailsData
anonymousDetailsRoute parameters =
    let
        maybeCountry =
            adpShippingCountry parameters
        priorityShipping =
            adpPriorityShipping parameters
        isValidMemberNumber =
            maybe False ((> 3) . T.length) $ adpMemberNumber parameters
    in
        withCheckoutDetailsErrors . runDB $ do
            currentTime <- liftIO getCurrentTime
            maybeTaxRate <- getTaxRate maybeCountry $ adpShippingRegion parameters
            maybeCoupon <- mapException DetailsCouponError $ sequence
                $ getCoupon currentTime Nothing <$> adpCouponCode parameters
            items <- getCartItems maybeTaxRate $ \c ->
                c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
            charges <- getCharges maybeTaxRate maybeCountry items
                isValidMemberNumber maybeCoupon priorityShipping
            mapException DetailsCouponError
                $ checkCouponMeetsMinimum maybeCoupon charges
            mapException DetailsPriorityError
                $ checkPriorityShippingAvailable priorityShipping charges
            return CheckoutDetailsData
                { cddShippingAddresses = []
                , cddBillingAddresses = []
                , cddItems = items
                , cddCharges = charges
                , cddStoreCredit = 0
                , cddMemberNumber = ""
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
        , cpopMemberNumber :: T.Text
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
            <*> v .: "memberNumber"
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
            : ( "memberNumber"
              , [ ( "Invalid Member Number"
                  , T.length (cpopMemberNumber parameters) > 0 &&
                    T.length (cpopMemberNumber parameters) < 4
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
    let memberNumberParameter = cpopMemberNumber parameters
        shippingParameter = cpopShippingAddress parameters
        maybeStripeToken = cpopStripeToken parameters
    currentTime <- liftIO getCurrentTime
    when (not (T.null memberNumberParameter) && memberNumberParameter /= customerMemberNumber customer)
        $ runDB $ update customerId [CustomerMemberNumber =. cpopMemberNumber parameters]
    stripeConfig <- asks getStripeConfig
    orderId <- withPlaceOrderErrors shippingParameter . runDB $ do
        (maybeBillingAddress, orderId, orderTotal, cartId, appliedCredit) <-
            createAddressesAndOrder ce shippingParameter (cpopBillingAddress parameters)
                (cpopStoreCredit parameters) (cpopMemberNumber parameters)
                (cpopPriorityShipping parameters) (cpopCouponCode parameters)
                (cpopComment parameters) currentTime
        when (orderTotal > 0) $ case (maybeBillingAddress, maybeStripeToken) of
            (Just billingAddress, Just stripeToken) -> do
                stripeCustomerId <- getOrCreateStripeCustomer stripeConfig ce stripeToken
                setCustomerCard stripeConfig customer stripeCustomerId billingAddress stripeToken
                chargeCustomer stripeConfig stripeCustomerId orderId orderTotal
            (Nothing, _) ->
                throwM BillingAddressRequired
            (_, Nothing) ->
                throwM StripeTokenRequired
        when (appliedCredit > customerStoreCredit customer) $
            throwM NotEnoughStoreCredit
        when (appliedCredit > 0) $
            update customerId [CustomerStoreCredit -=. appliedCredit]
        deleteCart cartId
        return orderId
    runDB (OrderPlaced.fetchData orderId)
        >>= maybe (return ()) (void . Emails.send . Emails.OrderPlaced)
    return $ PlaceOrderData orderId
    where
          getOrCreateStripeCustomer :: StripeConfig -> Entity Customer -> T.Text -> AppSQL StripeCustomerId
          getOrCreateStripeCustomer stripeConfig (Entity customerId customer) stripeToken =
            case customerStripeId customer of
                Just stripeId ->
                    return stripeId
                Nothing -> do
                    newId <- createStripeCustomer stripeConfig (customerEmail customer) stripeToken
                    update customerId [CustomerStripeId =. Just newId]
                    return newId
          createAddressesAndOrder
            :: Entity Customer -> CustomerAddress -> Maybe CustomerAddress
            -> Maybe Cents -> T.Text -> Bool -> Maybe T.Text -> T.Text -> UTCTime
            -> AppSQL (Maybe Address, OrderId, Cents, CartId, Cents)
          createAddressesAndOrder customer@(Entity customerId _) shippingParam billingParam maybeStoreCredit memberNumber priorityShipping maybeCouponCode comment currentTime = do
            shippingAddress <- getOrInsertAddress Shipping customerId shippingParam
            billingAddress <- sequence $ getOrInsertAddress Billing customerId <$> billingParam
            (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
                >>= maybe (throwM CartNotFound) return
            (orderId, orderTotal, appliedCredit) <- createOrder
                customer cartId shippingAddress (entityKey <$> billingAddress)
                maybeStoreCredit memberNumber priorityShipping maybeCouponCode comment currentTime
            return (entityVal <$> billingAddress, orderId, orderTotal, cartId, appliedCredit)
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
          setCustomerCard :: StripeConfig -> Customer -> StripeCustomerId -> Address -> T.Text -> AppSQL ()
          setCustomerCard stripeConfig customer stripeCustomerId billingAddress stripeToken = do
            stripeCardId <- case customerStripeId customer of
                Nothing ->
                    getFirstStripeCard stripeConfig stripeCustomerId
                Just _ -> do
                    -- Already Had Customer, Create New Card w/ Token
                    requestResult <- liftIO . stripe stripeConfig $
                        createCustomerCardByToken (fromStripeCustomerId stripeCustomerId)
                            (Stripe.TokenId stripeToken)
                    either (throwM . StripeError) (return . Stripe.cardId) requestResult
            updateCardAddress stripeConfig stripeCustomerId stripeCardId billingAddress
            liftIO (stripe stripeConfig (updateCustomer (fromStripeCustomerId stripeCustomerId)
                -&- Stripe.DefaultCard stripeCardId))
                >>= either (throwM . StripeError) (void . return)


data AnonymousPlaceOrderParameters =
    AnonymousPlaceOrderParameters
        { apopEmail :: T.Text
        , apopPassword :: T.Text
        , apopShippingAddress :: AddressData
        , apopBillingAddress :: Maybe AddressData
        , apopMemberNumber :: T.Text
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
            <*> v .: "memberNumber"
            <*> v .: "priorityShipping"
            <*> v .:? "couponCode"
            <*> v .: "comment"
            <*> v .: "sessionToken"
            <*> v .: "stripeToken"

instance Validation AnonymousPlaceOrderParameters where
    validators parameters = do
        emailDoesntExist <- V.doesntExist . UniqueEmail $ apopEmail parameters
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
            , ( "memberNumber"
              , [ ( "Invalid Member Number"
                  , T.length (apopMemberNumber parameters) > 0 &&
                    T.length (apopMemberNumber parameters) < 4
                  )
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
    let memberNumber = apopMemberNumber parameters
        shippingParameter = NewAddress $ apopShippingAddress parameters
    stripeConfig <- asks getStripeConfig
    (orderId, orderData) <- withPlaceOrderErrors shippingParameter . runDB $ do
        let customer = Customer
                { customerEmail = apopEmail parameters
                , customerStoreCredit = Cents 0
                , customerMemberNumber = memberNumber
                , customerEncryptedPassword = encryptedPass
                , customerAuthToken = authToken
                , customerStripeId = Nothing
                , customerIsAdmin = False
                }
        customerId <- insert customer
        mergeCarts (apopCartToken parameters) customerId
        (Entity cartId _) <- getBy (UniqueCustomerCart $ Just customerId)
            >>= maybe (throwM CartNotFound) return
        let shippingAddress = fromAddressData Shipping customerId
                $ apopShippingAddress parameters
            maybeBillingAddress = fromAddressData Billing customerId
                <$> apopBillingAddress parameters
        shippingId <- insert shippingAddress
        billingId <- sequence $ insert <$> maybeBillingAddress
        (orderId, orderTotal, _) <- createOrder (Entity customerId customer)
            cartId (Entity shippingId shippingAddress) billingId Nothing
            memberNumber (apopPriorityShipping parameters)
            (apopCouponCode parameters) (apopComment parameters) currentTime
        when (orderTotal > 0) $ case (maybeBillingAddress, apopStripeToken parameters) of
            (Just billingAddress, Just stripeToken) -> do
                stripeCustomerId <- createStripeCustomer stripeConfig (apopEmail parameters)
                    stripeToken
                update customerId [CustomerStripeId =. Just stripeCustomerId]
                stripeCardId <- getFirstStripeCard stripeConfig stripeCustomerId
                updateCardAddress stripeConfig stripeCustomerId stripeCardId billingAddress
                chargeCustomer stripeConfig stripeCustomerId orderId orderTotal
            (Nothing, _) ->
                throwM BillingAddressRequired
            (_, Nothing) ->
                throwM StripeTokenRequired
        deleteCart cartId
        return
            ( orderId
            , AnonymousPlaceOrderData orderId . toAuthorizationData
                $ Entity customerId customer
            )
    runDB (OrderPlaced.fetchData orderId)
        >>= maybe (return ()) (void . Emails.send . Emails.OrderPlaced)
    return orderData


-- | Create a Stripe Customer with an Email & Token.
createStripeCustomer :: StripeConfig -> T.Text -> T.Text -> AppSQL StripeCustomerId
createStripeCustomer stripeConfig email stripeToken =
    liftIO (stripe stripeConfig $ createCustomer -&- Email email -&- Stripe.TokenId stripeToken)
        >>= either (throwM . StripeError)
            (return . StripeCustomerId . Stripe.customerId)


-- | Update the Name & Address of a Stripe Card.
updateCardAddress :: StripeConfig -> StripeCustomerId -> Stripe.CardId -> Address -> AppSQL ()
updateCardAddress stripeConfig stripeCustomerId stripeCardId billingAddress =
    liftIO . void . stripe stripeConfig $
        updateCustomerCard (fromStripeCustomerId stripeCustomerId) stripeCardId
            -&- Stripe.Name (addressFirstName billingAddress <> " " <> addressLastName billingAddress)
            -&- Stripe.AddressLine1 (addressAddressOne billingAddress)
            -&- Stripe.AddressLine2 (addressAddressTwo billingAddress)
            -&- Stripe.AddressCity (addressCity billingAddress)
            -&- Stripe.AddressState (regionName $ addressState billingAddress)


-- | Charge a Stripe Customer for an Order or throw a validation error.
chargeCustomer :: StripeConfig -> StripeCustomerId -> OrderId -> Cents -> AppSQL ()
chargeCustomer stripeConfig stripeCustomerId orderId orderTotal = do
    chargeResult <- liftIO $ stripe stripeConfig
        (createCharge (toStripeAmount orderTotal) Stripe.USD
            -&- fromStripeCustomerId stripeCustomerId)
    case chargeResult of
        Left err ->
            update orderId [OrderStatus =. PaymentFailed]
                >> throwM (StripeError err)
        Right stripeCharge ->
            let
                stripeChargeId = StripeChargeId $ Stripe.chargeId stripeCharge
            in
                update orderId
                    [ OrderStatus =. PaymentReceived
                    , OrderStripeChargeId =. Just stripeChargeId
                    ]


-- | Return the first Stripe Card Id for a Stripe Customer. This is useful
-- for getting new CardId after creating a Stripe Customer from a Stripe
-- Token.
getFirstStripeCard :: (MonadThrow m, MonadIO m) => StripeConfig -> StripeCustomerId -> m Stripe.CardId
getFirstStripeCard stripeConfig stripeCustomerId =
    liftIO (stripe stripeConfig (getCustomerCards (fromStripeCustomerId stripeCustomerId)
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
    -> T.Text
    -- ^ Member Number
    -> Bool
    -- ^ Enable Priority Shipping
    -> Maybe T.Text
    -- ^ Coupon Code
    -> T.Text
    -- ^ Order Comment
    -> UTCTime
    -- ^ Current Time
    -> AppSQL (OrderId, Cents, Cents)
createOrder (Entity customerId customer) cartId shippingEntity billingId maybeStoreCredit memberNumber priorityShipping maybeCouponCode comment currentTime = do
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
        , orderCouponId = Nothing
        , orderCreatedAt = currentTime
        }
    (lineTotal, maybeCouponId) <- createLineItems currentTime customerId
        maybeTaxRate shippingAddress items orderId memberNumber
        priorityShipping maybeCouponCode
    when (isJust maybeCouponId) $
        update orderId [OrderCouponId =. maybeCouponId]
    productTotals <- createProducts maybeTaxRate items orderId
    let totalCharges = lineTotal + fromIntegral (fromCents $ sum productTotals)
    if totalCharges < 0 then
        -- TODO: Throw an error? This means credits > charges which shouldn't happen...
        return (orderId, 0, 0)
    else
        let totalInCents = Cents $ fromIntegral totalCharges in
        case maybeStoreCredit of
            Nothing ->
                return (orderId, totalInCents, 0)
            Just c ->
                if c > 0 && totalInCents > 0 then do
                    let storeCredit = min (customerStoreCredit customer) $ min totalInCents c
                    insert_ OrderLineItem
                        { orderLineItemOrderId = orderId
                        , orderLineItemType = StoreCreditLine
                        , orderLineItemDescription = "Store Credit"
                        , orderLineItemAmount = storeCredit
                        }
                    return (orderId, totalInCents - storeCredit, storeCredit)
                else
                    return (orderId, totalInCents, 0)


-- | Delete a Cart & all it's Items.
deleteCart :: CartId -> AppSQL ()
deleteCart cartId =
    deleteWhere [CartItemCartId ==. cartId] >> delete cartId

-- | Create the Shipping, Surcharge, MemberDiscount, & CouponDiscount
-- OrderLineItems for an Order, returning the total amount of all items
-- & the ID of a Coupon(if applied).
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
    -> Maybe TaxRate
    -- ^ Tax to Apply
    -> Address
    -- ^ Shipping Address
    -> [CartItemData]
    -- ^ Cart Items
    -> OrderId
    -- ^ Order ID to add Line Items to
    -> T.Text
    -- ^ Member Number
    -> Bool
    -- ^ Use Priority Shipping
    -> Maybe T.Text
    -- ^ Coupon Code
    -> AppSQL (Integer, Maybe CouponId)
createLineItems currentTime customerId maybeTaxRate shippingAddress items orderId memberNumber priorityShipping maybeCouponCode = do
    maybeCoupon <- mapException PlaceOrderCouponError $ sequence
        $ getCoupon currentTime (Just customerId) <$> maybeCouponCode
    -- TODO priority S&H
    charges <- getCharges maybeTaxRate
        (Just $ addressCountry shippingAddress) items
        (T.length memberNumber >= 4) maybeCoupon priorityShipping
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
    maybeMemberDiscount <-
        maybe (return Nothing) (fmap Just . insertEntity . makeLine MemberDiscountLine)
            $ ccMemberDiscount charges
    maybeCouponDiscount <-
        maybe (return Nothing) (fmap Just . insertEntity . makeLine CouponDiscountLine)
            $ ccCouponDiscount charges
    return
        ( sum (map (centsToInteger . orderLineItemAmount . entityVal) surcharges)
            + centsToInteger (orderLineItemAmount $ entityVal shippingLine)
            + maybeLineAmount maybePriorityShippingCharge
            - maybeLineAmount maybeMemberDiscount
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

createProducts :: Maybe TaxRate -> [CartItemData] -> OrderId -> AppSQL [Cents]
createProducts maybeTaxRate items orderId =
    mapM (fmap calculateTotal <$> insertEntity . makeProduct) items
    where calculateTotal (Entity _ prod) =
            orderProductPrice prod * (Cents $ orderProductQuantity prod)
                + orderProductTax prod
          makeProduct CartItemData { cidVariant, cidQuantity } =
            let
                price = getVariantPrice cidVariant
                productTotal = Cents $ fromCents price * cidQuantity
                tax = applyTax productTotal (vdProductId cidVariant)
            in
                OrderProduct
                    { orderProductOrderId = orderId
                    , orderProductProductVariantId = vdId cidVariant
                    , orderProductQuantity = cidQuantity
                    , orderProductPrice = price
                    , orderProductTax = tax
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
        , odBillingAddress :: Maybe AddressData
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


mapException
    :: (Exception e1, Exception e2, MonadThrow m, MonadCatch m)
    => (e1 -> e2) -> m a -> m a
mapException transform = try >=> eitherM (throwM . transform)
