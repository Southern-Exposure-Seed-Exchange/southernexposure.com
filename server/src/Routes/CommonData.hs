{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Routes.CommonData
    ( ProductData
    , getProductData
    , PredecessorCategory
    , categoryToPredecessor
    , AuthorizationData
    , toAuthorizationData
    , CartItemData(..)
    , getCartItems
    , CartCharges(..)
    , getCharges
    , ShippingCharge(..)
    , CartCharge(..)
    , calculateCouponDiscount
    , calculatePriorityFee
    , AddressData(..)
    , fromAddressData
    , toAddressData
    ) where

import Data.Aeson ((.=), (.:), (.:?), ToJSON(..), FromJSON(..), object, withObject)
import Data.List (intersect)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Database.Persist ((==.), Entity(..), SelectOpt(Asc), selectList, getBy)
import Numeric.Natural (Natural)

import Models
import Models.Fields
import Server
import Validation (Validation(..))

import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


-- Products


data ProductData =
    ProductData
        { pdProduct :: Entity Product
        , pdVariants :: [Entity ProductVariant]
        , pdSeedAttribute :: Maybe (Entity SeedAttribute)
        } deriving (Show)

instance ToJSON ProductData where
    toJSON ProductData { pdProduct, pdVariants, pdSeedAttribute } =
        object [ "product" .= toJSON pdProduct
               , "variants" .= toJSON pdVariants
               , "seedAttribute" .= toJSON pdSeedAttribute
               ]

getProductData :: Entity Product -> App ProductData
getProductData e@(Entity productId _) =
    runDB $ do
        variants <- selectList [ProductVariantProductId ==. productId] []
        maybeAttribute <- getBy $ UniqueAttribute productId
        return $ ProductData e variants maybeAttribute


-- Categories


data PredecessorCategory =
    PredecessorCategory
        { cpCategoryId :: Key Category
        , cpName :: T.Text
        , cpSlug :: T.Text
        } deriving (Show)

instance ToJSON PredecessorCategory where
    toJSON PredecessorCategory { cpCategoryId, cpName, cpSlug } =
        object [ "id" .= toJSON cpCategoryId
               , "name" .= toJSON cpName
               , "slug" .= toJSON cpSlug
               ]

categoryToPredecessor :: Entity Category -> PredecessorCategory
categoryToPredecessor (Entity categoryId category) =
    PredecessorCategory categoryId (categoryName category) (categorySlug category)


-- Customers


data AuthorizationData =
    AuthorizationData
        { adId :: CustomerId
        , adEmail :: T.Text
        , adToken :: T.Text
        } deriving (Show)

instance ToJSON AuthorizationData where
    toJSON authData =
        object [ "id" .= toJSON (adId authData)
               , "email" .= toJSON (adEmail authData)
               , "token" .= toJSON (adToken authData)
               ]

toAuthorizationData :: Entity Customer -> AuthorizationData
toAuthorizationData (Entity customerId customer) =
    AuthorizationData
        { adId = customerId
        , adEmail = customerEmail customer
        , adToken = customerAuthToken customer
        }


-- Carts


data CartItemData =
    CartItemData
        { cidItemId :: CartItemId
        , cidProduct :: Entity Product
        , cidVariant :: Entity ProductVariant
        , cidMaybeSeedAttribute :: Maybe (Entity SeedAttribute)
        , cidQuantity :: Natural
        , cidTax :: Cents
        }

instance ToJSON CartItemData where
    toJSON item =
        object [ "id" .= cidItemId item
               , "product" .= cidProduct item
               , "variant" .= cidVariant item
               , "seedAttribute" .= cidMaybeSeedAttribute item
               , "quantity" .= cidQuantity item
               , "tax" .= cidTax item
               ]


-- TODO: Add Grand Total & Don't Calculate in Frontend
data CartCharges =
    CartCharges
        { ccTax :: CartCharge
        , ccSurcharges :: [CartCharge]
        , ccShippingMethods :: [ShippingCharge]
        , ccPriorityShippingFee :: Maybe CartCharge
        , ccProductTotal :: Cents
        , ccMemberDiscount :: Maybe CartCharge
        , ccCouponDiscount :: Maybe CartCharge
        , ccGrandTotal :: Cents
        }

-- TODO: Add Product & Grand Total - Don't Calculate Either in Frontend
instance ToJSON CartCharges where
    toJSON charges =
        object [ "tax" .= ccTax charges
               , "surcharges" .= ccSurcharges charges
               , "shippingMethods" .= ccShippingMethods charges
               , "priorityShipping" .= ccPriorityShippingFee charges
               , "memberDiscount" .= ccMemberDiscount charges
               , "couponDiscount" .= ccCouponDiscount charges
               ]

data ShippingCharge =
    ShippingCharge
        { scCharge :: CartCharge
        , scPriorityFee :: Maybe PriorityShippingFee
        } deriving (Show)

instance ToJSON ShippingCharge where
    toJSON charge =
        object [ "charge" .= scCharge charge
               , "priorityFee" .= scPriorityFee charge
               ]


data CartCharge =
    CartCharge
        { ccDescription :: T.Text
        , ccAmount :: Cents
        } deriving (Show)

instance ToJSON CartCharge where
    toJSON charge =
        object [ "description" .= ccDescription charge
               , "amount" .= ccAmount charge
               ]

{-| Retrieve all the `CartItem`s of a `Cart` along with their `Product`,
`ProductVariant`, & Tax information.

-}
getCartItems
    :: Maybe TaxRate                                                -- ^ The tax to apply
    -> (E.SqlExpr (Entity Cart) -> E.SqlExpr (E.Value Bool))        -- ^ The `E.where_` query
    -> AppSQL [CartItemData]
getCartItems maybeTaxRate whereQuery = do
    items <- E.select $ E.from
        $ \(ci `E.InnerJoin` c `E.InnerJoin` v `E.InnerJoin` p `E.LeftOuterJoin` sa) -> do
            E.on (sa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId))
            E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
            E.on (v E.^. ProductVariantId E.==. ci E.^. CartItemProductVariantId)
            E.on (c E.^. CartId E.==. ci E.^. CartItemCartId)
            E.where_ $ whereQuery c
            E.orderBy [E.asc $ p E.^. ProductName]
            return (ci E.^. CartItemId, p, v, sa, ci E.^. CartItemQuantity)
    return $ map toItemData items
    where toItemData (i, p, v, sa, q) =
            let
                quantity =
                    E.unValue q
                productTotal =
                    Cents
                        $ fromCents (productVariantPrice $ fromEntity v)
                        * quantity
                fromEntity (Entity _ e) =
                    e
                entityId (Entity eId _) =
                    eId
            in
                CartItemData
                    { cidItemId = E.unValue i
                    , cidProduct = p
                    , cidVariant = v
                    , cidMaybeSeedAttribute = sa
                    , cidQuantity = quantity
                    , cidTax =
                        maybe (Cents 0) (applyTaxRate productTotal $ entityId p) maybeTaxRate
                    }


{-| Calculate the Charges for a set of Cart Items.

Tax, Shipping, & the Membership/Coupon Discounts are all based off of the
Product subtotal.

If the product subtotal does not meet the minimum order amount for
a coupon, no coupon discount will be applied. It is up to the calling code
to properly return an error for the user.

-}
getCharges
    :: Maybe TaxRate            -- ^ Applies tax to the Products if specified.
    -> Maybe Country            -- ^ Used to calculate the Shipping charge.
    -> [CartItemData]           -- ^ The cart items (see `getCartItems`)
    -> Bool                     -- ^ Include the 5% Member Discount?
    -> Maybe (Entity Coupon)    -- ^ A Coupon to apply.
    -> Bool                     -- ^ Include Priority S&H
    -> AppSQL CartCharges
getCharges maybeTaxRate maybeCountry items includeMemberDiscount maybeCoupon priorityShipping =
    let
        variant item =
            (\(Entity _ v) -> v) $ cidVariant item
        subTotal =
            foldl (\acc item -> acc + itemTotal item) 0 items
        itemTotal item =
            fromIntegral (cidQuantity item) * fromCents (productVariantPrice $ variant item)
        taxTotal =
            foldl (\acc item -> acc + fromCents (cidTax item)) 0 items
        memberDiscount =
            if includeMemberDiscount && calculatedMemberDiscount > 0 then
                Just CartCharge
                    { ccDescription = "5% Member Discount"
                    , ccAmount = calculatedMemberDiscount
                    }
            else Nothing
        calculatedMemberDiscount =
            Cents . round $ (5 % 100) * toRational subTotal
        couponCredit ms (Entity _ coupon) =
            if couponMinimumOrder coupon <= Cents subTotal then
                Just $ CartCharge
                    { ccDescription = "Coupon " <> couponCode coupon
                    , ccAmount =
                        calculateCouponDiscount coupon (map scCharge ms) subTotal
                    }
            else
                Nothing
        priorityCharge ms =
            if priorityShipping then
                CartCharge "Priority Shipping & Handling"
                    <$> calculatePriorityFee ms subTotal
            else
                Nothing
        sumGrandTotal cc =
            cc {
                ccGrandTotal =
                    ccProductTotal cc
                        + sum (map ccAmount $ ccSurcharges cc)
                        + amt ccTax
                        + mAmt (listToMaybe . map scCharge . ccShippingMethods)
                        + mAmt ccPriorityShippingFee
                        - mAmt ccMemberDiscount
                        - mAmt ccCouponDiscount
            }
            where amt f = ccAmount $ f cc
                  mAmt f = maybe (Cents 0) ccAmount $ f cc
    in do
        surcharges <- getSurcharges items
        shippingMethods <- getShippingMethods maybeCountry items subTotal
        return $ sumGrandTotal CartCharges
            { ccTax = CartCharge (maybe "" taxRateDescription maybeTaxRate) $ Cents taxTotal
            , ccSurcharges = surcharges
            , ccShippingMethods = shippingMethods
            , ccPriorityShippingFee = priorityCharge shippingMethods
            , ccProductTotal = Cents subTotal
            , ccMemberDiscount = memberDiscount
            , ccCouponDiscount = maybeCoupon >>= couponCredit shippingMethods
            , ccGrandTotal = 0
            }

-- | Calculate the discount of a Coupon, given the Coupon, a list of
-- shipping methods, & the order sub-total.
calculateCouponDiscount :: Coupon -> [CartCharge] -> Natural -> Cents
calculateCouponDiscount coupon shipMethods subTotal =
    case (couponDiscount coupon, shipMethods) of
        (FreeShipping, m:_) ->
            ccAmount m
        (FreeShipping, []) ->
            Cents 0
        -- TODO: Should Flat have a max of subTotal or grandTotal?
        (FlatDiscount (Cents amt), _) ->
            Cents $ min amt subTotal
        (PercentageDiscount percent, _) ->
            Cents . round $ toRational subTotal * (fromIntegral percent % 100)

calculatePriorityFee :: [ShippingCharge] -> Natural -> Maybe Cents
calculatePriorityFee shipMethods subTotal =
    case shipMethods of
        ShippingCharge _ (Just fee):_ ->
            let (PriorityShippingFee (Cents flatRate) percentRate) = fee
                percentAmount :: Rational
                percentAmount =
                    toRational subTotal * (fromIntegral percentRate % 100)
            in
                Just . Cents . round $ toRational flatRate + percentAmount
        _ ->
            Nothing

getSurcharges :: [CartItemData] -> AppSQL [CartCharge]
getSurcharges items =
    mapMaybe (getSurcharge $ foldl buildQuantityMap M.empty items)
        <$> selectList [SurchargeIsActive ==. True] []
    where buildQuantityMap initialMap item =
            foldl (\acc cId -> M.insertWith (+) cId (cidQuantity item) acc)
                initialMap
                ((\(Entity _ p) -> productCategoryIds p) $ cidProduct item)
          getSurcharge categories (Entity _ surcharge) =
            let
                quantity =
                    foldl (\acc catId -> maybe acc (+ acc) $ M.lookup catId categories)
                        0 (surchargeCategoryIds surcharge)
                amount =
                    if quantity == 1 then
                        surchargeSingleFee surcharge
                    else
                        surchargeMultipleFee surcharge
            in
                if quantity == 0 then
                    Nothing
                else
                    Just $ CartCharge (surchargeDescription surcharge) amount

getShippingMethods :: Maybe Country -> [CartItemData] -> Natural -> AppSQL [ShippingCharge]
getShippingMethods maybeCountry items subTotal =
    case maybeCountry of
        Nothing ->
            return []
        Just country ->
            mapMaybe (getMethod country)
                <$> selectList [ShippingMethodIsActive ==. True] [Asc ShippingMethodPriority]
    where getMethod country (Entity _ method) =
            let
                validCountry =
                    country `elem` shippingMethodCountries method
                validProducts =
                    null (shippingMethodCategoryIds method)
                        || all isValidProduct (map cidProduct items)
                isValidProduct (Entity _ prod) =
                     not . null $ productCategoryIds prod `intersect` shippingMethodCategoryIds method
                thresholdAmount rates =
                    case rates of
                        [] ->
                            Cents 0
                        [r] ->
                            applyRate r
                        r1 : r2 : rest ->
                            if getThreshold r1 <= Cents subTotal && getThreshold r2 > Cents subTotal then
                                applyRate r1
                            else
                                thresholdAmount $ r2 : rest
                getThreshold rate =
                    case rate of
                        Flat t _ ->
                            t
                        Percentage t _ ->
                            t
                applyRate rate =
                    case rate of
                        Flat _ amount ->
                            amount
                        Percentage _ percentage ->
                            Cents . round $ (toRational percentage / 100) * toRational subTotal
                addPriorityFee charge =
                    if priorityExcluded then
                        ShippingCharge charge Nothing
                    else
                        ShippingCharge charge
                            $ Just $ shippingMethodPriorityRate method
                priorityExcluded =
                    any productExcludesPriority (map cidProduct items)
                productExcludesPriority (Entity _ prod) =
                    not . null $ productCategoryIds prod `intersect`
                        shippingMethodExcludedPriorityCategoryIds method
            in
                if validCountry && validProducts then
                    Just . addPriorityFee
                        . CartCharge (shippingMethodDescription method)
                        . thresholdAmount $ shippingMethodRates method
                else
                    Nothing


-- Addresses


data AddressData =
    AddressData
        { adAddressId :: Maybe AddressId
        , adFirstName :: T.Text
        , adLastName :: T.Text
        , adCompanyName :: T.Text
        , adAddressOne :: T.Text
        , adAddressTwo :: T.Text
        , adCity :: T.Text
        , adState :: Region
        , adZipCode :: T.Text
        , adCountry :: Country
        , adIsDefault :: Bool
        } deriving (Eq)

instance FromJSON AddressData where
    parseJSON = withObject "AddressData" $ \v ->
        AddressData
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

instance ToJSON AddressData where
    toJSON address =
        object
            [ "id" .= adAddressId address
            , "firstName" .= adFirstName address
            , "lastName" .= adLastName address
            , "companyName" .= adCompanyName address
            , "addressOne" .= adAddressOne address
            , "addressTwo" .= adAddressTwo address
            , "city" .= adCity address
            , "state" .= adState address
            , "zipCode" .= adZipCode address
            , "country" .= adCountry address
            , "isDefault" .= adIsDefault address
            ]

instance Validation AddressData where
    validators address =
        let
            invalidRegionForCountry =
                case (fromCountry (adCountry address), adState address) of
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
            customRegionValidator =
                case adState address of
                    CustomRegion region ->
                        V.required region
                    _ ->
                        ("", False)
        in
            return
                [ ( "firstName", [ V.required $ adFirstName address ] )
                , ( "lastName", [ V.required $ adLastName address ] )
                , ( "addressOne", [ V.required $ adAddressOne address ] )
                , ( "city", [ V.required $ adCity address ] )
                , ( "state"
                  , [ ( "Invalid region for the selected Country.", invalidRegionForCountry )
                    , customRegionValidator
                    ]
                  )
                , ( "zipCode", [ V.required $ adZipCode address ] )
                ]

fromAddressData :: AddressType -> CustomerId -> AddressData -> Address
fromAddressData type_ customerId address =
    Address
        { addressFirstName = adFirstName address
        , addressLastName = adLastName address
        , addressCompanyName = adCompanyName address
        , addressAddressOne = adAddressOne address
        , addressAddressTwo = adAddressTwo address
        , addressCity = adCity address
        , addressState = adState address
        , addressZipCode = adZipCode address
        , addressCountry = adCountry address
        , addressIsDefault = adIsDefault address
        , addressType = type_
        , addressCustomerId = customerId
        , addressIsActive = True
        }

toAddressData :: Entity Address -> AddressData
toAddressData (Entity addressId address) =
    AddressData
        { adAddressId = Just addressId
        , adFirstName = addressFirstName address
        , adLastName = addressLastName address
        , adCompanyName = addressCompanyName address
        , adAddressOne = addressAddressOne address
        , adAddressTwo = addressAddressTwo address
        , adCity = addressCity address
        , adState = addressState address
        , adZipCode = addressZipCode address
        , adCountry = addressCountry address
        , adIsDefault = addressIsDefault address
        }
