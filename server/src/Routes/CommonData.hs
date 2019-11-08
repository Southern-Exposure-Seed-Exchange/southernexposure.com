{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Routes.CommonData
    ( ProductData
    , getProductData
    , BaseProductData
    , makeBaseProductData
    , VariantData(vdId, vdProductId)
    , makeVariantData
    , getVariantPrice
    , applySalesToVariants
    , applyCategorySale
    , applyCategorySaleDiscount
    , PredecessorCategory
    , categoryToPredecessor
    , CategoryData(cdDescription)
    , makeCategoryData
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

import Prelude hiding (product)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, lift)
import Data.Aeson ((.=), (.:), (.:?), ToJSON(..), FromJSON(..), object, withObject)
import Data.Int (Int64)
import Data.List (intersect)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Time (getCurrentTime)
import Database.Persist ((==.), (>=.), (<=.), Entity(..), SelectOpt(Asc), selectList, getBy)
import Numeric.Natural (Natural)

import Config
import Images
import Models
import Models.Fields
import Server
import Validation (Validation(..))

import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


-- Products


data ProductData =
    ProductData
        { pdProduct :: BaseProductData
        , pdVariants :: [VariantData]
        , pdSeedAttribute :: Maybe (Entity SeedAttribute)
        } deriving (Show)

instance ToJSON ProductData where
    toJSON ProductData { pdProduct, pdVariants, pdSeedAttribute } =
        object [ "product" .= toJSON pdProduct
               , "variants" .= toJSON pdVariants
               , "seedAttribute" .= toJSON pdSeedAttribute
               ]

data BaseProductData =
    BaseProductData
        { bpdId :: ProductId
        , bpdName :: T.Text
        , bpdSlug :: T.Text
        , bpdBaseSku :: T.Text
        , bpdLongDescription :: T.Text
        , bpdImage :: ImageSourceSet
        , bpdCategories :: [CategoryId]
        } deriving (Show)

instance ToJSON BaseProductData where
    toJSON BaseProductData {..} =
        object
            [ "id" .= bpdId
            , "name" .= bpdName
            , "slug" .= bpdSlug
            , "baseSku" .= bpdBaseSku
            , "longDescription" .= bpdLongDescription
            , "image" .= bpdImage
            ]

makeBaseProductData :: (MonadReader Config m, MonadIO m) => Entity Product -> m BaseProductData
makeBaseProductData (Entity pId Product {..}) = do
    image <- makeSourceSet "products" $ T.unpack productImageUrl
    return BaseProductData
        { bpdId = pId
        , bpdName = productName
        , bpdSlug = productSlug
        , bpdBaseSku = productBaseSku
        , bpdLongDescription = productLongDescription
        , bpdImage = image
        , bpdCategories = productCategoryIds
        }


data VariantData =
    VariantData
        { vdId :: ProductVariantId
        , vdProductId :: ProductId
        , vdSkuSuffix :: T.Text
        , vdPrice :: Cents
        , vdSalePrice :: Maybe Cents
        , vdQuantity :: Int64
        , vdLotSize :: Maybe LotSize
        , vdIsActive :: Bool
        } deriving (Show)

instance ToJSON VariantData where
    toJSON VariantData {..} =
        object
            [ "id" .= vdId
            , "productId" .= vdProductId
            , "skuSuffix" .= vdSkuSuffix
            , "price" .= vdPrice
            , "salePrice" .= vdSalePrice
            , "quantity" .= vdQuantity
            , "lotSize" .= vdLotSize
            , "isActive" .= vdIsActive
            ]

getVariantPrice :: VariantData -> Cents
getVariantPrice VariantData { vdPrice, vdSalePrice } =
    fromMaybe vdPrice vdSalePrice


getProductData :: Entity Product -> AppSQL ProductData
getProductData e@(Entity productId _) = do
    prod <- lift $ makeBaseProductData e
    variants <- selectList
        [ ProductVariantProductId ==. productId
        , ProductVariantIsActive ==. True
        ] []
        >>= applySalesToVariants e
    maybeAttribute <- getBy $ UniqueAttribute productId
    return $ ProductData prod variants maybeAttribute

-- | Apply any CategorySales & ProductSales to the ProductVariants while
-- morphing them into VariantData values.
applySalesToVariants :: Entity Product -> [Entity ProductVariant] -> AppSQL [VariantData]
applySalesToVariants p vs =
    mapM applyVariantSales vs >>= applyCategorySales p

-- | Search for any Product Sales & apply them to the ProductVariant,
-- transforming it into a VariantData.
applyVariantSales :: Entity ProductVariant -> AppSQL VariantData
applyVariantSales variant = do
    currentTime <- liftIO getCurrentTime
    maybeSale <- fmap (productSalePrice . entityVal) . listToMaybe <$>
        selectList
            [ ProductSaleProductVariantId ==. entityKey variant
            , ProductSaleStartDate <=. currentTime
            , ProductSaleEndDate >=. currentTime
            ]
            []
    return $ makeVariantData variant maybeSale

makeVariantData :: Entity ProductVariant -> Maybe Cents -> VariantData
makeVariantData (Entity variantId ProductVariant {..}) maybeSalePrice =
    let
        salePrice = maybeSalePrice >>= \price ->
            if price < productVariantPrice
                then return price
                else Nothing
    in VariantData
        { vdId = variantId
        , vdProductId = productVariantProductId
        , vdSkuSuffix = productVariantSkuSuffix
        , vdPrice = productVariantPrice
        , vdSalePrice = salePrice
        , vdQuantity = productVariantQuantity
        , vdLotSize = productVariantLotSize
        , vdIsActive = productVariantIsActive
        }

-- | Get the CategorySales for the Product's Categories, than apply any
-- amount if it is less than the Variant's current sale price.
applyCategorySales :: Entity Product -> [VariantData] -> AppSQL [VariantData]
applyCategorySales (Entity _ product) variants =
    getCategorySales product >>= \case
        [] ->
            return variants
        sale : _ ->
            return $ map (applyCategorySale sale) variants

-- | Apply the sale to the VariantData if there is no existing sale price
-- or if the sale price is cheaper than any existing sale price.
applyCategorySale :: CategorySale -> VariantData -> VariantData
applyCategorySale sale variant =
    let salePrice = applyCategorySaleDiscount sale variant
    in case vdSalePrice variant of
        Nothing ->
            variant { vdSalePrice = Just salePrice }
        Just existingSalePrice ->
            if existingSalePrice > salePrice then
                variant { vdSalePrice = Just salePrice }
            else
                variant

-- | Calculate the discounted price of a Variant, given a CategorySale.
applyCategorySaleDiscount :: CategorySale -> VariantData -> Cents
applyCategorySaleDiscount sale variant =
    case categorySaleType sale of
        FlatSale amount ->
            Cents $ fromInteger $ max 0
                $ asInteger (vdPrice variant) - asInteger amount
        PercentSale percent ->
            Cents . max 0 . round $
                toRational (fromCents $ vdPrice variant)
                * (1 - min 1 (fromIntegral percent % 100))
  where
    asInteger :: Cents -> Integer
    asInteger = fromIntegral . fromCents

-- | Get any active sales for the given categories.
getCategorySales :: Product -> AppSQL [CategorySale]
getCategorySales product = do
    currentTime <- liftIO getCurrentTime
    categories <- lift $ map entityKey . concat <$>
        mapM getParentCategories (productCategoryIds product)
    activeSales <- map entityVal <$> selectList
        [ CategorySaleStartDate <=. currentTime
        , CategorySaleEndDate >=. currentTime
        ]
        []
    return $ filter
        (not . null . L.intersect categories . categorySaleCategoryIds)
        activeSales



-- Categories


data PredecessorCategory =
    PredecessorCategory
        { pcCategoryId :: Key Category
        , pcName :: T.Text
        , pcSlug :: T.Text
        } deriving (Show)

instance ToJSON PredecessorCategory where
    toJSON PredecessorCategory { pcCategoryId, pcName, pcSlug } =
        object [ "id" .= toJSON pcCategoryId
               , "name" .= toJSON pcName
               , "slug" .= toJSON pcSlug
               ]

categoryToPredecessor :: Entity Category -> PredecessorCategory
categoryToPredecessor (Entity categoryId category) =
    PredecessorCategory categoryId (categoryName category) (categorySlug category)


data CategoryData =
    CategoryData
        { cdCategoryId :: CategoryId
        , cdName :: T.Text
        , cdSlug :: T.Text
        , cdParentId :: Maybe CategoryId
        , cdDescription :: T.Text
        , cdImage :: ImageSourceSet
        , cdOrder :: Int
        } deriving (Show)

instance ToJSON CategoryData where
    toJSON CategoryData {..} =
        object
            [ "id" .= cdCategoryId
            , "name" .= cdName
            , "slug" .= cdSlug
            , "parentId" .= cdParentId
            , "description" .= cdDescription
            , "image" .= cdImage
            , "order" .= cdOrder
            ]

makeCategoryData :: (MonadReader Config m, MonadIO m) => Entity Category -> m CategoryData
makeCategoryData (Entity cId Category {..}) = do
    image <- makeSourceSet "categories" $ T.unpack categoryImageUrl
    return CategoryData
        { cdCategoryId = cId
        , cdName = categoryName
        , cdSlug = categorySlug
        , cdParentId = categoryParentId
        , cdDescription = categoryDescription
        , cdImage = image
        , cdOrder = categoryOrder
        }



-- Customers


data AuthorizationData =
    AuthorizationData
        { adId :: CustomerId
        , adEmail :: T.Text
        , adIsAdmin :: Bool
        } deriving (Show)

instance ToJSON AuthorizationData where
    toJSON authData =
        object [ "id" .= toJSON (adId authData)
               , "email" .= toJSON (adEmail authData)
               , "isAdmin" .= toJSON (adIsAdmin authData)
               ]

toAuthorizationData :: Entity Customer -> AuthorizationData
toAuthorizationData (Entity customerId customer) =
    AuthorizationData
        { adId = customerId
        , adEmail = customerEmail customer
        , adIsAdmin = customerIsAdmin customer
        }


-- Carts


data CartItemData =
    CartItemData
        { cidItemId :: CartItemId
        , cidProduct :: BaseProductData
        , cidVariant :: VariantData
        , cidQuantity :: Natural
        , cidTax :: Cents
        }

instance ToJSON CartItemData where
    toJSON item =
        object [ "id" .= cidItemId item
               , "product" .= cidProduct item
               , "variant" .= cidVariant item
               , "quantity" .= cidQuantity item
               , "tax" .= cidTax item
               ]


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

-- TODO: Add Product Total - Don't Calculate in Frontend
instance ToJSON CartCharges where
    toJSON charges =
        object [ "tax" .= ccTax charges
               , "surcharges" .= ccSurcharges charges
               , "shippingMethods" .= ccShippingMethods charges
               , "priorityShipping" .= ccPriorityShippingFee charges
               , "memberDiscount" .= ccMemberDiscount charges
               , "couponDiscount" .= ccCouponDiscount charges
               , "grandTotal" .= ccGrandTotal charges
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
        $ \(ci `E.InnerJoin` c `E.InnerJoin` v `E.InnerJoin` p) -> do
            E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
            E.on (v E.^. ProductVariantId E.==. ci E.^. CartItemProductVariantId)
            E.on (c E.^. CartId E.==. ci E.^. CartItemCartId)
            E.where_ $ whereQuery c
            E.orderBy [E.asc $ p E.^. ProductName]
            return (ci E.^. CartItemId, p, v, ci E.^. CartItemQuantity)
    mapM toItemData items
    where toItemData (i, p, v, q) =
            let
                quantity =
                    E.unValue q
                productTotal vd =
                    Cents $ fromCents (getFinalPrice vd) * quantity
                tax vd =
                    case maybeTaxRate of
                        Nothing ->
                            Cents 0
                        Just taxRate ->
                            applyTaxRate (productTotal vd) (entityKey p) taxRate
            in do
                maybeCategorySale <- listToMaybe <$> getCategorySales (entityVal p)
                productData <- lift $ makeBaseProductData p
                variantData <- applyVariantSales v >>= \vd ->
                    return $ maybe vd (`applyCategorySale` vd) maybeCategorySale
                return CartItemData
                    { cidItemId = E.unValue i
                    , cidProduct = productData
                    , cidVariant = variantData
                    , cidQuantity = quantity
                    , cidTax = tax variantData
                    }
          getFinalPrice variant =
              fromMaybe (vdPrice variant) $ vdSalePrice variant



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
        subTotal =
            foldl (\acc item -> acc + itemTotal item) 0 items
        itemTotal item =
            fromIntegral (cidQuantity item) * fromCents (getVariantPrice $ cidVariant item)
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
                (bpdCategories $ cidProduct item)
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
                isValidProduct prod =
                     not . null $ bpdCategories prod `intersect` shippingMethodCategoryIds method
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
                productExcludesPriority prod =
                    not . null $ bpdCategories prod `intersect`
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
