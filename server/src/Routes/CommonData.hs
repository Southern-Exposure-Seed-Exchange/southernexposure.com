{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Routes.CommonData
    ( ProductData
    , getProductData
    , BaseProductData(bpdName, bpdShippingRestrictions)
    , makeBaseProductData
    , VariantData(vdId, vdProductId)
    , makeVariantData
    , getVariantPrice
    , applySalesToVariants
    , applyCategorySale
    , getAdditionalCategories
    , PredecessorCategory
    , categoryToPredecessor
    , CategoryData(cdDescription)
    , makeCategoryData
    , AdminCategorySelect(..)
    , makeAdminCategorySelect
    , makeAdminCategorySelects
    , validateCategorySelect
    , AuthorizationData
    , toAuthorizationData
    , LoginParameters(..)
    , validatePassword
    , PasswordValidationError(..)
    , handlePasswordValidationError
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
    , addressToAvalara
    , OrderDetails(..)
    , CheckoutOrder(..)
    , toCheckoutOrder
    , CheckoutProduct(..)
    , getCheckoutProducts
    ) where

import Prelude hiding (product)

import Control.Exception.Safe (Exception, throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, lift, asks, void, when)
import Data.Aeson ((.=), (.:), (.:?), ToJSON(..), FromJSON(..), object, withObject)
import Data.Digest.Pure.MD5 (md5)
import Data.Int (Int64)
import Data.List (intersect)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
    ( (=.), (==.), (>=.), (<=.), Entity(..), SelectOpt(Asc), selectList, getBy
    , update
    )
import Numeric.Natural (Natural)
import Servant (errBody, err500)

import Avalara (CreateTransactionRequest(..), AddressInfo(..))
import Cache (CategoryPredecessorCache, getCategoryPredecessorCache, queryCategoryPredecessorCache)
import Config
import Images
import Models
import Models.Fields
import Server
import Validation (Validation(..))

import qualified Avalara
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.StateCodes as StateCodes
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
        , bpdShippingRestrictions :: [Region]
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
            , "shippingRestrictions" .= bpdShippingRestrictions
            ]

makeBaseProductData :: (MonadReader Config m, MonadIO m) => Entity Product -> [Entity ProductToCategory] -> m BaseProductData
makeBaseProductData (Entity pId Product {..}) categories = do
    image <- makeSourceSet "products" $ T.unpack productImageUrl
    return BaseProductData
        { bpdId = pId
        , bpdName = productName
        , bpdSlug = productSlug
        , bpdBaseSku = productBaseSku
        , bpdLongDescription = productLongDescription
        , bpdImage = image
        , bpdCategories =
            productMainCategory
                : map (productToCategoryCategoryId . entityVal) categories
        , bpdShippingRestrictions = productShippingRestrictions
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
    categories <- getAdditionalCategories productId
    prod <- lift $ makeBaseProductData e categories
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
applyCategorySales product variants =
    getCategorySales product >>= \case
        [] ->
            return variants
        sale : _ ->
            return $ map (applyCategorySale sale) variants

-- | Apply the sale to the VariantData if there is no existing sale price
-- or if the sale price is cheaper than any existing sale price.
applyCategorySale :: CategorySale -> VariantData -> VariantData
applyCategorySale sale variant =
    let salePrice = categorySalePrice (vdPrice variant) sale
    in case vdSalePrice variant of
        Nothing ->
            variant { vdSalePrice = Just salePrice }
        Just existingSalePrice ->
            if existingSalePrice > salePrice then
                variant { vdSalePrice = Just salePrice }
            else
                variant

-- | Get any active sales for the given categories.
getCategorySales :: Entity Product -> AppSQL [CategorySale]
getCategorySales (Entity productId product) = do
    currentTime <- liftIO getCurrentTime
    secondaryCategories <- map (productToCategoryCategoryId . entityVal)
        <$> getAdditionalCategories productId
    categories <- lift $ concat <$>
        mapM (\c -> (c :) . map entityKey <$> getParentCategories c)
            (productMainCategory product : secondaryCategories)
    activeSales <- map entityVal <$> selectList
        [ CategorySaleStartDate <=. currentTime
        , CategorySaleEndDate >=. currentTime
        ]
        []
    return $ filter
        (not . null . L.intersect categories . categorySaleCategoryIds)
        activeSales

getAdditionalCategories :: ProductId -> AppSQL [Entity ProductToCategory]
getAdditionalCategories productId =
    selectList [ProductToCategoryProductId ==. productId] []



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


-- | Used in Category Select Dropdowns in the Admin site.
data AdminCategorySelect =
    AdminCategorySelect
        { acsId :: CategoryId
        , acsName :: T.Text
        } deriving (Show)

instance ToJSON AdminCategorySelect where
    toJSON AdminCategorySelect {..} =
        object
            [ "id" .= acsId
            , "name" .= acsName
            ]

-- | Build an AdminCategorySelect by prepending the parent category names
-- to the main category name.
makeAdminCategorySelect :: CategoryPredecessorCache -> Entity Category -> AdminCategorySelect
makeAdminCategorySelect cache (Entity cId cat) =
    prependParentNames AdminCategorySelect
        { acsId = cId
        , acsName = categoryName cat
        }
  where
    prependParentNames :: AdminCategorySelect -> AdminCategorySelect
    prependParentNames acs =
        let predecessors = queryCategoryPredecessorCache (acsId acs) cache
            newName =
                T.intercalate " > "
                    $ (++ [acsName acs])
                    $ map (categoryName . entityVal)
                    $ reverse predecessors
        in acs { acsName = newName}

-- | Build a list of AdminCategorySelects from every Category, using the
-- CategoryCache to prepend the Category names with their ancestors'.
makeAdminCategorySelects :: AppSQL [AdminCategorySelect]
makeAdminCategorySelects = do
    categoryCache <- lift $ readCache getCategoryPredecessorCache
    L.sortOn acsName . map (makeAdminCategorySelect categoryCache) <$> selectList [] []

-- | Validate a mutli-category select field by ensuring at least one
-- category is selected & all the Categories exist in the database.
validateCategorySelect :: Bool -> [CategoryId] -> App [(T.Text, [(T.Text, Bool)])]
validateCategorySelect isRequired categories =
    if null categories && isRequired then
        return [ ("", [ ("At least one Category is required.", True) ]) ]
    else
        V.indexedValidator "category" validateCategory categories
  where
    validateCategory :: CategoryId -> App [(T.Text, Bool)]
    validateCategory categoryId = do
        exists <- V.exists categoryId
        return [("Could not find this Category in the database.", exists)]


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


data LoginParameters =
    LoginParameters
        { lpEmail :: T.Text
        , lpPassword :: T.Text
        , lpCartToken :: Maybe T.Text
        , lpRemember :: Bool
        }

instance FromJSON LoginParameters where
    parseJSON =
        withObject "LoginParameters" $ \v ->
            LoginParameters
                <$> v .: "email"
                <*> v .: "password"
                <*> v .:? "sessionToken"
                <*> v .: "remember"

-- | Validate a login attempt by querying for a matching Customer and
-- comparing password hashes.
--
-- If normal BCrypt hashing fails, we also attempt ZenCart's hashing
-- scheme. If that passes, we upgrade the stored hash to BCrypt.
--
-- If the BCrypt hash matches but uses an outdated hashing policy, the
-- password hash will be upgraded to the current
-- 'BCrypt.slowerBcryptHashingPolicy'.
validatePassword :: T.Text -> T.Text -> AppSQL (Entity Customer)
validatePassword email password = do
    maybeCustomer <- getCustomerByEmail email
    case maybeCustomer of
        Just e@(Entity customerId customer) -> do
            when (T.null $ customerEncryptedPassword customer) resetRequiredError
            let hashedPassword =
                    encodeUtf8 $ customerEncryptedPassword customer
                isValid =
                    BCrypt.validatePassword hashedPassword
                        (encodeUtf8 password)
                usesPolicy =
                    BCrypt.hashUsesPolicy BCrypt.slowerBcryptHashingPolicy
                        hashedPassword
            if isValid && usesPolicy then
                return e
            else if isValid then
                makeNewPasswordHash customerId >> return e
            else
                validateZencartPassword e
        Nothing ->
            hashAnyways $ throw NoCustomer
  where
    resetRequiredError =
        void . hashAnyways $ throw PasswordResetRequired
    authorizationError =
        throw AuthorizationError
    -- Hash the password without trying to validate it to prevent
    -- mining of valid logins.
    hashAnyways :: AppSQL a -> AppSQL a
    hashAnyways returnValue = do
        hash <- liftIO . BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy
            $ encodeUtf8 password
        const returnValue $! hash
    -- Try to use Zencart's password hashing scheme to validate the
    -- user's password. If it is valid, upgrade to the BCrypt hashing
    -- scheme.
    validateZencartPassword :: Entity Customer -> AppSQL (Entity Customer)
    validateZencartPassword e@(Entity customerId customer) =
        case T.splitOn ":" (customerEncryptedPassword customer) of
            [passwordHash, salt] ->
                if T.length salt == 2 then
                    let isValid =
                            T.pack (show . md5 . LBS.fromStrict . encodeUtf8 $ salt <> password)
                                == passwordHash
                    in
                        if isValid then
                            makeNewPasswordHash customerId >> return e
                        else
                            authorizationError
                else
                    authorizationError
            _ ->
                authorizationError
    -- Upgrade the Customer's password by hashing it using the current
    -- BCrypt hashing policy.
    makeNewPasswordHash :: CustomerId -> AppSQL ()
    makeNewPasswordHash customerId = do
        maybeNewHash <- liftIO . BCrypt.hashPasswordUsingPolicy
            BCrypt.slowerBcryptHashingPolicy
            $ encodeUtf8 password
        newHash <- maybe
            (throw MisconfiguredHashingPolicy)
            (return . decodeUtf8) maybeNewHash
        update customerId [CustomerEncryptedPassword =. newHash]

data PasswordValidationError
    = AuthorizationError
    | PasswordResetRequired
    | NoCustomer
    | MisconfiguredHashingPolicy
    deriving (Show)

instance Exception PasswordValidationError

-- | Handle a PasswordError by throwing HTTP errors.
--
-- A 422 error will be thrown if the email/password are invalid. A 500
-- error will be thrown if the hashing policy is invalid.
handlePasswordValidationError :: PasswordValidationError -> App a
handlePasswordValidationError = \case
    AuthorizationError ->
        V.singleError "Invalid Email or Password."
    NoCustomer ->
        V.singleError "Invalid Email or Password."
    PasswordResetRequired ->
        V.singleError "Sorry, you need to reset your password before logging in."
    MisconfiguredHashingPolicy ->
        serverError $ err500 { errBody = "Misconfigured Hashing Policy" }


-- Carts


data CartItemData =
    CartItemData
        { cidItemId :: CartItemId
        , cidProduct :: BaseProductData
        , cidVariant :: VariantData
        , cidQuantity :: Natural
        }

instance ToJSON CartItemData where
    toJSON item =
        object [ "id" .= cidItemId item
               , "product" .= cidProduct item
               , "variant" .= cidVariant item
               , "quantity" .= cidQuantity item
               ]


data CartCharges =
    CartCharges
        { ccTax :: CartCharge
        , ccSurcharges :: [CartCharge]
        , ccShippingMethods :: [ShippingCharge]
        , ccPriorityShippingFee :: Maybe CartCharge
        , ccProductTotal :: Cents
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
               , "couponDiscount" .= ccCouponDiscount charges
               , "grandTotal" .= ccGrandTotal charges
               ]

data ShippingCharge =
    ShippingCharge
        { scCharge :: CartCharge
        , scPriorityFee :: Maybe PriorityShippingFee
        , scPriorityEnabled :: Bool
        } deriving (Show)

instance ToJSON ShippingCharge where
    toJSON charge =
        object [ "charge" .= scCharge charge
               , "priorityFee" .= scPriorityFee charge
               , "priorityEnabled" .= scPriorityEnabled charge
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
    :: (E.SqlExpr (Entity Cart) -> E.SqlExpr (E.Value Bool))        -- ^ The `E.where_` query
    -> AppSQL [CartItemData]
getCartItems whereQuery = do
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
            in do
                categories <- getAdditionalCategories (entityKey p)
                maybeCategorySale <- listToMaybe <$> getCategorySales p
                productData <- lift $ makeBaseProductData p categories
                variantData <- applyVariantSales v >>= \vd ->
                    return $ maybe vd (`applyCategorySale` vd) maybeCategorySale
                return CartItemData
                    { cidItemId = E.unValue i
                    , cidProduct = productData
                    , cidVariant = variantData
                    , cidQuantity = quantity
                    }



{-| Calculate the Charges for a set of Cart Items.

Shipping, & the Membership/Coupon Discounts are all based off of the
Product subtotal.

Tax is calculated using an Avalara 'SalesOrder'.

If the product subtotal does not meet the minimum order amount for
a coupon, no coupon discount will be applied. It is up to the calling code
to properly return an error for the user.

-}
getCharges
    :: Maybe AddressData        -- ^ Used to calculate the Shipping & Tax charge.
    -> Maybe Avalara.CustomerCode   -- ^ Optional customer code for tax exemption.
    -> [CartItemData]           -- ^ The cart items (see `getCartItems`)
    -> Maybe (Entity Coupon)    -- ^ A Coupon to apply.
    -> Bool                     -- ^ Include Priority S&H
    -> Bool                     -- ^ Include Avalara Tax Quote
    -> AppSQL CartCharges
getCharges maybeShipping maybeAvalaraCustomer items maybeCoupon priorityShipping includeAvalara =
    let
        maybeCountry =
            adCountry <$> maybeShipping
        subTotal =
            foldl (\acc item -> acc + itemTotal item) 0 items
        itemTotal item =
            fromIntegral (cidQuantity item) * fromCents (getVariantPrice $ cidVariant item)
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
        taxLine preTaxTotal =
            case adState <$> maybeShipping of
                Just (USState StateCodes.VA) ->
                    CartCharge
                        { ccDescription = "VA Sales Tax (5.3%)"
                        , ccAmount = applyVATax preTaxTotal
                        }
                _ ->
                    blankCharge
        sumGrandTotal :: AvalaraStatus -> CartCharges -> AppSQL CartCharges
        sumGrandTotal avalaraStatus cc =
            let preTaxTotal =
                    ccProductTotal cc
                        + sum (map ccAmount $ ccSurcharges cc)
                        + mAmt (listToMaybe . map scCharge . ccShippingMethods)
                        + mAmt ccPriorityShippingFee
                        - mAmt ccCouponDiscount
            in do
            taxCharge <- case avalaraStatus of
                AvalaraEnabled ->
                    if includeAvalara then
                        getTaxQuote cc
                    else
                        return blankCharge
                _ ->
                    return (taxLine preTaxTotal)
            return cc
                { ccGrandTotal = preTaxTotal + ccAmount taxCharge
                , ccTax = taxCharge
                }
            where mAmt f = maybe (Cents 0) ccAmount $ f cc
    in do
        avalaraStatus <- lift $ asks getAvalaraStatus
        surcharges <- getSurcharges items
        shippingMethods <- getShippingMethods maybeCountry items subTotal
        sumGrandTotal avalaraStatus CartCharges
            { ccTax = blankCharge
            , ccSurcharges = surcharges
            , ccShippingMethods = shippingMethods
            , ccPriorityShippingFee = priorityCharge shippingMethods
            , ccProductTotal = Cents subTotal
            , ccCouponDiscount = maybeCoupon >>= couponCredit shippingMethods
            , ccGrandTotal = 0
            }
  where
    blankCharge =
        CartCharge "" 0
    getTaxQuote :: CartCharges -> AppSQL CartCharge
    getTaxQuote cc = flip (maybe $ return blankCharge) maybeShipping $ \shippingAddress -> do
        date <- liftIO getCurrentTime
        sourceAddress <- fmap Avalara.addressFromLocation . lift
            $ asks getAvalaraSourceLocationCode
        let productLines =
                map makeProductLine items
            surchargeLines =
                map (makeLineItem SurchargeLine) $ ccSurcharges cc
            shippingLine =
                take 1 $ map (makeLineItem ShippingLine . scCharge)
                    $ ccShippingMethods cc
            priorityLine =
                take 1 $ map (makeLineItem PriorityShippingLine) $ maybeToList
                    $ ccPriorityShippingFee cc
            debitLines =
                surchargeLines <> shippingLine <> priorityLine
            discount =
                toDollars $ sum $ map ccAmount $ maybeToList $ ccCouponDiscount cc
            address =
                Avalara.Address
                    { Avalara.addrSingleLocation = Nothing
                    , Avalara.addrShipFrom = Just sourceAddress
                    , Avalara.addrShipTo = Just $ addressToAvalara shippingAddress
                    , Avalara.addrPointOfOrderOrigin = Nothing
                    , Avalara.addrPointOfOrderAcceptance = Nothing
                    }
            request =
                CreateTransactionRequest
                    { ctrCode = Nothing
                    , ctrLines = productLines <> debitLines
                    , ctrType = Just Avalara.SalesOrder
                    , ctrCompanyCode = Nothing
                    , ctrDate = date
                    , ctrCustomerCode = fromMaybe (Avalara.CustomerCode "SALES_ORDER") maybeAvalaraCustomer
                    , ctrDiscount =
                        if discount /= 0 then
                            Just discount
                        else
                            Nothing
                    , ctrAddresses = Just address
                    , ctrCommit = Just False
                    }
        lift (avalaraRequest $ Avalara.createTransaction request) >>= \case
            Avalara.ErrorResponse _ ->
                return blankCharge
            Avalara.HttpException _ ->
                return blankCharge
            Avalara.SuccessfulResponse quote ->
                case fromDollars <$> Avalara.tTotalTax quote of
                    Nothing ->
                        return blankCharge
                    Just tax ->
                        return $ CartCharge "Sales Tax" tax
    makeLineItem type_ charge =
        Avalara.LineItem
            { liNumber = Nothing
            , liQuantity = Just 1
            , liTotalAmount = Just $ toDollars $ ccAmount charge
            , liAddresses = Nothing
            , liTaxCode = lineItemToTaxCode type_
            , liItemCode = Nothing
            , liDiscounted = Just True
            , liTaxIncluded = Just False
            , liDescription = Just $ ccDescription charge
            }
    makeProductLine :: CartItemData -> Avalara.LineItem
    makeProductLine CartItemData {..} =
        let quantity =
                fromIntegral cidQuantity
            singlePrice =
                toDollars $ getVariantPrice cidVariant
            fullSku =
                bpdBaseSku cidProduct <> vdSkuSuffix cidVariant
        in
        Avalara.LineItem
            { liNumber = Nothing
            , liQuantity = Just quantity
            , liTotalAmount = Just $ quantity * singlePrice
            , liAddresses = Nothing
            , liTaxCode = Nothing
            , liItemCode = Just fullSku
            , liDiscounted = Just True
            , liTaxIncluded = Just False
            , liDescription = Just $ bpdName cidProduct
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
        ShippingCharge _ (Just fee) True:_ ->
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
                priorityEnabled =
                    shippingMethodIsPriorityEnabled method
                addPriorityFee charge =
                    if priorityExcluded || not priorityEnabled then
                        ShippingCharge charge Nothing priorityEnabled
                    else
                        ShippingCharge charge
                            (Just $ shippingMethodPriorityRate method)
                            priorityEnabled
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
        , adPhoneNumber :: T.Text
        , adIsDefault :: Bool
        } deriving (Eq, Show)

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
            <*> v .: "phoneNumber"
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
            , "phoneNumber" .= adPhoneNumber address
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
                , ( "phoneNumber", [ V.required $ adPhoneNumber address ])
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
        , addressPhoneNumber = adPhoneNumber address
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
        , adPhoneNumber = addressPhoneNumber address
        , adIsDefault = addressIsDefault address
        }

-- | Transform an 'AddressData' into an Avalara Address.
addressToAvalara :: AddressData -> Avalara.AddressInfo
addressToAvalara AddressData {..} =
    AddressInfo
        { aiLocationCode = Nothing
        , aiLineOne = Just adAddressOne
        , aiLineTwo = Just adAddressTwo
        , aiLineThree = Nothing
        , aiCity = Just adCity
        , aiRegion = Just $ avalaraRegion adState
        , aiCountry = Just . T.pack . show $ fromCountry adCountry
        , aiPostalCode = Just adZipCode
        , aiLatitude = Nothing
        , aiLongitude = Nothing
        }


-- Order Details


data OrderDetails =
    OrderDetails
        { odOrder :: CheckoutOrder
        , odLineItems :: [Entity OrderLineItem]
        , odProducts :: [CheckoutProduct]
        , odShippingAddress :: AddressData
        , odBillingAddress :: Maybe AddressData
        } deriving (Show)

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
        { coId :: OrderId
        , coStatus :: OrderStatus
        , coComment :: T.Text
        , coCreatedAt :: UTCTime
        } deriving (Show)

instance ToJSON CheckoutOrder where
    toJSON order =
        object
            [ "id" .= coId order
            , "status" .= coStatus order
            , "comment" .= coComment order
            , "createdAt" .= coCreatedAt order
            ]

toCheckoutOrder :: Entity Order -> CheckoutOrder
toCheckoutOrder (Entity orderId order) =
    CheckoutOrder
        { coId = orderId
        , coStatus = orderStatus order
        , coComment = orderCustomerComment order
        , coCreatedAt = orderCreatedAt order
        }

data CheckoutProduct =
    CheckoutProduct
        { cpName :: T.Text
        , cpSku :: T.Text
        , cpLotSize :: Maybe LotSize
        , cpQuantity :: Natural
        , cpPrice :: Cents
        } deriving (Show)

instance ToJSON CheckoutProduct where
    toJSON prod =
        object
            [ "name" .= cpName prod
            , "sku" .= cpSku prod
            , "lotSize" .= cpLotSize prod
            , "quantity" .= cpQuantity prod
            , "price" .= cpPrice prod
            ]

getCheckoutProducts :: OrderId -> AppSQL [CheckoutProduct]
getCheckoutProducts orderId = do
    orderProducts <- E.select $ E.from $
        \(op `E.InnerJoin` v `E.InnerJoin` p) -> do
            E.on $ p E.^. ProductId E.==. v E.^. ProductVariantProductId
            E.on $ v E.^. ProductVariantId E.==. op E.^. OrderProductProductVariantId
            E.where_ $ op E.^. OrderProductOrderId E.==. E.val orderId
            return (op, v , p)
    return $ map makeCheckoutProduct orderProducts
    where makeCheckoutProduct (Entity _ orderProd, Entity _ variant, Entity _ product) =
            CheckoutProduct
                { cpName = productName product
                , cpSku = productBaseSku product <> productVariantSkuSuffix variant
                , cpLotSize = productVariantLotSize variant
                , cpQuantity = orderProductQuantity orderProd
                , cpPrice = orderProductPrice orderProd
                }
