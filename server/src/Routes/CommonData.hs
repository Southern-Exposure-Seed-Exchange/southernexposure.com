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
    , CartCharge(..)
    , AddressData(..)
    , fromAddressData
    , toAddressData
    ) where

import Data.Aeson ((.=), (.:), (.:?), ToJSON(..), FromJSON(..), object, withObject)
import Data.List (intersect)
import Data.Maybe (mapMaybe)
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


data CartCharges =
    CartCharges
        { ccTax :: CartCharge
        , ccSurcharges :: [CartCharge]
        , ccShippingMethods :: [CartCharge]
        }

instance ToJSON CartCharges where
    toJSON charges =
        object [ "tax" .= ccTax charges
               , "surcharges" .= ccSurcharges charges
               , "shippingMethods" .= ccShippingMethods charges
               ]


data CartCharge =
    CartCharge
        { ccDescription :: T.Text
        , ccAmount :: Cents
        }

instance ToJSON CartCharge where
    toJSON charge =
        object [ "description" .= ccDescription charge
               , "amount" .= ccAmount charge
               ]

getCartItems :: Maybe TaxRate -> (E.SqlExpr (Entity Cart) -> E.SqlExpr (E.Value Bool)) -> AppSQL [CartItemData]
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


getCharges :: Maybe TaxRate -> Maybe Country -> [CartItemData] -> AppSQL CartCharges
getCharges maybeTaxRate maybeCountry items =
    let
        variant item =
            (\(Entity _ v) -> v) $ cidVariant item
        subTotal =
            foldl (\acc item -> acc + itemTotal item) 0 items
        itemTotal item =
            fromIntegral (cidQuantity item) * fromCents (productVariantPrice $ variant item)
        taxTotal =
            foldl (\acc item -> acc + fromCents (cidTax item)) 0 items
    in do
        surcharges <- getSurcharges items
        shippingMethods <- getShippingMethods maybeCountry items subTotal
        return $ CartCharges
            (CartCharge (maybe "" taxRateDescription maybeTaxRate) $ Cents taxTotal)
            surcharges shippingMethods


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

getShippingMethods :: Maybe Country -> [CartItemData] -> Natural -> AppSQL [CartCharge]
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
            in
                if validCountry && validProducts then
                    Just . CartCharge (shippingMethodDescription method)
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
