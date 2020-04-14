{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Products
    ( ProductAPI
    , productRoutes
    ) where

import Control.Monad (forM_, unless)
import Control.Monad.Reader (liftIO, lift)
import Data.Aeson ((.=), (.:), (.:?), ToJSON(..), FromJSON(..), Value(Object), object, withObject)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
    ( (==.), (=.), Entity(..), SelectOpt(Asc), selectList, selectFirst
    , insert, insertMany_, insert_, update, get, getBy, deleteWhere
    )
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Capture, Get, Post, JSON, err404)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Models
    ( EntityField(..), Product(..), ProductId, ProductVariant(..), ProductVariantId
    , SeedAttribute(..), Category(..), CategoryId, Unique(..), slugify
    , ProductToCategory(..)
    )
import Models.Fields (LotSize, Cents)
import Routes.CommonData
    ( AdminCategorySelect, makeAdminCategorySelects, validateCategorySelect
    , getAdditionalCategories
    )
import Routes.Utils (activeVariantExists, makeImageFromBase64, sanitize)
import Server (App, AppSQL, runDB, serverError)
import Validation (Validation(validators))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E
import qualified Validation as V


type ProductAPI =
         "list" :> ProductListRoute
    :<|> "data" :> SharedProductDataRoute
    :<|> "new" :> NewProductRoute
    :<|> "edit" :> EditProductDataRoute
    :<|> "edit" :> EditProductRoute

type ProductRoutes =
         (WrappedAuthToken -> App (Cookied ProductListData))
    :<|> (WrappedAuthToken -> App (Cookied SharedProductData))
    :<|> (WrappedAuthToken -> ProductParameters -> App (Cookied ProductId))
    :<|> (WrappedAuthToken -> ProductId -> App (Cookied EditProductData))
    :<|> (WrappedAuthToken -> EditProductParameters -> App (Cookied ProductId))

productRoutes :: ProductRoutes
productRoutes =
         productListRoute
    :<|> sharedProductDataRoute
    :<|> newProductRoute
    :<|> editProductDataRoute
    :<|> editProductRoute


-- LIST


type ProductListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ProductListData)

newtype ProductListData =
    ProductListData
        { pldProducts :: [ListProduct]
        } deriving (Show)

instance ToJSON ProductListData where
    toJSON ProductListData {..} =
        object
            [ "products" .= pldProducts
            ]

data ListProduct =
    ListProduct
        { lpId :: ProductId
        , lpName :: T.Text
        , lpBaseSku :: T.Text
        , lpCategories :: [T.Text]
        , lpIsActive :: Bool
        } deriving (Show)

instance ToJSON ListProduct where
    toJSON ListProduct {..} =
        object
            [ "id" .= lpId
            , "name" .= lpName
            , "baseSKU" .= lpBaseSku
            , "categories" .= lpCategories
            , "isActive" .= lpIsActive
            ]

productListRoute :: WrappedAuthToken -> App (Cookied ProductListData)
productListRoute t = withAdminCookie t $ \_ -> runDB $ do
    categoryNameMap <- makeNameMap <$> selectList [] []
    ProductListData . map (makeProduct categoryNameMap) <$> getProducts
  where
    makeNameMap :: [Entity Category] -> M.Map CategoryId T.Text
    makeNameMap =
        foldr (\(Entity cId c) m -> M.insert cId (categoryName c) m) M.empty
    getProducts :: AppSQL [(Entity Product, E.Value Bool)]
    getProducts =
        E.select $ E.from $ \p -> do
            E.orderBy [E.asc $ p E.^. ProductBaseSku]
            return (p, activeVariantExists p )
    makeProduct :: M.Map CategoryId T.Text -> (Entity Product, E.Value Bool) -> ListProduct
    makeProduct nameMap (Entity pId prod, isActive) =
        ListProduct
            { lpId = pId
            , lpName = productName prod
            , lpBaseSku = productBaseSku prod
            , lpCategories = (: []) . fromMaybe "" $ productMainCategory prod `M.lookup` nameMap
            , lpIsActive = E.unValue isActive
            }


-- NEW / EDIT COMMON DATA


type SharedProductDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied SharedProductData)

newtype SharedProductData =
    SharedProductData
        { spdCategories :: [AdminCategorySelect]
        } deriving (Show)

instance ToJSON SharedProductData where
    toJSON SharedProductData {..} =
        object [ "categories" .= spdCategories ]

sharedProductDataRoute :: WrappedAuthToken -> App (Cookied SharedProductData)
sharedProductDataRoute t = withAdminCookie t $ \_ ->
    SharedProductData <$> runDB makeAdminCategorySelects


data ProductParameters =
    ProductParameters
        { ppName :: T.Text
        , ppSlug :: T.Text
        , ppCategories :: [CategoryId]
        , ppBaseSku :: T.Text
        , ppLongDescription :: T.Text
        , ppImageData :: BS.ByteString
        -- ^ Base64 Encoded
        , ppImageName :: T.Text
        , ppKeywords :: T.Text
        , ppVariantData :: [VariantData]
        , ppSeedAttribute :: Maybe SeedData
        } deriving (Show)

instance FromJSON ProductParameters where
    parseJSON = withObject "ProductParameters" $ \v -> do
        ppName <- v .: "name"
        ppSlug <- v .: "slug"
        ppCategories <- v .: "categories"
        ppBaseSku <- v .: "baseSku"
        ppLongDescription <- v .: "longDescription"
        ppImageData <- encodeUtf8 <$> v .: "imageData"
        ppImageName <- v .: "imageName"
        ppKeywords <- fromMaybe "" <$> (v .:? "keywords")
        ppVariantData <- v .: "variants"
        ppSeedAttribute <- v .: "seedAttributes"
        return ProductParameters {..}

instance Validation ProductParameters where
    validators ProductParameters {..} = do
        slugDoesntExist <- V.doesntExist $ UniqueProductSlug ppSlug
        skuDoesntExist <- V.doesntExist $ UniqueBaseSku ppBaseSku
        categoryValidations <- validateCategorySelect True ppCategories
        return $
            [ ( ""
              , [ ("At least one Variant is required.", null ppVariantData) ]
              )
            , ( "name"
              , [ V.required ppName ]
              )
            , ( "slug"
              ,  [ V.required ppSlug
                 , ( "A Product with this Slug already exists."
                   , slugDoesntExist
                   )
                 ]
              )
            , ( "baseSku"
              , [ V.required ppBaseSku
                , ( "A Product with this SKU already exists."
                  , skuDoesntExist
                  )
                ]
              )
            ]
            ++ getDuplicateSuffixErrors ppVariantData
            ++ categoryValidations

data VariantData =
    VariantData
        { vdSkuSuffix :: T.Text
        , vdPrice :: Cents
        , vdQuantity :: Int
        , vdLotSize :: Maybe LotSize
        , vdIsActive :: Bool
        , vdId :: Maybe ProductVariantId
        -- ^ Only used in the EditProductRoute
        } deriving (Show)

instance FromJSON VariantData where
    parseJSON = withObject "VariantData" $ \v -> do
        vdSkuSuffix <- v .: "skuSuffix"
        vdPrice <- v .: "price"
        vdQuantity <- v .: "quantity"
        vdLotSize <- v .: "lotSize"
        vdIsActive <- v .: "isActive"
        vdId <- v .: "id"
        return VariantData {..}

instance ToJSON VariantData where
    toJSON VariantData {..} =
        object
            [ "id" .= vdId
            , "skuSuffix" .= vdSkuSuffix
            , "price" .= vdPrice
            , "quantity" .= vdQuantity
            , "lotSize" .= vdLotSize
            , "isActive" .= vdIsActive
            ]

data SeedData =
    SeedData
        { sdOrganic :: Bool
        , sdHeirloom :: Bool
        , sdSmallGrower :: Bool
        , sdRegional :: Bool
        } deriving (Show)

instance FromJSON SeedData where
    parseJSON = withObject "SeedData" $ \v -> do
        sdOrganic <- v .: "organic"
        sdHeirloom <- v .: "heirloom"
        sdSmallGrower <- v .: "smallGrower"
        sdRegional <- v .: "regional"
        return SeedData {..}

instance ToJSON SeedData where
    toJSON SeedData {..} =
        object
            [ "organic" .= sdOrganic
            , "heirloom" .= sdHeirloom
            , "smallGrower" .= sdSmallGrower
            , "regional" .= sdRegional
            ]


-- NEW


type NewProductRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] ProductParameters
    :> Post '[JSON] (Cookied ProductId)

newProductRoute :: WrappedAuthToken -> ProductParameters -> App (Cookied ProductId)
newProductRoute = validateAdminAndParameters $ \_ p@ProductParameters {..} -> do
    time <- liftIO getCurrentTime
    imageFileName <- makeImageFromBase64 "products" ppImageName ppImageData
    runDB $ do
        (prod, extraCategories) <- makeProduct p imageFileName time
        productId <- insert prod
        insertMany_ $ map (ProductToCategory productId) extraCategories
        insertMany_ $ map (makeVariant productId) ppVariantData
        maybe (return ()) (insert_ . makeAttributes productId) ppSeedAttribute
        return productId
  where
    makeProduct :: ProductParameters -> T.Text -> UTCTime -> AppSQL (Product, [CategoryId])
    makeProduct ProductParameters {..} imageUrl time =
        case ppCategories of
            main : rest -> return
                ( Product
                    { productName = sanitize ppName
                    , productSlug = slugify ppSlug
                    , productMainCategory = main
                    , productBaseSku = ppBaseSku
                    , productShortDescription = ""
                    , productLongDescription = sanitize ppLongDescription
                    , productImageUrl = imageUrl
                    , productKeywords = ppKeywords
                    , productCreatedAt = time
                    , productUpdatedAt = time
                    }
                , rest
                )
            [] ->
                lift $ V.singleError "At least one Category is required."


-- EDIT


type EditProductDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" ProductId
    :> Get '[JSON] (Cookied EditProductData)

data EditProductData =
    EditProductData
        { epdId :: ProductId
        , epdName :: T.Text
        , epdSlug :: T.Text
        , epdCategories :: [CategoryId]
        , epdBaseSku :: T.Text
        , epdLongDescription :: T.Text
        , epdImageUrl :: T.Text
        , epdKeywords :: T.Text
        , epdVariantData :: [VariantData]
        , epdSeedAttribute :: Maybe SeedData
        } deriving (Show)

instance ToJSON EditProductData where
    toJSON EditProductData {..} =
        object
            [ "id" .= epdId
            , "name" .= epdName
            , "slug" .= epdSlug
            , "categories" .= epdCategories
            , "baseSku" .= epdBaseSku
            , "longDescription" .= epdLongDescription
            , "imageUrl" .= epdImageUrl
            , "keywords" .= epdKeywords
            , "variants" .= epdVariantData
            , "seedAttributes" .= epdSeedAttribute
            ]

editProductDataRoute :: WrappedAuthToken -> ProductId -> App (Cookied EditProductData)
editProductDataRoute t productId = withAdminCookie t $ \_ ->
    runDB (get productId) >>= \case
        Nothing ->
            serverError err404
        Just Product {..} -> runDB $ do
            variants <- map makeVariantData
                <$> selectList
                    [ProductVariantProductId ==. productId]
                    [Asc ProductVariantSkuSuffix]
            seedAttr <- fmap makeAttributeData <$> getBy (UniqueAttribute productId)
            categories <- getAdditionalCategories productId
            return EditProductData
                { epdId = productId
                , epdName = productName
                , epdSlug = productSlug
                , epdCategories = productMainCategory
                    : map (productToCategoryCategoryId . entityVal) categories
                , epdBaseSku = productBaseSku
                , epdLongDescription = productLongDescription
                , epdImageUrl = productImageUrl
                , epdKeywords = productKeywords
                , epdVariantData = variants
                , epdSeedAttribute = seedAttr
                }
  where
    makeVariantData :: Entity ProductVariant -> VariantData
    makeVariantData (Entity variantId ProductVariant {..}) =
        VariantData
            { vdId = Just variantId
            , vdSkuSuffix = productVariantSkuSuffix
            , vdPrice = productVariantPrice
            , vdQuantity = fromIntegral productVariantQuantity
            , vdLotSize = productVariantLotSize
            , vdIsActive = productVariantIsActive
            }
    makeAttributeData :: Entity SeedAttribute -> SeedData
    makeAttributeData (Entity _ SeedAttribute {..}) =
        SeedData
            { sdOrganic = seedAttributeIsOrganic
            , sdHeirloom = seedAttributeIsHeirloom
            , sdRegional = seedAttributeIsRegional
            , sdSmallGrower = seedAttributeIsSmallGrower
            }



type EditProductRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] EditProductParameters
    :> Post '[JSON] (Cookied ProductId)


data EditProductParameters =
    EditProductParameters
        { eppId :: ProductId
        , eppProduct :: ProductParameters
        }

instance FromJSON EditProductParameters where
    parseJSON = withObject "EditProductParameters" $ \v -> do
        eppId <- v .: "id"
        eppProduct <- parseJSON $ Object v
        return EditProductParameters {..}

instance Validation EditProductParameters where
    validators EditProductParameters {..} = do
        let ProductParameters {..} = eppProduct
        productExists <- V.exists eppId
        categoryValidations <- validateCategorySelect True ppCategories
        return $
            [ ( ""
              , [ ("Could not find this product in the database.", productExists)
                , ("At least one Variant is required.", null ppVariantData)
                ]
              )
            , ( "name", [ V.required ppName ] )
            , ( "slug", [ V.required ppSlug ] )
            , ( "baseSku", [ V.required ppBaseSku ])
            ]
            ++ getDuplicateSuffixErrors ppVariantData
            ++ categoryValidations


editProductRoute :: WrappedAuthToken -> EditProductParameters -> App (Cookied ProductId)
editProductRoute = validateAdminAndParameters $ \_ EditProductParameters {..} -> do
    let ProductParameters {..} = eppProduct
    imageUpdate <-
        if not (BS.null ppImageData) && not (T.null ppImageName) then do
            imageFileName <- makeImageFromBase64 "products" ppImageName ppImageData
            return [ ProductImageUrl =. imageFileName ]
        else
            return []
    time <- liftIO getCurrentTime
    (mainCategory, extraCategories) <-
        case ppCategories of
            [] ->
                V.singleError "At least one Category is required."
            main : rest ->
                return (main, rest)
    runDB $ do
        update eppId $
            [ ProductName =. sanitize ppName
            , ProductSlug =. slugify ppSlug
            , ProductMainCategory =. mainCategory
            , ProductBaseSku =. ppBaseSku
            , ProductLongDescription =. sanitize ppLongDescription
            , ProductKeywords =. ppKeywords
            , ProductUpdatedAt =. time
            ] ++ imageUpdate
        deleteWhere [ProductToCategoryProductId ==. eppId]
        insertMany_ $ map (ProductToCategory eppId) extraCategories
        (ppSeedAttribute,) <$> selectFirst [SeedAttributeProductId ==. eppId] [] >>= \case
            (Just seedData, Nothing) ->
                insert_ $ makeAttributes eppId seedData
            (Just SeedData {..}, Just (Entity attrId _)) ->
                update attrId
                    [ SeedAttributeProductId =. eppId
                    , SeedAttributeIsOrganic =. sdOrganic
                    , SeedAttributeIsHeirloom =. sdHeirloom
                    , SeedAttributeIsRegional =. sdRegional
                    , SeedAttributeIsSmallGrower =. sdSmallGrower
                    ]
            _ ->
                return ()
        forM_ ppVariantData $ \variant@VariantData {..} ->
            case vdId of
                Nothing ->
                    insert_ $ makeVariant eppId variant
                Just variantId -> do
                    update variantId
                        [ ProductVariantProductId =. eppId
                        , ProductVariantSkuSuffix =. vdSkuSuffix
                        , ProductVariantPrice =. vdPrice
                        , ProductVariantQuantity =. fromIntegral vdQuantity
                        , ProductVariantLotSize =. vdLotSize
                        , ProductVariantIsActive =. vdIsActive
                        ]
                    unless vdIsActive $
                        deleteWhere [CartItemProductVariantId ==. variantId]
    return eppId


-- UTILS


makeVariant :: ProductId -> VariantData -> ProductVariant
makeVariant pId VariantData {..} =
    ProductVariant
        { productVariantProductId = pId
        , productVariantSkuSuffix = vdSkuSuffix
        , productVariantPrice = vdPrice
        , productVariantQuantity = fromIntegral vdQuantity
        , productVariantLotSize = vdLotSize
        , productVariantIsActive = vdIsActive
        }

makeAttributes :: ProductId -> SeedData -> SeedAttribute
makeAttributes pId SeedData {..} =
    SeedAttribute
        { seedAttributeProductId = pId
        , seedAttributeIsOrganic = sdOrganic
        , seedAttributeIsHeirloom = sdHeirloom
        , seedAttributeIsSmallGrower = sdSmallGrower
        , seedAttributeIsRegional = sdRegional
        }

getDuplicateSuffixErrors :: [VariantData] -> [(T.Text, [(T.Text, Bool)])]
getDuplicateSuffixErrors variants =
        let duplicateSuffixes =
                mapMaybe (\l -> if length l > 1 then listToMaybe l else Nothing)
                    $ L.group
                    $ L.sort
                    $ map vdSkuSuffix variants
        in checkDuplicateSuffix duplicateSuffixes
  where
    checkDuplicateSuffix dupes =
        let duplicateIndexes =
                L.findIndices ((`elem` dupes) . vdSkuSuffix) variants
        in
            map (\i ->
                    ( "variant-" <> T.pack (show i) <> "-skuSuffix"
                    , [ ("SKU suffixes must be unique.", True) ]
                    )
                )
                duplicateIndexes
