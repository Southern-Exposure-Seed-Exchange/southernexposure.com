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

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), Value(Object), object, withObject)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
    ( (==.), (=.), Entity(..), selectList, selectFirst, insert, insertMany_
    , insert_, update, get, getBy
    )
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Capture, Get, Post, JSON, err404)
import Text.HTML.SanitizeXSS (sanitize)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Config (Config(getCaches))
import Cache (Caches(getCategoryPredecessorCache), CategoryPredecessorCache, queryCategoryPredecessorCache)
import Models
    ( EntityField(..), Product(..), ProductId, ProductVariant(..), ProductVariantId
    , SeedAttribute(..), Category(..), CategoryId, Unique(..), slugify
    )
import Models.Fields (LotSize, Cents)
import Routes.Utils (activeVariantExists, makeImageFromBase64)
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
        E.select $ E.from $ \p ->
            return (p, activeVariantExists p )
    makeProduct :: M.Map CategoryId T.Text -> (Entity Product, E.Value Bool) -> ListProduct
    makeProduct nameMap (Entity pId prod, isActive) =
        ListProduct
            { lpId = pId
            , lpName = productName prod
            , lpBaseSku = productBaseSku prod
            , lpCategories = mapMaybe (`M.lookup` nameMap) $ productCategoryIds prod
            , lpIsActive = E.unValue isActive
            }


-- NEW / EDIT COMMON DATA


type SharedProductDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied SharedProductData)

newtype SharedProductData =
    SharedProductData
        { spdCategories :: [ProductCategory]
        } deriving (Show)

instance ToJSON SharedProductData where
    toJSON SharedProductData {..} =
        object [ "categories" .= spdCategories ]

data ProductCategory =
    ProductCategory
        { pcId :: CategoryId
        , pcName :: T.Text
        } deriving (Show)

instance ToJSON ProductCategory where
    toJSON ProductCategory {..} =
        object
            [ "id" .= pcId
            , "name" .= pcName
            ]

sharedProductDataRoute :: WrappedAuthToken -> App (Cookied SharedProductData)
sharedProductDataRoute t = withAdminCookie t $ \_ -> do
    categories <- fmap (map makeProductCategory) . runDB $ selectList [] []
    categoryCache <- asks getCaches >>= fmap getCategoryPredecessorCache . liftIO . readTVarIO
    return . SharedProductData . L.sortOn pcName $ map (prependParentNames categoryCache) categories
  where
    makeProductCategory :: Entity Category -> ProductCategory
    makeProductCategory (Entity cId cat) =
        ProductCategory
            { pcId = cId
            , pcName = categoryName cat
            }
    prependParentNames :: CategoryPredecessorCache -> ProductCategory -> ProductCategory
    prependParentNames cache pc =
        let predecessors = queryCategoryPredecessorCache (pcId pc) cache
            newName =
                T.intercalate " > "
                    $ (++ [pcName pc])
                    $ map (categoryName . entityVal)
                    $ reverse predecessors
        in pc { pcName = newName}


data ProductParameters =
    ProductParameters
        { ppName :: T.Text
        , ppSlug :: T.Text
        , ppCategory :: CategoryId
        , ppBaseSku :: T.Text
        , ppLongDescription :: T.Text
        , ppImageData :: BS.ByteString
        -- ^ Base64 Encoded
        , ppImageName :: T.Text
        , ppVariantData :: [VariantData]
        , ppSeedAttribute :: Maybe SeedData
        } deriving (Show)

instance FromJSON ProductParameters where
    parseJSON = withObject "ProductParameters" $ \v -> do
        ppName <- v .: "name"
        ppSlug <- v .: "slug"
        ppCategory <- v .: "category"
        ppBaseSku <- v .: "baseSku"
        ppLongDescription <- v .: "longDescription"
        ppImageData <- encodeUtf8 <$> v .: "imageData"
        ppImageName <- v .: "imageName"
        ppVariantData <- v .: "variants"
        ppSeedAttribute <- v .: "seedAttributes"
        return ProductParameters {..}

instance Validation ProductParameters where
    validators ProductParameters {..} = do
        slugDoesntExist <- V.doesntExist $ UniqueProductSlug ppSlug
        skuDoesntExist <- V.doesntExist $ UniqueBaseSku ppBaseSku
        categoryExists <- V.exists ppCategory
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
            , ( "category"
              , [ ( "Could not find this Category in the database."
                  , categoryExists)
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
        productId <- insert $ makeProduct p imageFileName time
        insertMany_ $ map (makeVariant productId) ppVariantData
        maybe (return ()) (insert_ . makeAttributes productId) ppSeedAttribute
        return productId
  where
    makeProduct :: ProductParameters -> T.Text -> UTCTime -> Product
    makeProduct ProductParameters {..} imageUrl time =
        Product
            { productName = sanitize ppName
            , productSlug = slugify ppSlug
            , productCategoryIds = [ppCategory]
            , productBaseSku = ppBaseSku
            , productShortDescription = ""
            , productLongDescription = sanitize ppLongDescription
            , productImageUrl = imageUrl
            , productCreatedAt = time
            , productUpdatedAt = time
            }


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
        , epdCategory :: Maybe CategoryId
        , epdBaseSku :: T.Text
        , epdLongDescription :: T.Text
        , epdImageUrl :: T.Text
        , epdVariantData :: [VariantData]
        , epdSeedAttribute :: Maybe SeedData
        } deriving (Show)

instance ToJSON EditProductData where
    toJSON EditProductData {..} =
        object
            [ "id" .= epdId
            , "name" .= epdName
            , "slug" .= epdSlug
            , "category" .= epdCategory
            , "baseSku" .= epdBaseSku
            , "longDescription" .= epdLongDescription
            , "imageUrl" .= epdImageUrl
            , "variants" .= epdVariantData
            , "seedAttributes" .= epdSeedAttribute
            ]

editProductDataRoute :: WrappedAuthToken -> ProductId -> App (Cookied EditProductData)
editProductDataRoute t productId = withAdminCookie t $ \_ ->
    runDB (get productId) >>= \case
        Nothing ->
            serverError err404
        Just Product {..} -> runDB $ do
            variants <- map makeVariantData <$> selectList [ProductVariantProductId ==. productId] []
            seedAttr <- fmap makeAttributeData <$> getBy (UniqueAttribute productId)
            return EditProductData
                { epdId = productId
                , epdName = productName
                , epdSlug = productSlug
                , epdCategory = listToMaybe productCategoryIds
                , epdBaseSku = productBaseSku
                , epdLongDescription = productLongDescription
                , epdImageUrl = productImageUrl
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
        categoryExists <- V.exists ppCategory
        return $
            [ ( ""
              , [ ("Could not find this product in the database.", productExists)
                , ("At least one Variant is required.", null ppVariantData)
                ]
              )
            , ( "name", [ V.required ppName ] )
            , ( "slug", [ V.required ppSlug ] )
            , ( "category"
              , [ ( "Could not find this Category in the database."
                  , categoryExists)
                ]
              )
            , ( "baseSku", [ V.required ppBaseSku ])
            ]
            ++ getDuplicateSuffixErrors ppVariantData


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
    runDB $ do
        update eppId $
            [ ProductName =. sanitize ppName
            , ProductSlug =. slugify ppSlug
            , ProductCategoryIds =. [ppCategory]
            , ProductBaseSku =. ppBaseSku
            , ProductLongDescription =. sanitize ppLongDescription
            , ProductUpdatedAt =. time
            ] ++ imageUpdate
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
                Just variantId ->
                    update variantId
                        [ ProductVariantProductId =. eppId
                        , ProductVariantSkuSuffix =. vdSkuSuffix
                        , ProductVariantPrice =. vdPrice
                        , ProductVariantQuantity =. fromIntegral vdQuantity
                        , ProductVariantLotSize =. vdLotSize
                        , ProductVariantIsActive =. vdIsActive
                        ]
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
