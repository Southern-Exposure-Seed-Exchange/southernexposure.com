{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Products
    ( ProductAPI
    , productRoutes
    ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (Entity(..), SelectOpt(Asc), selectList, insert, insertMany_, insert_)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)
import Text.HTML.SanitizeXSS (sanitize)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Config (Config(getCaches))
import Cache (Caches(getCategoryPredecessorCache), CategoryPredecessorCache, queryCategoryPredecessorCache)
import Models
    ( EntityField(..), Product(..), ProductId, ProductVariant(..)
    , SeedAttribute(..), Category(..), CategoryId, Unique(..), slugify
    )
import Models.Fields (LotSize, Cents)
import Routes.Utils (makeImageFromBase64)
import Server (App, runDB)
import Validation (Validation(validators))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Validation as V


type ProductAPI =
         "list" :> ProductListRoute
    :<|> "new" :> NewProductDataRoute
    :<|> "new" :> NewProductRoute

type ProductRoutes =
         (WrappedAuthToken -> App (Cookied ProductListData))
    :<|> (WrappedAuthToken -> App (Cookied NewProductData))
    :<|> (WrappedAuthToken -> NewProductParameters -> App (Cookied ProductId))

productRoutes :: ProductRoutes
productRoutes =
         productListRoute
    :<|> newProductDataRoute
    :<|> newProductRoute


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
    ProductListData . map (makeProduct categoryNameMap) <$> selectList [] [Asc ProductBaseSku]
  where
    makeNameMap :: [Entity Category] -> M.Map CategoryId T.Text
    makeNameMap =
        foldr (\(Entity cId c) m -> M.insert cId (categoryName c) m) M.empty
    makeProduct :: M.Map CategoryId T.Text -> Entity Product -> ListProduct
    makeProduct nameMap (Entity pId prod) =
        ListProduct
            { lpId = pId
            , lpName = productName prod
            , lpBaseSku = productBaseSku prod
            , lpCategories = mapMaybe (`M.lookup` nameMap) $ productCategoryIds prod
            , lpIsActive = productIsActive prod
            }


-- NEW


type NewProductDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied NewProductData)

newtype NewProductData =
    NewProductData
        { npdCategories :: [ProductCategory]
        } deriving (Show)

instance ToJSON NewProductData where
    toJSON NewProductData {..} =
        object [ "categories" .= npdCategories ]

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

newProductDataRoute :: WrappedAuthToken -> App (Cookied NewProductData)
newProductDataRoute t = withAdminCookie t $ \_ -> do
    categories <- fmap (map makeProductCategory) . runDB $ selectList [] []
    categoryCache <- asks getCaches >>= fmap getCategoryPredecessorCache . liftIO . readTVarIO
    return . NewProductData . L.sortOn pcName $ map (prependParentNames categoryCache) categories
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


type NewProductRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] NewProductParameters
    :> Post '[JSON] (Cookied ProductId)

data NewProductParameters =
    NewProductParameters
        { nppName :: T.Text
        , nppSlug :: T.Text
        , nppCategory :: CategoryId
        , nppBaseSku :: T.Text
        , nppLongDescription :: T.Text
        , nppIsActive :: Bool
        , nppImageData :: BS.ByteString
        -- ^ Base64 Encoded
        , nppImageName :: T.Text
        , nppVariantData :: [NewVariantData]
        , nppSeedAttribute :: Maybe NewSeedData
        }

instance FromJSON NewProductParameters where
    parseJSON = withObject "NewProductParameters" $ \v -> do
        nppName <- v .: "name"
        nppSlug <- v .: "slug"
        nppCategory <- v .: "category"
        nppBaseSku <- v .: "baseSku"
        nppLongDescription <- v .: "longDescription"
        nppIsActive <- v .: "isActive"
        nppImageData <- encodeUtf8 <$> v .: "imageData"
        nppImageName <- v .: "imageName"
        nppVariantData <- v .: "variants"
        nppSeedAttribute <- v .: "seedAttributes"
        return NewProductParameters {..}

instance Validation NewProductParameters where
    validators NewProductParameters {..} = do
        slugDoesntExist <- V.doesntExist $ UniqueProductSlug nppSlug
        skuDoesntExist <- V.doesntExist $ UniqueBaseSku nppBaseSku
        categoryExists <- V.exists nppCategory
        let duplicateSuffixes =
                mapMaybe (\l -> if length l > 1 then listToMaybe l else Nothing)
                    $ L.group
                    $ L.sort
                    $ map nvdSkuSuffix nppVariantData
            variantErrors = checkDuplicateSuffix duplicateSuffixes nppVariantData
        return $
            [ ( ""
              , [ ("At least one Variant is required.", null nppVariantData) ]
              )
            , ( "name"
              , [ V.required nppName ]
              )
            , ( "slug"
              ,  [ V.required nppSlug
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
              , [ V.required nppBaseSku
                , ( "A Product with this SKU already exists."
                  , skuDoesntExist
                  )
                ]
              )
            ]
            ++ variantErrors
      where
        checkDuplicateSuffix dupes variants =
            let duplicateIndexes =
                    L.findIndices ((`elem` dupes) . nvdSkuSuffix) variants
            in
                map (\i ->
                        ( "variant-" <> T.pack (show i) <> "-skuSuffix"
                        , [ ("SKU suffixes must be unique.", True) ]
                        )
                    )
                    duplicateIndexes

data NewVariantData =
    NewVariantData
        { nvdSkuSuffix :: T.Text
        , nvdPrice :: Cents
        , nvdQuantity :: Int
        , nvdLotSize :: Maybe LotSize
        , nvdIsActive :: Bool
        }

instance FromJSON NewVariantData where
    parseJSON = withObject "NewVariantData" $ \v -> do
        nvdSkuSuffix <- v .: "skuSuffix"
        nvdPrice <- v .: "price"
        nvdQuantity <- v .: "quantity"
        nvdLotSize <- v .: "lotSize"
        nvdIsActive <- v .: "isActive"
        return NewVariantData {..}

data NewSeedData =
    NewSeedData
        { nsdOrganic :: Bool
        , nsdHeirloom :: Bool
        , nsdSmallGrower :: Bool
        , nsdRegional :: Bool
        }

instance FromJSON NewSeedData where
    parseJSON = withObject "NewSeedData" $ \v -> do
        nsdOrganic <- v .: "organic"
        nsdHeirloom <- v .: "heirloom"
        nsdSmallGrower <- v .: "smallGrower"
        nsdRegional <- v .: "regional"
        return NewSeedData {..}

newProductRoute :: WrappedAuthToken -> NewProductParameters -> App (Cookied ProductId)
newProductRoute = validateAdminAndParameters $ \_ p@NewProductParameters {..} -> do
    time <- liftIO getCurrentTime
    imageFileName <- makeImageFromBase64 "products" nppImageName nppImageData
    runDB $ do
        productId <- insert $ makeProduct p imageFileName time
        insertMany_ $ map (makeVariant productId) nppVariantData
        maybe (return ()) (insert_ . makeAttributes productId) nppSeedAttribute
        return productId
  where
    makeProduct :: NewProductParameters -> T.Text -> UTCTime -> Product
    makeProduct NewProductParameters {..} imageUrl time =
        Product
            { productName = sanitize nppName
            , productSlug = slugify nppSlug
            , productCategoryIds = [nppCategory]
            , productBaseSku = nppBaseSku
            , productShortDescription = ""
            , productLongDescription = sanitize nppLongDescription
            , productImageUrl = imageUrl
            , productIsActive = nppIsActive
            , productCreatedAt = time
            }
    makeVariant :: ProductId -> NewVariantData -> ProductVariant
    makeVariant pId NewVariantData {..} =
        ProductVariant
            { productVariantProductId = pId
            , productVariantSkuSuffix = nvdSkuSuffix
            , productVariantPrice = nvdPrice
            , productVariantQuantity = fromIntegral nvdQuantity
            , productVariantLotSize = nvdLotSize
            , productVariantIsActive = nvdIsActive
            }
    makeAttributes :: ProductId -> NewSeedData -> SeedAttribute
    makeAttributes pId NewSeedData {..} =
        SeedAttribute
            { seedAttributeProductId = pId
            , seedAttributeIsOrganic = nsdOrganic
            , seedAttributeIsHeirloom = nsdHeirloom
            , seedAttributeIsSmallGrower = nsdSmallGrower
            , seedAttributeIsRegional = nsdRegional
            }
