{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes
    ( CategoryAPI
    , categoryRoutes
    , ProductAPI
    , productRoutes
    ) where

import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Int (Int64)
import Database.Persist ((==.), (<-.), (||.), Entity(..), Filter(..), SelectOpt(..), selectList, getBy)
import Database.Persist.Sql (fromSqlKey, PersistFilter(..))
import Servant ((:>), (:<|>)(..), Capture, ReqBody, Get, Post, JSON, throwError, err404)

import Models
import Server

import qualified Data.Map.Strict as Map
import qualified Data.Text as T


-- Common
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
type CategoryAPI =
         "nav" :> CategoryNavbarRoute
    :<|> "details" :> CategoryDetailsRoute

type CategoryRoutes =
         App CategoryNavbarData
    :<|> (T.Text -> App CategoryDetailsData)

categoryRoutes :: CategoryRoutes
categoryRoutes =
         categoryNavbarRoute
    :<|> categoryDetailsRoute


data CategoryNavbarData =
    CategoryNavbarData
        { cndRoots :: [Entity Category]
        , cndChildren :: Map.Map Int64 [Entity Category]
        } deriving (Show)


instance ToJSON CategoryNavbarData where
    toJSON CategoryNavbarData { cndRoots = roots, cndChildren = children } =
        object [ "rootCategories" .= toJSON roots
               , "childrenCategories" .= toJSON children
               ]

type CategoryNavbarRoute =
    Get '[JSON] CategoryNavbarData

categoryNavbarRoute :: App CategoryNavbarData
categoryNavbarRoute = do
    roots <- runDB $ selectList [CategoryParentId ==. Nothing] [Asc CategoryOrder]
    let rootIds = map (\(Entity i _) -> Just i) roots
    children <- runDB $ selectList [CategoryParentId <-. rootIds] [Desc CategoryOrder]
    let childrenMap = foldl mergeChild Map.empty children
    return $ CategoryNavbarData roots childrenMap
    where mergeChild childMap (Entity childId child) =
            case categoryParentId child of
                Nothing ->
                    childMap
                Just parentId ->
                    Map.insertWith (++) (fromSqlKey parentId)
                        [Entity childId (child { categoryDescription = "" })]
                        childMap


data CategoryDetailsData =
    CategoryDetailsData
        { cddCategory :: Entity Category
        , cddSubCategories :: [Entity Category]
        , cddProducts :: [ProductData]
        } deriving (Show)

instance ToJSON CategoryDetailsData where
    toJSON CategoryDetailsData { cddCategory, cddSubCategories, cddProducts } =
        object [ "category" .= toJSON cddCategory
               , "subCategories" .= toJSON cddSubCategories
               , "products" .= toJSON cddProducts
               ]


type CategoryDetailsRoute =
    Capture "slug" T.Text :> Get '[JSON] CategoryDetailsData

categoryDetailsRoute :: T.Text -> App CategoryDetailsData
categoryDetailsRoute slug = do
    maybeCategory <- runDB . getBy $ UniqueCategorySlug slug
    case maybeCategory of
        Nothing ->
            throwError err404
        Just e@(Entity categoryId _) -> do
            subCategories <- runDB $ selectList [CategoryParentId ==. Just categoryId] [Asc CategoryName]
            products <- runDB $ selectList [ProductCategoryIds ==. [categoryId]] [Asc ProductName]
            productData <- mapM (getProductData . truncateDescription) products
            return $ CategoryDetailsData e subCategories productData



-- Products
type ProductAPI =
         "search" :> ProductsSearchRoute
    :<|> "details" :> ProductDetailsRoute

type ProductRoutes =
         (ProductsSearchParameters -> App ProductsSearchData)
    :<|> (T.Text -> App ProductDetailsData)

productRoutes :: ProductRoutes
productRoutes =
         productsSearchRoute
    :<|> productDetailsRoute


data ProductDetailsData =
    ProductDetailsData
        { pddProduct :: Entity Product
        , pddVariants :: [Entity ProductVariant]
        , pddSeedAttribute :: Maybe (Entity SeedAttribute)
        , pddCategories :: [Entity Category]
        } deriving (Show)

instance ToJSON ProductDetailsData where
    toJSON productData =
        object [ "product" .= toJSON (pddProduct productData)
               , "variants" .= map toJSON (pddVariants productData)
               , "seedAttribute" .= toJSON (pddSeedAttribute productData)
               , "categories" .= map toJSON (pddCategories productData)
               ]

type ProductDetailsRoute =
    Capture "slug" T.Text :> Get '[JSON] ProductDetailsData

-- TODO: Use Esqueleto library to make 1 query instead of 4
productDetailsRoute :: T.Text -> App ProductDetailsData
productDetailsRoute slug = do
        maybeProduct <- runDB . getBy $ UniqueProductSlug slug
        case maybeProduct of
            Nothing ->
                throwError err404
            Just e@(Entity productId prod) -> do
                (variants, maybeAttribute, categories) <- runDB $
                    (,,) <$> selectList [ProductVariantProductId ==. productId] []
                        <*> getBy (UniqueAttribute productId)
                        <*> selectList [CategoryId <-. productCategoryIds prod] []
                return $ ProductDetailsData e variants maybeAttribute categories


newtype ProductsSearchParameters =
    ProductsSearchParameters
        { pspQuery :: T.Text
        } deriving (Show)

instance FromJSON ProductsSearchParameters where
    parseJSON =
        withObject "ProductsSearchParameters" $ \v ->
            ProductsSearchParameters <$>
                v .: "query"

newtype ProductsSearchData =
    ProductsSearchData
        { psdProductData :: [ProductData]
        } deriving (Show)

instance ToJSON ProductsSearchData where
    toJSON searchData =
        object [ "products" .= toJSON (psdProductData searchData)
               ]

type ProductsSearchRoute =
    ReqBody '[JSON] ProductsSearchParameters :> Post '[JSON] ProductsSearchData

productsSearchRoute :: ProductsSearchParameters -> App ProductsSearchData
productsSearchRoute ProductsSearchParameters { pspQuery } = do
    products <- runDB $ selectList
        ([ProductName `like` pspQuery] ||. [ProductLongDescription `like` pspQuery])
        []
    searchData <- mapM (getProductData . truncateDescription) products
    return $ ProductsSearchData searchData
    where like :: EntityField a T.Text -> T.Text -> Filter a
          like field value =
            Filter field (Left $ T.concat ["%", value, "%"]) (BackendSpecificFilter "like")
