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
import Data.Maybe (fromMaybe)
import Database.Persist ((==.), (<-.), (||.), Entity(..), Filter(..), SelectOpt(..), selectList, count, getBy)
import Database.Persist.Sql (fromSqlKey, PersistFilter(..))
import Servant ((:>), (:<|>)(..), Capture, QueryParam, ReqBody, Get, Post, JSON, throwError, err404)

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
    :<|> (T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> App CategoryDetailsData)

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
        , cddTotalProducts :: Int
        } deriving (Show)

instance ToJSON CategoryDetailsData where
    toJSON CategoryDetailsData { cddCategory, cddSubCategories, cddProducts, cddTotalProducts } =
        object [ "category" .= toJSON cddCategory
               , "subCategories" .= toJSON cddSubCategories
               , "products" .= toJSON cddProducts
               , "total" .= toJSON cddTotalProducts
               ]


type CategoryDetailsRoute =
    Capture "slug" T.Text
    :> QueryParam "sortBy" T.Text
    :> QueryParam "page" Int
    :> QueryParam "perPage" Int
    :> Get '[JSON] CategoryDetailsData

categoryDetailsRoute :: T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> App CategoryDetailsData
categoryDetailsRoute slug maybeSort maybePage maybePerPage = do
    maybeCategory <- runDB . getBy $ UniqueCategorySlug slug
    case maybeCategory of
        Nothing ->
            throwError err404
        Just e@(Entity categoryId _) -> do
            subCategories <- runDB $ selectList [CategoryParentId ==. Just categoryId] [Asc CategoryName]
            (products, productsCount) <- paginatedSelect
                [ProductCategoryIds ==. [categoryId]] [parseProductsSorting maybeSort]
                maybePage maybePerPage
            productData <- mapM (getProductData . truncateDescription) products
            return $ CategoryDetailsData e subCategories productData productsCount



-- Products
type ProductAPI =
         "search" :> ProductsSearchRoute
    :<|> "details" :> ProductDetailsRoute

type ProductRoutes =
         (Maybe T.Text -> Maybe Int -> Maybe Int -> ProductsSearchParameters -> App ProductsSearchData)
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

data ProductsSearchData =
    ProductsSearchData
        { psdProductData :: [ProductData]
        , psdTotalProducts :: Int
        } deriving (Show)

instance ToJSON ProductsSearchData where
    toJSON searchData =
        object [ "products" .= toJSON (psdProductData searchData)
               , "total" .= toJSON (psdTotalProducts searchData)
               ]

type ProductsSearchRoute =
       QueryParam "sortBy" T.Text
    :> QueryParam "page" Int
    :> QueryParam "perPage" Int
    :> ReqBody '[JSON] ProductsSearchParameters
    :> Post '[JSON] ProductsSearchData

productsSearchRoute :: Maybe T.Text -> Maybe Int -> Maybe Int -> ProductsSearchParameters -> App ProductsSearchData
productsSearchRoute maybeSort maybePage maybePerPage ProductsSearchParameters { pspQuery } = do
    (products, productsCount) <- paginatedSelect
        ([ProductName `ilike` pspQuery] ||. [ProductLongDescription `ilike` pspQuery])
        [parseProductsSorting maybeSort] maybePage maybePerPage
    searchData <- mapM (getProductData . truncateDescription) products
    return $ ProductsSearchData searchData productsCount
    where ilike :: EntityField a T.Text -> T.Text -> Filter a
          ilike field value =
            Filter field (Left $ T.concat ["%", value, "%"]) (BackendSpecificFilter "ILIKE")


-- Utils
paginatedSelect :: [Filter Product] -> [SelectOpt Product] -> Maybe Int -> Maybe Int -> App ([Entity Product], Int)
paginatedSelect filters options maybePage maybePerPage =
    let
        perPage = fromMaybe 25 maybePerPage
        page = fromMaybe 1 maybePage
        offset = (page - 1) * perPage
    in
        runDB $ do
            products <- selectList filters ([LimitTo perPage, OffsetBy offset] ++ options)
            productsCount <- count filters
            return (products, productsCount)

-- TODO: Implement price sorting by ordering queries w/ esqueleto
parseProductsSorting :: Maybe T.Text -> SelectOpt Product
parseProductsSorting queryString =
        case fromMaybe "" queryString of
            "name-asc" ->
                Asc ProductName
            "name-desc" ->
                Desc ProductName
            "price-asc" ->
                error "Not Implemented"
            "price-desc" ->
                error "Not Implemented"
            "number-asc" ->
                Asc ProductBaseSku
            _ ->
                Asc ProductName
