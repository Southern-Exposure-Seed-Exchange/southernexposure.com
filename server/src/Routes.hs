{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Database.Persist ((==.), (<-.), Entity(..), SelectOpt(..), selectList, getBy)
import Database.Persist.Sql (fromSqlKey)
import Servant ((:>), (:<|>)(..), Capture, QueryParam, ReqBody, Get, Post, JSON, throwError, err404)

import Models
import Models.Fields (Cents)
import Server

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Esqueleto as E


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
    :<|> "search" :> AdvancedSearchRoute

type CategoryRoutes =
         App CategoryNavbarData
    :<|> (T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> App CategoryDetailsData)
    :<|> App AdvancedSearchData

categoryRoutes :: CategoryRoutes
categoryRoutes =
         categoryNavbarRoute
    :<|> categoryDetailsRoute
    :<|> advancedSearchRoute


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
                maybeSort maybePage maybePerPage
                    (\p -> (p E.^. ProductCategoryIds) E.==. E.val [categoryId])
            productData <- mapM (getProductData . truncateDescription) products
            return $ CategoryDetailsData e subCategories productData productsCount


newtype AdvancedSearchData =
    AdvancedSearchData
        { asdCategories :: [ASDCategory]
        } deriving (Show)

instance ToJSON AdvancedSearchData where
    toJSON AdvancedSearchData { asdCategories } =
        object [ "categories" .= toJSON asdCategories ]

data ASDCategory =
    ASDCategory
        { asdcId :: Int64
        , asdcName :: T.Text
        } deriving (Show)

instance ToJSON ASDCategory where
    toJSON ASDCategory { asdcId, asdcName } =
        object [ "id" .= asdcId, "name" .= asdcName ]

type AdvancedSearchRoute =
    Get '[JSON] AdvancedSearchData

advancedSearchRoute :: App AdvancedSearchData
advancedSearchRoute = do
    cs <- runDB $ E.select $ E.from $ \c -> do
        E.orderBy [E.asc $ c E.^. CategoryName]
        return (c E.^. CategoryId, c E.^. CategoryName)
    return . AdvancedSearchData $ map (\(i, n) -> ASDCategory (fromSqlKey $ E.unValue i) (E.unValue n)) cs



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
        maybeSort maybePage maybePerPage
        (\p -> ((p E.^. ProductName) `E.ilike` ((E.%) E.++. E.val pspQuery E.++. (E.%))) E.||.
               ((p E.^. ProductLongDescription) `E.ilike` ((E.%) E.++. E.val pspQuery E.++. (E.%)))
        )
    searchData <- mapM (getProductData . truncateDescription) products
    return $ ProductsSearchData searchData productsCount


-- Utils
--

variantSorted :: (E.SqlExpr (E.Value (Maybe Cents)) -> [E.SqlExpr E.OrderBy])
              -> Int64 -> Int64
              -> (E.SqlExpr (Entity Product) -> E.SqlExpr (E.Value Bool))
              -> App ([Entity Product], Int)
variantSorted ordering offset perPage filters = runDB $ do
    productsAndPrice <- E.select $ E.from $ \(p `E.InnerJoin` v) -> do
        E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
        let minPrice = E.min_ $ v E.^. ProductVariantPrice
        E.groupBy $ p E.^. ProductId
        E.orderBy $ ordering minPrice
        E.where_ $ filters p
        E.limit perPage
        E.offset offset
        return (p, minPrice)
    pCount <- E.select $ E.from $ \p -> do
        E.where_ $ filters p
        return (E.countRows :: E.SqlExpr (E.Value Int))
    let (ps, _) = unzip productsAndPrice
    return (ps, E.unValue $ head pCount)

paginatedSelect :: Maybe T.Text -> Maybe Int -> Maybe Int -> (E.SqlExpr (Entity Product) -> E.SqlExpr (E.Value Bool)) -> App ([Entity Product], Int)
paginatedSelect maybeSorting maybePage maybePerPage productFilters =
    let sorting = fromMaybe "" maybeSorting in
    case sorting of
        "name-asc" ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductName])
        "name-desc" ->
            productsSelect (\p -> E.orderBy [E.desc $ p E.^. ProductName])
        "number-asc" ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductBaseSku])
        "price-asc" ->
            variantSorted (\f -> [E.asc f]) offset perPage productFilters
        "price-desc" ->
            variantSorted (\f -> [E.desc f]) offset perPage productFilters
        _ ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductName])
    where perPage =
            fromIntegral $ fromMaybe 25 maybePerPage
          page =
            fromIntegral $ fromMaybe 1 maybePage
          offset =
            (page - 1) * perPage
          productsSelect ordering =
            runDB $ do
                products <- E.select $ E.from $ \p -> do
                    E.where_ $ productFilters p
                    _ <- ordering p
                    E.limit perPage
                    E.offset offset
                    return p
                productsCount <- E.select $ E.from $ \p -> do
                    E.where_ $ productFilters p
                    return (E.countRows :: E.SqlExpr (E.Value Int))
                return (products, E.unValue $ head productsCount)
