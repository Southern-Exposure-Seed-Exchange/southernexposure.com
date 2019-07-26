{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Categories
    ( CategoryAPI
    , categoryRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Int (Int64)
import Database.Persist ((==.), (<-.), Entity(..), SelectOpt(..), selectList, getBy)
import Database.Persist.Sql (fromSqlKey)
import Servant ((:>), (:<|>)(..), Capture, QueryParam, Get, JSON, throwError, err404)

import Models
import Routes.CommonData
import Routes.Utils (paginatedSelect)
import Server

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Esqueleto as E


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


-- NAVBAR


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


-- DETAILS


data CategoryDetailsData =
    CategoryDetailsData
        { cddCategory :: Entity Category
        , cddSubCategories :: [Entity Category]
        , cddProducts :: [ProductData]
        , cddTotalProducts :: Int
        , cddPredecessors :: [PredecessorCategory]
        } deriving (Show)

instance ToJSON CategoryDetailsData where
    toJSON CategoryDetailsData { cddCategory, cddSubCategories, cddProducts, cddTotalProducts, cddPredecessors } =
        object [ "category" .= toJSON cddCategory
               , "subCategories" .= toJSON cddSubCategories
               , "products" .= toJSON cddProducts
               , "total" .= toJSON cddTotalProducts
               , "predecessors" .= toJSON cddPredecessors
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
        Just e@(Entity categoryId _) -> runDB $ do
            subCategories <- selectList [CategoryParentId ==. Just categoryId] [Asc CategoryName]
            descendants <- getChildCategoryIds categoryId
            (products, productsCount) <- paginatedSelect
                maybeSort maybePage maybePerPage
                    (\p _ -> (p E.^. ProductCategoryIds) `E.in_` E.valList (map (: []) descendants))
            productData <- mapM (getProductData . truncateDescription) products
            predecessors <- getParentCategories categoryId
            return . CategoryDetailsData e subCategories productData productsCount
                   $ map categoryToPredecessor predecessors


-- ADVANCED SEARCH


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
    return . AdvancedSearchData
        $ map (\(i, n) -> ASDCategory (fromSqlKey $ E.unValue i) (E.unValue n)) cs
