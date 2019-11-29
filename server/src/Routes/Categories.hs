{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Categories
    ( CategoryAPI
    , categoryRoutes
    ) where

import Control.Monad.Trans (lift)
import Data.Aeson ((.=), ToJSON(..), object)
import Data.Foldable (foldlM)
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
        { cndRoots :: [CategoryData]
        , cndChildren :: Map.Map Int64 [CategoryData]
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
    childrenMap <- foldlM mergeChild Map.empty children
    rootData <- mapM makeCategoryData roots
    return $ CategoryNavbarData rootData childrenMap
  where
    mergeChild :: Map.Map Int64 [CategoryData] -> Entity Category -> App (Map.Map Int64 [CategoryData])
    mergeChild childMap e@(Entity _ child) =
        case categoryParentId child of
            Nothing ->
                return childMap
            Just parentId -> do
                catData <- makeCategoryData e
                return $ Map.insertWith (++) (fromSqlKey parentId)
                    [(catData { cdDescription = "" })]
                    childMap


-- DETAILS


data CategoryDetailsData =
    CategoryDetailsData
        { cddCategory :: CategoryData
        , cddSubCategories :: [CategoryData]
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
            categoryData <- lift $ makeCategoryData e
            subCategories <- selectList [CategoryParentId ==. Just categoryId] [Asc CategoryName]
                >>= lift . mapM makeCategoryData
            descendants <- getChildCategoryIds categoryId
            (products, productsCount) <- paginatedSelect
                maybeSort maybePage maybePerPage
                    (\p _ pToC ->
                        p E.^. ProductMainCategory `E.in_` E.valList descendants E.||.
                        (E.just (p E.^. ProductId) E.==. pToC E.?. ProductToCategoryProductId E.&&.
                         pToC E.?. ProductToCategoryCategoryId `E.in_` E.justList (E.valList descendants)
                        )
                    )
            productData <- mapM (getProductData . truncateDescription) products
            predecessors <- lift $ getParentCategories categoryId
            return . CategoryDetailsData categoryData subCategories productData productsCount
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
