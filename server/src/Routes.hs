{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes
    ( CategoryAPI
    , categoryRoutes
    , ProductDetailsRoute
    , productDetailsRoute
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Database.Persist ((==.), (<-.), Entity(..), SelectOpt(..), selectList, getBy)
import Database.Persist.Sql (fromSqlKey)
import Servant ((:>), (:<|>)(..), Capture, Get, JSON, throwError, err404)
import Text.HTML.TagSoup (parseTags, innerText)

import Models
import Server

import qualified Data.Map.Strict as Map
import qualified Data.Text as T


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
        , cddProducts :: [CategoryDetailsProductData]
        } deriving (Show)
data CategoryDetailsProductData =
    CategoryDetailsProductData
        { cdpdProduct :: Entity Product
        , cdpdVariants :: [Entity ProductVariant]
        , cdpdSeedAttribute :: Maybe (Entity SeedAttribute)
        } deriving (Show)

instance ToJSON CategoryDetailsData where
    toJSON CategoryDetailsData { cddCategory, cddSubCategories, cddProducts } =
        object [ "category" .= toJSON cddCategory
               , "subCategories" .= toJSON cddSubCategories
               , "products" .= toJSON cddProducts
               ]

instance ToJSON CategoryDetailsProductData where
    toJSON CategoryDetailsProductData { cdpdProduct, cdpdVariants, cdpdSeedAttribute } =
        object [ "product" .= toJSON cdpdProduct
               , "variants" .= toJSON cdpdVariants
               , "seedAttribute" .= toJSON cdpdSeedAttribute
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
    where truncateDescription (Entity pId p) =
            let
                strippedDescription =
                    innerText . parseTags $ productLongDescription p
                truncatedDescription =
                    T.unwords . take 40 $ T.words strippedDescription
                newDescription =
                    if truncatedDescription /= strippedDescription then
                        truncatedDescription <> "..."
                    else
                        truncatedDescription
            in
                Entity pId $ p { productLongDescription = newDescription }
          getProductData e@(Entity productId _) = do
                variants <- runDB $ selectList [ProductVariantProductId ==. productId] []
                maybeAttribute <- runDB . getBy $ UniqueAttribute productId
                return $ CategoryDetailsProductData e variants maybeAttribute



-- Products

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
