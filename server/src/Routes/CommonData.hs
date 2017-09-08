{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Routes.CommonData
    ( ProductData
    , getProductData
    , PredecessorCategory
    , categoryToPredecessor
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Database.Persist ((==.), Entity(..), selectList, getBy)

import Models
import Server

import qualified Data.Text as T


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
