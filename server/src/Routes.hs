{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes
    ( ProductDetailsRoute
    , productDetailsRoute
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Database.Persist ((==.), (<-.), Entity(..), selectList, getBy)
import Servant ((:>), Capture, Get, JSON, throwError, err404)

import Models
import Server

import qualified Data.Text as T


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
