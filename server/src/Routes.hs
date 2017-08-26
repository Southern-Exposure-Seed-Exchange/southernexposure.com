{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes
    ( ProductDetailsRoute
    , productDetailsRoute
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Database.Persist ((==.), Entity(..), selectList, getBy)
import Servant ((:>), Capture, Get, JSON, throwError, err404)

import Models
import Server

import qualified Data.Text as T


data ProductDetailsData =
    ProductDetailsData
        { pddProduct :: Entity Product
        , pddVariants :: [Entity ProductVariant]
        , pddSeedAttribute :: Maybe (Entity SeedAttribute)
        } deriving (Show)

instance ToJSON ProductDetailsData where
    toJSON ProductDetailsData { pddProduct = prod, pddVariants = variants, pddSeedAttribute = maybeAttr } =
        object [ "product" .= toJSON prod
               , "variants" .= map toJSON variants
               , "seedAttribute" .= toJSON maybeAttr
               ]

type ProductDetailsRoute =
    Capture "slug" T.Text :> Get '[JSON] ProductDetailsData

productDetailsRoute :: T.Text -> App ProductDetailsData
productDetailsRoute slug = do
        maybeProduct <- runDB . getBy $ UniqueProductSlug slug
        case maybeProduct of
            Nothing ->
                throwError err404
            Just prod@(Entity productId _) -> do
                (variants, maybeAttribute) <- runDB $
                    (,) <$> selectList [ProductVariantProductId ==. productId] []
                        <*> getBy (UniqueAttribute productId)
                return $ ProductDetailsData prod variants maybeAttribute
