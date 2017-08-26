{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models.DB where

import Data.Int (Int64)
import Database.Persist.TH

import Models.Fields

import qualified Data.Text as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category json
    name T.Text
    slug T.Text
    parentId CategoryId Maybe
    description T.Text
    imageUrl T.Text
    order Int
    UniqueCategorySlug slug
    deriving Show

Product json
    name T.Text
    slug T.Text
    categoryIds [CategoryId]
    baseSku T.Text
    shortDescription T.Text
    longDescription T.Text
    imageUrl T.Text
    UniqueProductSlug slug
    UniqueBaseSku baseSku
    deriving Show

ProductVariant json
    productId ProductId
    skuSuffix T.Text
    price Cents
    quantity Int64
    weight Milligrams
    isActive Bool
    UniqueSku productId skuSuffix
    deriving Show

SeedAttribute json
    productId ProductId
    isOrganic Bool
    isHeirloom Bool
    isEcological Bool
    isRegional Bool
    UniqueAttribute productId
    deriving Show
|]
