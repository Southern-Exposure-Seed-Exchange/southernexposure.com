{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{- Export Product SKUs, Names, & Categories. -}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (ToNamedRecord, DefaultOrdered(..), encodeDefaultOrderedByName)
import Data.List (sortOn)
import Data.Monoid ((<>))
import Data.Time (formatTime, defaultTimeLocale)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import GHC.Generics (Generic)

import Models
import Models.Fields

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Database.Esqueleto as E

main :: IO ()
main = do
    productData <- connectToPostgres >>= runSqlPool getProductData
    exportData $ sortOn sku $ map makeExportData productData

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

getProductData :: SqlPersistT IO [(Entity ProductVariant, Entity Product, Maybe (Entity SeedAttribute), E.Value T.Text)]
getProductData = E.select $ E.from $ \(v `E.InnerJoin` p `E.InnerJoin` c `E.LeftOuterJoin` sa) -> do
    E.on $ sa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId)
    E.on $ c E.^. CategoryId E.==. p E.^. ProductMainCategory
    E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
    E.where_ $ v E.^. ProductVariantIsActive E.==. E.val True
    return (v, p, sa, c E.^. CategoryName)


data ExportData =
    ExportData
        { sku :: T.Text
        , name :: T.Text
        , price :: T.Text
        , lotSize :: T.Text
        , slug :: T.Text
        , description :: T.Text
        , imageUrl :: T.Text
        , isOrganic :: T.Text
        , isHeirloom :: T.Text
        , isSmallGrower :: T.Text
        , isRegional :: T.Text
        , categories :: T.Text
        , searchKeywords :: T.Text
        , shippingRestrictions :: T.Text
        , createdAt :: T.Text
        , updatedAt :: T.Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

makeExportData :: (Entity ProductVariant, Entity Product, Maybe (Entity SeedAttribute), E.Value T.Text) -> ExportData
makeExportData (Entity _ ProductVariant {..}, Entity _ Product {..}, maybeAttribute, E.Value categoryName_) =
    ExportData
        { sku = productBaseSku <> productVariantSkuSuffix
        , name = productName
        , slug = productSlug
        , price = formatCents productVariantPrice
        , lotSize = maybe "" renderLotSize productVariantLotSize
        , description = productLongDescription
        , imageUrl = "https://southernexposure.com/media/products/originals/" <> productImageUrl
        , isOrganic = getStatus seedAttributeIsOrganic maybeAttribute
        , isHeirloom = getStatus seedAttributeIsHeirloom maybeAttribute
        , isSmallGrower = getStatus seedAttributeIsSmallGrower maybeAttribute
        , isRegional = getStatus seedAttributeIsRegional maybeAttribute
        , categories = categoryName_
        , searchKeywords = productKeywords
        , shippingRestrictions = T.intercalate ", " $ map regionName productShippingRestrictions
        , createdAt = T.pack $ formatTime defaultTimeLocale "%F" productCreatedAt
        , updatedAt = T.pack $ formatTime defaultTimeLocale "%F" productUpdatedAt
        }
  where
    getStatus field =
        maybe "No" (\sa -> if field $ entityVal sa then "Yes" else "No")

exportData :: [ExportData] -> IO ()
exportData rows =
    LBS.writeFile "products-export-racks.csv" $ encodeDefaultOrderedByName rows
