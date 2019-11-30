{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-| Export Product SKUs, Names, & Categories. -}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (ToNamedRecord, DefaultOrdered(..), encodeDefaultOrderedByName, header)
import Data.List (sortOn)
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import GHC.Generics (Generic)

import Cache (syncCategoryPredecessorCache, queryCategoryPredecessorCache)
import Models
import Models.Fields

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Database.Esqueleto as E

main :: IO ()
main = do
    productData <- connectToPostgres >>= runSqlPool getProductData
    exportData $ sortOn fullSku $ map makeExportData productData

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

getProductData :: SqlPersistT IO [(Entity Product, Entity ProductVariant, [Entity Category])]
getProductData = do
    categoryCache <- syncCategoryPredecessorCache
    products <- E.select $ E.from $ \(p `E.InnerJoin` v `E.InnerJoin` c) -> do
        E.on $ c E.^. CategoryId E.==. p E.^. ProductMainCategory
        E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
        E.where_ $ v E.^. ProductVariantIsActive
        return (p, v, c)
    return $ map (addCategoryParents categoryCache) products
  where
    addCategoryParents cache (p, v, c@(Entity categoryId _)) =
        (p, v, reverse $ c : queryCategoryPredecessorCache categoryId cache)


data ExportData =
    ExportData
        { fullSku :: T.Text
        , name :: T.Text
        , categories :: T.Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData where
    headerOrder _ = header ["fullSku", "name", "categories"]

makeExportData :: (Entity Product, Entity ProductVariant, [Entity Category]) -> ExportData
makeExportData (Entity _ prod, Entity _ variant, categories_) =
    ExportData
        { fullSku = productBaseSku prod <> productVariantSkuSuffix variant
        , name = productName prod <> maybe "" ((", " <>) . renderLotSize) (productVariantLotSize variant)
        , categories = T.intercalate " > " $ map (categoryName . entityVal) categories_
        }

exportData :: [ExportData] -> IO ()
exportData rows =
    LBS.writeFile "products-export.csv" $ encodeDefaultOrderedByName rows
