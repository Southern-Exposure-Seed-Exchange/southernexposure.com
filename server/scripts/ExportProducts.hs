{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- Export Product SKUs, Names, & Categories. -}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (ToNamedRecord, DefaultOrdered(..), encodeDefaultOrderedByName, header)
import Data.List (sortOn)
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
import qualified Database.Esqueleto.Experimental as E

main :: IO ()
main = do
    productData <- connectToPostgres >>= runSqlPool getProductData
    exportData $ sortOn fullSku $ map makeExportData productData

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

getProductData :: SqlPersistT IO [(Entity Product, Entity ProductVariant, Maybe (Entity SeedAttribute), [Entity Category])]
getProductData = do
    categoryCache <- syncCategoryPredecessorCache
    products <- E.select $ do 
        (p E.:& v E.:& c E.:& sa) <- E.from $ E.table 
            `E.innerJoin` E.table 
                `E.on` (\(p E.:& v) -> v E.^. ProductVariantProductId E.==. p E.^. ProductId)
            `E.innerJoin` E.table 
                `E.on` (\(p E.:& _ E.:& c) -> c E.^. CategoryId E.==. p E.^. ProductMainCategory)
            `E.leftJoin` E.table 
                `E.on` (\(p E.:& _ E.:& _ E.:& sa) -> sa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId))
        E.where_ $ v E.^. ProductVariantIsActive
        return (p, v, sa, c)
    return $ map (addCategoryParents categoryCache) products
  where
    addCategoryParents cache (p, v, sa, c@(Entity categoryId _)) =
        (p, v, sa, reverse $ c : queryCategoryPredecessorCache categoryId cache)


data ExportData =
    ExportData
        { fullSku :: T.Text
        , name :: T.Text
        , isOrganic :: T.Text
        , categories :: T.Text
        } deriving (Show, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData where
    headerOrder _ = header ["fullSku", "name", "isOrganic", "categories"]

makeExportData :: (Entity Product, Entity ProductVariant, Maybe (Entity SeedAttribute), [Entity Category]) -> ExportData
makeExportData (Entity _ prod, Entity _ variant, maybeAttribute, categories_) =
    ExportData
        { fullSku = productBaseSku prod <> productVariantSkuSuffix variant
        , name = productName prod <> maybe "" ((", " <>) . renderLotSize) (productVariantLotSize variant)
        , isOrganic = getOrganicStatus maybeAttribute
        , categories = T.intercalate " > " $ map (categoryName . entityVal) categories_
        }
  where
    getOrganicStatus =
        maybe "No" (\sa -> if seedAttributeIsOrganic $ entityVal sa then "Yes" else "No")

exportData :: [ExportData] -> IO ()
exportData rows =
    LBS.writeFile "products-export.csv" $ encodeDefaultOrderedByName rows
