{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (FromRecord(..), HasHeader(NoHeader), decode)
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import GHC.Generics (Generic)

import Models

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Esqueleto as E

main :: IO ()
main = do
    importData <- readImportFile
    connectToPostgres >>= runSqlPool (updateCategoryKeywords importData)

readImportFile :: IO [ImportData]
readImportFile =
    either error V.toList . decode NoHeader <$> LBS.readFile "category-keywords.csv"

data ImportData =
    ImportData
        { idCategorySlug :: T.Text
        , idCategoryKeywords :: T.Text
        } deriving (Show, Read, Generic)

instance FromRecord ImportData


connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

updateCategoryKeywords :: [ImportData] -> SqlPersistT IO ()
updateCategoryKeywords =
    mapM_ updateCategory
  where
    updateCategory :: ImportData -> SqlPersistT IO ()
    updateCategory iData = do
        let slug = T.toLower $ idCategorySlug iData
        categoryTree <- getBy (UniqueCategorySlug slug) >>= \case
            Nothing ->
                error $ "Invalid slug: " <> T.unpack slug
            Just (Entity cId _) ->
                getChildCategoryIds cId
        products <- E.select $ E.from $ \p -> do
            let alternateExists = E.exists $ E.from $ \ptc ->
                    E.where_ $ (ptc E.^. ProductToCategoryProductId E.==. p E.^. ProductId) E.&&.
                        (ptc E.^. ProductToCategoryCategoryId `E.in_` E.valList categoryTree)
            E.where_ $
                (p E.^. ProductMainCategory `E.in_` E.valList categoryTree) E.||.
                alternateExists
            return p
        mapM_ (updateProduct $ idCategoryKeywords iData) products
    updateProduct :: T.Text -> Entity Product -> SqlPersistT IO ()
    updateProduct newKeywords (Entity pId prod) =
        let keywords = T.unwords . L.nub . filter (/= "") . T.words
                $ productKeywords prod <> " " <> newKeywords
        in  update pId [ProductKeywords =. keywords]
