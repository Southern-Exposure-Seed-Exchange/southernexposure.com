{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (FromRecord(..), HasHeader(NoHeader), decode)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool, toSqlKey
    )
import GHC.Generics (Generic)

import Models

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
    importData <- filter (\d -> idProductKeywords d /= "") <$> readImportFile
    connectToPostgres >>= runSqlPool (updateProductKeywords importData)


readImportFile :: IO [ImportData]
readImportFile =
    either error V.toList . decode NoHeader
        <$> LBS.readFile "product-keywords.csv"

data ImportData =
    ImportData
        { idProductId :: Int64
        , _idProductName :: T.Text
        , _idProductSku :: T.Text
        , idProductKeywords :: T.Text
        } deriving (Show, Read, Generic)

instance FromRecord ImportData


connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

updateProductKeywords :: [ImportData] -> SqlPersistT IO ()
updateProductKeywords =
    mapM_ updateProduct
  where
    updateProduct :: ImportData -> SqlPersistT IO ()
    updateProduct iData  =
        let pId = toSqlKey $ idProductId iData
            newKeywords = T.replace "," " " $ idProductKeywords iData
        in
            get pId >>= \case
                Nothing ->
                    error $ "Could not find product: " <> show pId
                Just prod ->
                    let keywords = T.unwords . L.nub . filter (/= "") . T.words
                            $ productKeywords prod <> " " <> newKeywords
                    in  update pId [ProductKeywords =. keywords]
