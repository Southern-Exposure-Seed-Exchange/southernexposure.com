{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (mzero)
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv ((.!), FromRecord(..), HasHeader(NoHeader), decode)
import Data.Monoid ((<>))
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import GHC.Generics (Generic)

import Models

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
    importData <- filter (\d -> idSuggestionSku d /= "") <$> readImportFile
    connectToPostgres >>= runSqlPool (updateProductDescription importData)


readImportFile :: IO [ImportData]
readImportFile =
    either error V.toList . decode NoHeader
        <$> LBS.readFile "sold-out-export.csv"

data ImportData =
    ImportData
        { idProductSku :: T.Text
        , idSuggestionSku :: T.Text
        } deriving (Show, Read, Generic)

instance FromRecord ImportData where
    parseRecord v
        | length v == 3 = ImportData <$> v .! 0 <*> v .! 2
        | otherwise = mzero


connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1


updateProductDescription :: [ImportData] -> SqlPersistT IO ()
updateProductDescription =
    mapM_ updateProduct
  where
    updateProduct :: ImportData -> SqlPersistT IO ()
    updateProduct iData = do
        mProd <- getBy $ UniqueBaseSku $ idProductSku iData
        mSugg <- getBy $ UniqueBaseSku $ idSuggestionSku iData
        case (,) <$> mProd <*> mSugg of
            Nothing ->
                error $ "Could not find products: " <> show iData
            Just (Entity baseId baseProduct, Entity _ suggestion) ->
                let suggestionLink =
                        "/products/" <> productSlug suggestion
                    suggestionText = T.concat
                        [ "<span class='text-danger'>"
                        , "Consider "
                        , "<a href='" <> suggestionLink <> "' class='text-danger'>"
                        , productName suggestion
                        , "</a>"
                        , " as a substitute. "
                        , "</span>"
                        ]

                in
                update baseId
                    [ ProductLongDescription =.
                        suggestionText <> productLongDescription baseProduct
                    ]
