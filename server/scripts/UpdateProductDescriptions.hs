{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- Export Product SKUs, Names, & Categories. -}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (FromNamedRecord, decodeByName)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool, toSqlKey
    )
import GHC.Generics (Generic)

import Models

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
    productRows <- either error snd . decodeByName
        <$> LBS.readFile "new_product_descriptions.csv"
    connectToPostgres >>= runSqlPool (updateProducts productRows)

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

data ProductData =
    ProductData
        { productId :: T.Text
        , description :: T.Text
        } deriving (Show, Generic)

instance FromNamedRecord ProductData

updateProducts :: V.Vector ProductData -> SqlPersistT IO ()
updateProducts =
    V.mapM_ $ \prod ->
        update (toSqlKey . read . T.unpack $ productId prod)
            [ProductLongDescription =. description prod]
