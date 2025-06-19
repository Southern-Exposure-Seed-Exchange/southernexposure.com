{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- Update ProductVariant Prices by SKU from a CSV. -}
import Control.Monad.Logger (runNoLoggingT)
import Data.Csv (FromNamedRecord, decodeByName)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import GHC.Generics (Generic)

import Models
import Models.Fields (Cents, fromDollars)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
    productRows <- fmap readPrices . either error snd . decodeByName
        <$> LBS.readFile "new_product_prices.csv"
    connectToPostgres >>= runSqlPool (updateProducts productRows)

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

data ProductData =
    ProductData
        { productSku :: T.Text
        , price :: T.Text
        } deriving (Show, Generic)

instance FromNamedRecord ProductData

readPrices :: ProductData -> (T.Text, T.Text, Cents)
readPrices prod =
    let fullSku =
            productSku prod
        (prefix, suffix) =
            if Char.isAlpha $ T.head $ T.takeEnd 1 fullSku then
                (T.dropEnd 1 fullSku, T.takeEnd 1 fullSku)
            else
                (fullSku, "")
    in
    ( prefix
    , suffix
    , fromDollars $ read $ T.unpack $ price prod
    )

updateProducts :: V.Vector (T.Text, T.Text, Cents) -> SqlPersistT IO ()
updateProducts =
    V.mapM_ $ \(baseSku, skuSuffix, price_) ->
        getBy (UniqueBaseSku baseSku) >>= \case
            Nothing ->
                error $ "No product with base SKU: " ++ T.unpack baseSku
            Just (Entity pId _) ->
                getBy (UniqueSku pId skuSuffix) >>= \case
                    Nothing ->
                        error $ "No product with full SKU: "
                            ++ T.unpack baseSku ++ T.unpack skuSuffix
                    Just (Entity vId _) ->
                        update vId [ProductVariantPrice =. price_]
