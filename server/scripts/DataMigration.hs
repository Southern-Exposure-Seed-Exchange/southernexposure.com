{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad (forM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Char (isAlpha)
import Data.Int (Int32)
import Data.List (nubBy)
import Data.Pool (destroyAllResources)
import Data.Scientific (Scientific)
import Database.MySQL.Base
    ( MySQLConn, ConnectInfo(..), connect, defaultConnectInfo, query_, close, MySQLValue(..)
    , prepareStmt, queryStmt
    )
import Database.Persist (Entity(..), insertMany_, getBy, insert)
import Database.Persist.Postgresql
    (ConnectionPool, SqlWriteT, createPostgresqlPool, toSqlKey, runSqlPool)
import System.Environment (lookupEnv)

import Models
import Models.Fields

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified System.IO.Streams as Streams


main :: IO ()
main = do
    mysqlConn <- connectToMysql
    psqlConn <- connectToPostgres
    mysqlProducts <- makeProducts mysqlConn
    let products = nubBy (\p1 p2 -> productBaseSku p1 == productBaseSku p2)
            $ map (\(_, _, _, _, _, p) -> p) mysqlProducts
        variants = makeVariants mysqlProducts
    attributes <- makeSeedAttributes mysqlConn
    flip runSqlPool psqlConn
        $ insertMany_ products
        >> insertVariants variants
        >> insertAttributes attributes
    close mysqlConn
    destroyAllResources psqlConn


connectToMysql :: IO MySQLConn
connectToMysql = do
    mysqlUser <- maybe "" BS.pack <$> lookupEnv "DB_USER"
    mysqlPass <- maybe "" BS.pack <$> lookupEnv "DB_PASS"
    connect $ defaultConnectInfo
            { ciUser = mysqlUser
            , ciDatabase = "retailzen"
            , ciPassword = mysqlPass
            }

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runStderrLoggingT $ createPostgresqlPool "dbname=sese-website" 1


makeProducts :: MySQLConn -> IO [(Int32, T.Text, Scientific, Float, Float, Product)]
makeProducts mysql = do
    (_, productStream) <- query_ mysql
        "SELECT products_id, products_price, products_quantity, products_weight, products_model, products_image, products_status FROM products WHERE products_status=1"
    ps <- Streams.toList productStream
    forM ps $
        \[MySQLInt32 prodId, MySQLDecimal prodPrice, MySQLFloat prodQty, MySQLFloat prodWeight, MySQLText prodSKU, MySQLText prodImg, _] -> do
            queryString <- prepareStmt mysql
                "SELECT products_id, products_name, products_description FROM products_description WHERE products_id=?"
            (_, descriptionStream) <- queryStmt mysql queryString [MySQLInt32 prodId]
            [_, MySQLText name, MySQLText description] <- head <$> Streams.toList descriptionStream
            _ <- return prodId
            _ <- return prodQty
            let (baseSku, skuSuffix) = splitSku prodSKU
            return (prodId, skuSuffix, prodPrice, prodQty, prodWeight, Product name baseSku "" description prodImg)


makeVariants :: [(Int32, T.Text, Scientific, Float, Float, Product)] -> [(T.Text, ProductVariant)]
makeVariants =
    map makeVariant
    where makeVariant (_, suffix, price, qty, weight, prod) =
            (,) (productBaseSku prod) $
            ProductVariant
                (toSqlKey 0)
                suffix
                (Cents . round $ 100 * price)
                (floor qty)
                (Milligrams . round $ 1000 * weight)
                True


makeSeedAttributes :: MySQLConn -> IO [(T.Text, SeedAttribute)]
makeSeedAttributes mysql = do
    (_, attributeStream) <- query_ mysql
        "SELECT p.products_id, products_model, is_eco, is_organic, is_heirloom, is_southern FROM sese_products_icons as i RIGHT JOIN products AS p ON p.products_id=i.products_id WHERE p.products_status=1"
    attrs <- Streams.toList attributeStream
    return . nubBy (\a1 a2 -> fst a1 == fst a2) . flip map attrs $
        \[MySQLInt32 _, MySQLText prodSku, MySQLInt8 isEco, MySQLInt8 isOrg, MySQLInt8 isHeir, MySQLInt8 isRegion] ->
            let
                (baseSku, _) = splitSku prodSku
            in
                (,) baseSku $
                    SeedAttribute (toSqlKey 0) (toBool isOrg) (toBool isHeir)
                        (toBool isEco) (toBool isRegion)
            where toBool = (==) 1



splitSku :: T.Text -> (T.Text, T.Text)
splitSku fullSku =
    case T.split isAlpha fullSku of
         [baseSku, ""] ->
            case T.stripPrefix baseSku fullSku of
                Just skuSuffix ->
                    (baseSku, skuSuffix)
                Nothing ->
                    (fullSku, "")
         _ ->
            (fullSku, "")

insertVariants :: [(T.Text, ProductVariant)] -> SqlWriteT IO ()
insertVariants =
    mapM_ insertVariant
    where insertVariant (baseSku, variant) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    lift . putStrLn $ "No product for: " ++ show variant
                Just (Entity prodId _) ->
                    void . insert $ variant { productVariantProductId = prodId }

insertAttributes :: [(T.Text, SeedAttribute)] -> SqlWriteT IO ()
insertAttributes =
    mapM_ insertAttribute
    where insertAttribute (baseSku, attribute) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    return ()
                Just (Entity prodId _) ->
                    void . insert $ attribute { seedAttributeProductId = prodId }
