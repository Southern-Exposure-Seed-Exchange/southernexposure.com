{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- Export the StoneEdge Import XML for an Order. -}

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Time (utcToLocalTime, getTimeZone)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, createPostgresqlPool, runSqlPool
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

import Models
import Models.Fields
import Routes.StoneEdge
import StoneEdge

import qualified Data.ByteString as BS
import qualified Database.Esqueleto as E

main :: IO ()
main = do
    args <- getArgs
    case mapMaybe (fmap E.toSqlKey . readMaybe) args of
        [] -> do
            putStrLn "Expected at least 1 argument: ORDER_ID [... ORDER_ID]"
            exitFailure
        orderIds -> do
            let idString = show $ map E.fromSqlKey orderIds
            putStrLn "Connecting to Database"
            psql <- connectToPostgres
            putStrLn $ "Fetching Orders Numbers: " <> idString
            orders <- flip runSqlPool psql $ fetchOrder orderIds
            putStrLn $ "Writing Order XML to ./order-export.xml"
            BS.writeFile "order-export.xml"
                . renderDownloadOrdersResponse
                $ DownloadOrdersResponse orders
            putStrLn "Script Completed Successfully!"

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

-- | Ripped from Routes.StoneEdge.downloadOrdersRoute
fetchOrder :: [OrderId] -> SqlPersistT IO [StoneEdgeOrder]
fetchOrder orderIds = do
    orders <- E.select $ E.from $ \(o `E.InnerJoin` c `E.InnerJoin` sa `E.LeftOuterJoin` ba `E.LeftOuterJoin` cp) -> do
        E.on (o E.^. OrderCouponId E.==. cp E.?. CouponId)
        E.on (o E.^. OrderBillingAddressId E.==. ba E.?. AddressId)
        E.on (o E.^. OrderShippingAddressId E.==. sa E.^. AddressId)
        E.on (o E.^. OrderCustomerId E.==. c E.^. CustomerId)
        E.where_ $ o E.^. OrderId `E.in_` E.valList orderIds
        E.orderBy [E.asc $ o E.^. OrderId]
        return (o, c, sa, ba, cp)
    orderData <- forM orders $ \(o, c, sa, ba, cp) -> do
        createdAt <- convertToLocalTime $ orderCreatedAt $ entityVal o
        adminComments <- forM (orderAdminComments $ entityVal o) $ \comment ->
            (adminCommentContent comment,)
                <$> convertToLocalTime (adminCommentTime comment)
        lineItems <- selectList [OrderLineItemOrderId ==. entityKey o] []
        products <- E.select $ E.from $ \(op `E.InnerJoin` v `E.InnerJoin` p) -> do
            E.on (v E.^. ProductVariantProductId E.==. p E.^. ProductId)
            E.on (op E.^. OrderProductProductVariantId E.==. v E.^. ProductVariantId)
            E.where_ $ op E.^. OrderProductOrderId E.==. E.val (entityKey o)
            return (op, p, v)
        return (o, createdAt, c, sa, ba, cp, lineItems, products, adminComments)
    return $ map transformOrder orderData
    where
        convertToLocalTime utcTime = do
            timeZone <- liftIO $ getTimeZone utcTime
            return $ utcToLocalTime timeZone utcTime
