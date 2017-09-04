{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Database.MySQL.Base
    (MySQLConn, ConnectInfo(..), connect, defaultConnectInfo)
import System.Environment (lookupEnv)

import qualified Data.ByteString.Char8 as BS

connectToMysql :: IO MySQLConn
connectToMysql = do
    mysqlUser <- maybe "" BS.pack <$> lookupEnv "DB_USER"
    mysqlPass <- maybe "" BS.pack <$> lookupEnv "DB_PASS"
    connect $ defaultConnectInfo
            { ciUser = mysqlUser
            , ciDatabase = "retailzen"
            , ciPassword = mysqlPass
            }
