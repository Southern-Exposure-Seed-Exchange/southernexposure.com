{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( lookupSettingWith
    , makeSqlPool
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool)
import Data.ByteString.Char8 (pack)
import System.Environment (lookupEnv)
import UnliftIO (MonadUnliftIO)

lookupSettingWith :: String -> a -> (String -> a) -> IO a
lookupSettingWith env def builder = maybe def builder <$> lookupEnv env

makeSqlPool :: (MonadIO m, MonadLoggerIO m, MonadUnliftIO m) => Int -> m ConnectionPool
makeSqlPool poolSize = do
    mbDbConnectionString <- liftIO $ lookupEnv "DB_CONNECTION_STRING"
    connStr <- case mbDbConnectionString of
        Just connStr -> return $ pack connStr
        Nothing -> do
            dbHost <- liftIO $ lookupSettingWith "DB_HOST" "127.0.0.1" pack
            dbPort <- liftIO $ lookupSettingWith "DB_PORT" "5432" pack
            dbUser <- liftIO $ lookupSettingWith "DB_USER" "sese-website" pack
            mbDbPass <- liftIO $ lookupSettingWith "DB_PASS" Nothing (Just . pack)
            dbName <- liftIO $ lookupSettingWith "DB_NAME" "sese-website" pack
            let connStr = "host=" <> dbHost
                    <> " port=" <> dbPort
                    <> " user=" <> dbUser
                    <> maybe "" (" password=" <>) mbDbPass
                    <> " dbname=" <> dbName
            return connStr

    createPostgresqlPool connStr poolSize

