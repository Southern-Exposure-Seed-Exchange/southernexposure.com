{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromMaybe)
import Database.Persist.Postgresql
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Environment (lookupEnv)

import Api
import Config
import Models

import qualified Network.Wai.Handler.Warp as Warp

-- | Connect to the database, configure the application, & start the server.
main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3000
    mediaDir <- lookupSetting "MEDIA" =<< (++ "/media/") <$> getCurrentDirectory
    createDirectoryIfMissing True mediaDir
    smtpServer <- lookupSetting "SMTP_SERVER" "southernexposure.com"
    smtpUser <- fromMaybe "" <$> lookupEnv "SMTP_USER"
    smtpPass <- fromMaybe "" <$> lookupEnv "SMTP_PASS"
    emailPool <- smtpPool smtpServer (poolSize env)
    dbPool <- makePool env
    let cfg = defaultConfig
            { getPool = dbPool
            , getEnv = env
            , getMediaDirectory = mediaDir
            , getSmtpPool = emailPool
            , getSmtpUser = smtpUser
            , getSmtpPass = smtpPass
            }
    Warp.runSettings (warpSettings port) . httpLogger env $ app cfg
    where lookupSetting env def =
            maybe def read <$> lookupEnv env
          makePool :: Environment -> IO ConnectionPool
          makePool env =
            runStderrLoggingT $ do
                pool <- createPostgresqlPool "dbname=sese-website" $ poolSize env
                runSqlPool (runMigration migrateAll) pool
                return pool
          poolSize env =
            case env of
                Production ->
                    8
                Development ->
                    1
          warpSettings port =
              Warp.setServerName ""
              $ Warp.setPort port
              $ Warp.defaultSettings
          httpLogger env =
              case env of
                Production ->
                    logStdout
                Development ->
                    logStdoutDev
