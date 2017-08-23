{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import System.Environment (lookupEnv)

import Api
import Config
import Models

-- | Connect to the database, configure the application, & start the server.
main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3000
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
    run port . logger env $ app cfg
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
          logger env =
              case env of
                Production ->
                    logStdout
                Development ->
                    logStdoutDev
