{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (MonadLogger, runNoLoggingT, runStderrLoggingT)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Database.Persist.Postgresql
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Log.FastLogger (LoggerSet, newStdoutLoggerSet, newFileLoggerSet, defaultBufSize)
import Web.Stripe.Client (StripeConfig(..), StripeKey(..))

import Api
import Auth (sessionEntropy, mkPersistentServerKey)
import Cache (initializeCaches)
import Config
import Models
import Paths_sese_website (version)
import StoneEdge (StoneEdgeCredentials(..))

import qualified Avalara
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | Connect to the database, configure the application, & start the server.
--
-- TODO: Document enviornmental variables in README.
main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3000
    mediaDir <- lookupDirectory "MEDIA" "/media/"
    (avalaraLogger, stripeLogger, serverLogger) <- makeLoggers env
    smtpServer <- fromMaybe "" <$> lookupEnv "SMTP_SERVER"
    smtpUser <- fromMaybe "" <$> lookupEnv "SMTP_USER"
    smtpPass <- fromMaybe "" <$> lookupEnv "SMTP_PASS"
    emailPool <- smtpPool smtpServer (poolSize env)
    stripeToken <- fromMaybe "" <$> lookupEnv "STRIPE_TOKEN"
    cookieSecret <- mkPersistentServerKey . C.pack . fromMaybe ""
        <$> lookupEnv "COOKIE_SECRET"
    entropySource <- sessionEntropy
    stoneEdgeAuth <- makeStoneEdgeAuth
    avalaraConfig <- makeAvalaraConfig
    avalaraCompanyId <- Avalara.CompanyId <$> lookupSetting "AVATAX_COMPANY_ID" 0
    avalaraCompanyCode <- Avalara.CompanyCode . T.pack <$> requireSetting "AVATAX_COMPANY_CODE"
    avalaraSourceLocation <- lookupSetting "AVATAX_LOCATION_CODE" "DEFAULT"
    dbPool <- makePool env
    cache <- runSqlPool initializeCaches dbPool >>= newTVarIO
    let cfg = defaultConfig
            { getPool = dbPool
            , getEnv = env
            , getCaches = cache
            , getMediaDirectory = mediaDir
            , getSmtpPool = emailPool
            , getSmtpUser = smtpUser
            , getSmtpPass = smtpPass
            , getStripeConfig = StripeConfig $ StripeKey $ C.pack stripeToken
            , getStoneEdgeAuth = stoneEdgeAuth
            , getCookieSecret = cookieSecret
            , getCookieEntropySource = entropySource
            , getAvalaraConfig = avalaraConfig
            , getAvalaraCompanyId = avalaraCompanyId
            , getAvalaraCompanyCode = avalaraCompanyCode
            , getAvalaraSourceLocationCode = avalaraSourceLocation
            , getAvalaraLogger = avalaraLogger
            , getStripeLogger = stripeLogger
            , getServerLogger = serverLogger
            }
    Warp.runSettings (warpSettings port) . httpLogger env $ app cfg
    where lookupSetting env def =
            maybe def read <$> lookupEnv env
          requireSetting :: String -> IO String
          requireSetting env =
            lookupEnv env
                >>= maybe (error $ "Could not find required env variable: " ++ env) return
          makePool :: Environment -> IO ConnectionPool
          makePool env =
            case env of
                Production ->
                    runNoLoggingT $ makeSqlPool env
                Development ->
                    runStderrLoggingT $ makeSqlPool env
          makeSqlPool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => Environment -> m ConnectionPool
          makeSqlPool env = do
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
              $ Warp.setPort port Warp.defaultSettings
          httpLogger env =
              case env of
                Production ->
                    logStdout
                Development ->
                    logStdoutDev
          makeStoneEdgeAuth = do
            secUsername <- T.pack . fromMaybe "" <$> lookupEnv "STONE_EDGE_USER"
            secPassword <- T.pack . fromMaybe "" <$> lookupEnv "STONE_EDGE_PASS"
            secStoreCode <- T.pack . fromMaybe "" <$> lookupEnv "STONE_EDGE_CODE"
            return StoneEdgeCredentials {..}
          makeAvalaraConfig :: IO Avalara.Config
          makeAvalaraConfig = do
            let cAppName = "SESE Retail Website"
                cAppVersion = T.pack $ showVersion version
            cAccountId <- Avalara.AccountId . T.pack
                <$> requireSetting "AVATAX_ACCOUNT_ID"
            cLicenseKey <- Avalara.LicenseKey . T.pack
                <$> requireSetting "AVATAX_LICENSE_KEY"
            rawEnvironment <- T.pack <$> requireSetting "AVATAX_ENVIRONMENT"
            cServiceEnvironment <- case T.toLower rawEnvironment of
                "sandbox" ->
                    return Avalara.SandboxEnvironment
                "production" ->
                    return Avalara.ProductionEnvironment
                _ ->
                    error $ "Invalid AVATAX_ENVIRONMENT: " ++ T.unpack rawEnvironment
                        ++ "\n\n\tValid value are `Production` or `Sandbox`"
            return Avalara.Config {..}
          makeLoggers :: Environment -> IO (LoggerSet, LoggerSet, LoggerSet)
          makeLoggers env =
            case env of
                Development ->
                    (,,)
                        <$> newStdoutLoggerSet defaultBufSize
                        <*> newStdoutLoggerSet defaultBufSize
                        <*> newStdoutLoggerSet defaultBufSize
                Production -> do
                    logDir <- lookupDirectory "LOGS" "/logs/"
                    (,,)
                        <$> newFileLoggerSet defaultBufSize (logDir ++ "/avalara.log")
                        <*> newFileLoggerSet defaultBufSize (logDir ++ "/stripe.log")
                        <*> newFileLoggerSet defaultBufSize (logDir ++ "/server.log")
          lookupDirectory :: String -> FilePath -> IO FilePath
          lookupDirectory envName defaultPath = do
                dir <- lookupEnv envName
                    >>= maybe ((++ defaultPath) <$> getCurrentDirectory) return
                createDirectoryIfMissing True dir
                return dir
