{-# LANGUAGE OverloadedStrings #-}
module Config
    ( Environment(..)
    , Config(..)
    , defaultConfig
    , smtpPool
    ) where

import Control.Concurrent.STM (TVar)
import Data.Pool (Pool, createPool)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool)
import Network.HaskellNet.SMTP.SSL (SMTPConnection, connectSMTPSTARTTLS, closeSMTP)
import Servant.Server.Experimental.Auth.Cookie (PersistentServerKey, RandomSource)
import System.Log.FastLogger (LoggerSet)
import Web.Stripe.Client (StripeConfig)

import Cache (Caches)
import StoneEdge (StoneEdgeCredentials)

import qualified Avalara


data Environment
    = Production
    | Development
    deriving (Eq, Show, Read)

data Config
    = Config
    { getPool :: ConnectionPool
    , getEnv :: Environment
    , getCaches :: TVar Caches
    , getMediaDirectory :: FilePath
    , getSmtpPool :: Pool SMTPConnection
    , getSmtpUser :: String
    , getSmtpPass :: String
    , getStripeConfig :: StripeConfig
    , getStoneEdgeAuth :: StoneEdgeCredentials
    , getCookieSecret :: PersistentServerKey
    , getCookieEntropySource :: RandomSource
    , getAvalaraConfig :: Avalara.Config
    , getAvalaraCompanyId :: Avalara.CompanyId
    , getAvalaraCompanyCode :: Avalara.CompanyCode
    , getAvalaraSourceLocationCode :: Text
    , getAvalaraLogger :: LoggerSet
    , getStripeLogger :: LoggerSet
    , getServerLogger :: LoggerSet
    }

defaultConfig :: Config
defaultConfig =
    Config
        { getPool = undefined
        , getEnv = Development
        , getCaches = undefined
        , getMediaDirectory = undefined
        , getSmtpPool = undefined
        , getSmtpUser = undefined
        , getSmtpPass = undefined
        , getStripeConfig = undefined
        , getStoneEdgeAuth = undefined
        , getCookieSecret = undefined
        , getCookieEntropySource = undefined
        , getAvalaraConfig = undefined
        , getAvalaraCompanyId = undefined
        , getAvalaraCompanyCode = undefined
        , getAvalaraSourceLocationCode = "DEFAULT"
        , getAvalaraLogger = undefined
        , getStripeLogger = undefined
        , getServerLogger = undefined
        }


smtpPool :: String -> Int -> IO (Pool SMTPConnection)
smtpPool serverName =
    createPool (connectSMTPSTARTTLS serverName) closeSMTP 1 20
