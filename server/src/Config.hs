{-# LANGUAGE OverloadedStrings #-}
module Config
    ( Environment(..)
    , AvalaraStatus(..)
    , Config(..)
    , defaultConfig
    , timedLogStr
    , smtpPool
    ) where

import Control.Concurrent.STM (TVar)
import Data.Monoid ((<>))
import Data.Pool (Pool, createPool)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool)
import Network.Mail.SMTP (SMTPConnection, connectSMTP', closeSMTP)
import Servant.Server.Experimental.Auth.Cookie (PersistentServerKey, RandomSource)
import System.Log.FastLogger (TimedFastLogger, LogStr, FormattedTime, ToLogStr(..))
import Web.Stripe.Client (StripeConfig)

import Cache (Caches)
import StoneEdge (StoneEdgeCredentials)

import qualified Avalara


data Environment
    = Production
    | Development
    deriving (Eq, Show, Read)

data AvalaraStatus
    = AvalaraDisabled
    | AvalaraTesting
    | AvalaraEnabled
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
    , getAvalaraStatus :: AvalaraStatus
    , getAvalaraConfig :: Avalara.Config
    , getAvalaraCompanyId :: Avalara.CompanyId
    , getAvalaraCompanyCode :: Avalara.CompanyCode
    , getAvalaraSourceLocationCode :: Text
    , getAvalaraLogger :: TimedFastLogger
    , getStripeLogger :: TimedFastLogger
    , getServerLogger :: TimedFastLogger
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
        , getAvalaraStatus = AvalaraDisabled
        , getAvalaraConfig = undefined
        , getAvalaraCompanyId = undefined
        , getAvalaraCompanyCode = undefined
        , getAvalaraSourceLocationCode = "DEFAULT"
        , getAvalaraLogger = undefined
        , getStripeLogger = undefined
        , getServerLogger = undefined
        }

timedLogStr :: ToLogStr a => a -> FormattedTime -> LogStr
timedLogStr msg time = "[" <> toLogStr time <> "]: " <> toLogStr msg <> "\n"

smtpPool :: String -> Int -> IO (Pool SMTPConnection)
smtpPool serverName =
    createPool (connectSMTP' serverName 2525) closeSMTP 1 20
