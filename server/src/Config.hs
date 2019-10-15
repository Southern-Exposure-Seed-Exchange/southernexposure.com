module Config
    ( Environment(..)
    , Config(..)
    , defaultConfig
    , smtpPool
    ) where

import Control.Concurrent.STM (TVar)
import Data.Pool (Pool, createPool)
import Database.Persist.Sql (ConnectionPool)
import Network.HaskellNet.SMTP.SSL (SMTPConnection, connectSMTPSTARTTLS, closeSMTP)
import Web.Stripe.Client (StripeConfig)

import Cache (Caches)
import StoneEdge (StoneEdgeCredentials)


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
        }


smtpPool :: String -> Int -> IO (Pool SMTPConnection)
smtpPool serverName =
    createPool (connectSMTPSTARTTLS serverName) closeSMTP 1 20
