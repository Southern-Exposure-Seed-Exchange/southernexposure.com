module Config
    ( Environment(..)
    , Config(..)
    , defaultConfig
    , smtpPool
    ) where

import Data.Pool (Pool, createPool)
import Database.Persist.Sql (ConnectionPool)
import Network.HaskellNet.SMTP.SSL (SMTPConnection, connectSMTPSTARTTLS, closeSMTP)


data Environment
    = Production
    | Development
    deriving (Eq, Show, Read)

data Config
    = Config
    { getPool :: ConnectionPool
    , getEnv :: Environment
    , getMediaDirectory :: FilePath
    , getSmtpPool :: Pool SMTPConnection
    , getSmtpUser :: String
    , getSmtpPass :: String
    }

defaultConfig :: Config
defaultConfig =
    Config
        { getPool = undefined
        , getEnv = Development
        , getMediaDirectory = undefined
        , getSmtpPool = undefined
        , getSmtpUser = undefined
        , getSmtpPass = undefined
        }


smtpPool :: String -> Int -> IO (Pool SMTPConnection)
smtpPool serverName =
    createPool (connectSMTPSTARTTLS serverName) closeSMTP 1 20
