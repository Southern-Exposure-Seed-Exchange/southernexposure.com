{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Server
    ( App
    , AppSQL
    , runDB
    ) where

import Control.Monad.Reader (ReaderT, asks, lift)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Servant (Handler)

import Config


-- | `App` is the monad stack used in the Servant Handlers. It wraps
-- Servant's `Handler` monad in a `ReaderT` monad that holds the server's
-- `Config` data.
type App =
    ReaderT Config Handler

type AppSQL =
    SqlPersistT Handler

-- | Run & return a database query.
runDB :: AppSQL a -> App a
runDB query =
    asks getPool >>= lift . runSqlPool query
