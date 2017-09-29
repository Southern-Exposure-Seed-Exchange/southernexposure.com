{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (getCurrentTime)
import Database.Persist ((<.), deleteWhere)
import Database.Persist.Postgresql
    (ConnectionPool, SqlWriteT, createPostgresqlPool, runSqlPool)

import Models

main :: IO ()
main =
    connectToPostgres >>= runSqlPool cleanDatabase

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1

cleanDatabase :: SqlWriteT IO ()
cleanDatabase = do
    currentTime <- lift getCurrentTime
    deleteWhere [PasswordResetExpirationTime <. currentTime]
    deleteWhere [CartExpirationTime <. Just currentTime]
