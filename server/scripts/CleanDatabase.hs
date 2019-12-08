{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist ((<.), deleteWhere)
import Database.Persist.Postgresql
    ( ConnectionPool, SqlWriteT, createPostgresqlPool, runSqlPool
    )

import Models

import qualified Database.Esqueleto as E

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
    deactivateCoupons currentTime

-- | De-activate coupons whose expiration date has passed and coupons that
-- have reached their maximum number of uses.
deactivateCoupons :: UTCTime -> SqlWriteT IO ()
deactivateCoupons currentTime =
    E.update $ \c -> do
        E.set c [ CouponIsActive E.=. E.val False ]
        let orderCount = E.sub_select $ E.from $ \o -> do
                E.where_ $ o E.^. OrderCouponId E.==. E.just (c E.^. CouponId)
                return E.countRows
        E.where_ $
            (orderCount E.>=. c E.^. CouponTotalUses E.&&. c E.^. CouponTotalUses E.!=. E.val 0)
            E.||.  E.val currentTime E.>. c E.^. CouponExpirationDate
