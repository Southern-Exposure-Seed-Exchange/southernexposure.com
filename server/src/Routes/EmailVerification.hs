{-# LANGUAGE RecordWildCards #-}

module Routes.EmailVerification
  ( newEmailVerification
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
    ( Entity(..), getBy, insertUnique
    )
import Servant
    ( err500
    )

import Models
import Routes.Utils (generateUniqueToken)
import Server
import Workers (Task(..), enqueueTask)

import qualified Database.Esqueleto.Experimental as E
import qualified Emails
import Data.Foldable (for_)

newEmailVerification :: CustomerId -> App ()
newEmailVerification customerId = join $ runDB $ do
    mOldVer <- fmap entityKey <$> getBy (UniqueCustomerToVerify customerId)
    for_ mOldVer $ \verId -> E.delete $ do
        ver <- E.from $ E.table @Verification
        E.where_ (ver E.^. VerificationId E.==. E.val verId)
    verificationCreatedAt <- liftIO getCurrentTime
    verificationCode <- generateUniqueToken UniqueVerificationCode
    let verification = Verification
            { verificationCustomerId = customerId, ..
            }
    mVer <- insertUnique verification
    case mVer of
        Nothing -> pure (serverError err500)
        Just verId -> do
            enqueueTask Nothing (SendEmail $ Emails.EmailVerification customerId verId)
            pure (pure ())
