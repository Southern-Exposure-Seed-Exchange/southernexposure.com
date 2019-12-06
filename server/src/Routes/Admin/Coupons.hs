{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Coupons
    ( CouponAPI
    , couponRoutes
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), (.=), (.:), object, withObject, withText)
import Data.Time
    ( UTCTime, LocalTime(..), localTimeToUTC, getCurrentTimeZone
    , getCurrentTime
    )
import Database.Persist (Entity(..), SelectOpt(Desc), selectList, insert)
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models (EntityField(..), Unique(..), Coupon(..), CouponId)
import Models.Fields (Cents, CouponType(..))
import Server (App, runDB)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Validation as V


type CouponAPI =
         "list" :> CouponListRoute
    :<|> "new" :> NewCouponRoute

type CouponRoutes =
         (WrappedAuthToken -> App (Cookied CouponListData))
    :<|> (WrappedAuthToken -> NewCouponParameters -> App (Cookied CouponId))

couponRoutes :: CouponRoutes
couponRoutes =
         couponListRoute
    :<|> newCouponRoute


-- LIST

type CouponListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CouponListData)

newtype CouponListData =
    CouponListData
        { cldCoupons :: [ListCoupon]
        } deriving (Show)

instance ToJSON CouponListData where
    toJSON CouponListData {..} =
        object
            [ "coupons" .= cldCoupons
            ]

data ListCoupon =
    ListCoupon
     { lcId :: CouponId
     , lcCode :: T.Text
     , lcName :: T.Text
     , lcIsActive :: Bool
     , lcType :: CouponType
     , lcExpiration :: UTCTime
     } deriving (Show)

instance ToJSON ListCoupon where
    toJSON ListCoupon {..} =
        object
            [ "id" .= lcId
            , "code" .= lcCode
            , "name" .= lcName
            , "isActive" .= lcIsActive
            , "type" .= lcType
            , "expires" .= lcExpiration
            ]

couponListRoute :: WrappedAuthToken -> App (Cookied CouponListData)
couponListRoute = flip withAdminCookie $ const $
    CouponListData . map makeListCoupon <$> runDB (selectList [] [Desc CouponExpirationDate])
  where
    makeListCoupon :: Entity Coupon -> ListCoupon
    makeListCoupon (Entity couponId Coupon {..}) =
        ListCoupon
            { lcId = couponId
            , lcCode = couponCode
            , lcName = couponName
            , lcIsActive = couponIsActive
            , lcType = couponDiscount
            , lcExpiration = couponExpirationDate
            }


-- NEW

type NewCouponRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] NewCouponParameters
    :> Post '[JSON] (Cookied CouponId)

data NewCouponParameters =
    NewCouponParameters
        { ncpCode :: T.Text
        , ncpName :: T.Text
        , ncpDescription :: T.Text
        , ncpIsActive :: Bool
        , ncpDiscount :: CouponType
        , ncpMinimumOrder :: Cents
        , ncpExpirationDate :: LocalTime
        -- ^ Send an ISO8601 UTC Datetime(with a trailing `Z`)
        , ncpTotalUses :: Natural
        , ncpUsesPerCustomer :: Natural
        } deriving (Show)

instance FromJSON NewCouponParameters where
    parseJSON = withObject "NewCouponParameters" $ \v -> do
        ncpCode <- v .: "code"
        ncpName <- v .: "name"
        ncpDescription <- v .: "description"
        ncpIsActive <- v .: "isActive"
        ncpDiscount <- v .: "discount"
        ncpMinimumOrder <- v .: "minimumOrder"
        ncpExpirationDate <- v .: "expires" >>= parseExpires
        ncpTotalUses <- v .: "totalUses"
        ncpUsesPerCustomer <- v .: "usesPerCustomer"
        return NewCouponParameters {..}
      where
        -- Drop the trailing `Z` from the ISO8601 UTC datetime so the
        -- parsing of LocalTime will succeed.
        parseExpires =
            withText "expires" (parseJSON . String . T.dropEnd 1)

instance Validation NewCouponParameters where
    validators NewCouponParameters {..} = do
        codeDoesntExist <- V.doesntExist $ UniqueCoupon ncpCode
        return
            [ ( "code"
              , [ V.required ncpCode
                , ( "A Coupon with this Code already exists.", codeDoesntExist )
                ]
              )
            , ( "name", [ V.required ncpName ] )
            ]

newCouponRoute :: WrappedAuthToken -> NewCouponParameters -> App (Cookied CouponId)
newCouponRoute = validateAdminAndParameters $ \_ NewCouponParameters {..} -> do
    currentTime <- liftIO getCurrentTime
    timeZone <- liftIO getCurrentTimeZone
    let expiration = localTimeToUTC timeZone ncpExpirationDate
    runDB $ insert Coupon
        { couponCode = ncpCode
        , couponName = ncpName
        , couponDescription = ncpDescription
        , couponIsActive = ncpIsActive
        , couponDiscount = ncpDiscount
        , couponMinimumOrder = ncpMinimumOrder
        , couponExpirationDate = expiration
        , couponTotalUses = ncpTotalUses
        , couponUsesPerCustomer = ncpUsesPerCustomer
        , couponCreatedAt = currentTime
        }
