{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Coupons
    ( CouponAPI
    , couponRoutes
    ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Time (UTCTime)
import Database.Persist (Entity(..), SelectOpt(Desc), selectList)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models (EntityField(..), Coupon(..), CouponId)
import Models.Fields (CouponType(..))
import Server (App, runDB)

import qualified Data.Text as T


type CouponAPI =
         "list" :> CouponListRoute

type CouponRoutes =
         (WrappedAuthToken -> App (Cookied CouponListData))

couponRoutes :: CouponRoutes
couponRoutes =
         couponListRoute


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
