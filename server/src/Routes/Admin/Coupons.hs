{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Coupons
    ( CouponAPI
    , couponRoutes
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import Data.Maybe (catMaybes)
import Data.Time
    ( UTCTime, LocalTime(..), Day, localTimeToUTC, getCurrentTimeZone
    , getCurrentTime, utcToLocalTime
    )
import Database.Persist
    ( Entity(..), SelectOpt(Desc), Update, selectList, insert, get, update
    , OverflowNatural(..)
    )
import Numeric.Natural (Natural)
import Servant
    ( (:<|>)(..), (:>), AuthProtect, Capture, ReqBody, Get, Post, Patch, JSON
    , err404
    )

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models (EntityField(..), Unique(..), Coupon(..), CouponId)
import Models.Fields (Cents, CouponType(..))
import Server (App, runDB, serverError)
import Routes.Utils (mapUpdate, parseLocalTime, parseMaybeLocalTime)
import Validation (Validation(..))
import Data.Coerce(coerce)

import qualified Data.Text as T
import qualified Validation as V


type CouponAPI =
         "list" :> CouponListRoute
    :<|> "new" :> NewCouponRoute
    :<|> "edit" :> EditCouponDataRoute
    :<|> "edit" :> EditCouponRoute

type CouponRoutes =
         (WrappedAuthToken -> App (Cookied CouponListData))
    :<|> (WrappedAuthToken -> NewCouponParameters -> App (Cookied CouponId))
    :<|> (WrappedAuthToken -> CouponId -> App (Cookied EditCouponData))
    :<|> (WrappedAuthToken -> EditCouponParameters -> App (Cookied ()))

couponRoutes :: CouponRoutes
couponRoutes =
         couponListRoute
    :<|> newCouponRoute
    :<|> editCouponDataRoute
    :<|> editCouponRoute


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
        ncpExpirationDate <- v .: "expires" >>= parseLocalTime
        ncpTotalUses <- v .: "totalUses"
        ncpUsesPerCustomer <- v .: "usesPerCustomer"
        return NewCouponParameters {..}

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
        , couponTotalUses = OverflowNatural ncpTotalUses
        , couponUsesPerCustomer = OverflowNatural ncpUsesPerCustomer
        , couponCreatedAt = currentTime
        }


-- EDIT

type EditCouponDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" CouponId
    :> Get '[JSON] (Cookied EditCouponData)

data EditCouponData =
    EditCouponData
        { ecdId :: CouponId
        , ecdCode :: T.Text
        , ecdName :: T.Text
        , ecdDescription :: T.Text
        , ecdIsActive :: Bool
        , ecdDiscount :: CouponType
        , ecdMinimumOrder :: Cents
        , ecdExpirationDate :: Day
        , ecdTotalUses :: Natural
        , ecdUsesPerCustomer :: Natural
        } deriving (Show)

instance ToJSON EditCouponData where
    toJSON EditCouponData {..} =
        object
            [ "id" .= ecdId
            , "code" .= ecdCode
            , "name" .= ecdName
            , "description" .= ecdDescription
            , "isActive" .= ecdIsActive
            , "discount" .= ecdDiscount
            , "minimumOrder" .= ecdMinimumOrder
            , "expires" .= ecdExpirationDate
            , "totalUses" .= ecdTotalUses
            , "usesPerCustomer" .= ecdUsesPerCustomer
            ]

editCouponDataRoute :: WrappedAuthToken -> CouponId -> App (Cookied EditCouponData)
editCouponDataRoute t couponId = withAdminCookie t $ \_ ->
    runDB (get couponId) >>= \case
        Nothing ->
            serverError err404
        Just Coupon {..} -> do
            timezone <- liftIO getCurrentTimeZone
            let localTime = utcToLocalTime timezone couponExpirationDate
            return EditCouponData
                { ecdId = couponId
                , ecdCode = couponCode
                , ecdName = couponName
                , ecdDescription = couponDescription
                , ecdIsActive = couponIsActive
                , ecdDiscount = couponDiscount
                , ecdMinimumOrder = couponMinimumOrder
                , ecdExpirationDate = localDay localTime
                , ecdTotalUses = unOverflowNatural couponTotalUses
                , ecdUsesPerCustomer = unOverflowNatural couponUsesPerCustomer
                }


type EditCouponRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] EditCouponParameters
    :> Patch '[JSON] (Cookied ())

data EditCouponParameters =
    EditCouponParameters
        { ecpId :: CouponId
        , ecpCode :: Maybe T.Text
        , ecpName :: Maybe T.Text
        , ecpDescription :: Maybe T.Text
        , ecpIsActive :: Maybe Bool
        , ecpDiscount :: Maybe CouponType
        , ecpMinimumOrder :: Maybe Cents
        , ecpExpirationDate :: Maybe LocalTime
        -- ^ Send an ISO8601 UTC Datetime(with a trailing `Z`)
        , ecpTotalUses :: Maybe Natural
        , ecpUsesPerCustomer :: Maybe Natural
        }

instance FromJSON EditCouponParameters where
    parseJSON = withObject "EditCouponParameters" $ \v -> do
        ecpId <- v .: "id"
        ecpCode <- v .: "code"
        ecpName <- v .: "name"
        ecpDescription <- v .: "description"
        ecpIsActive <- v .: "isActive"
        ecpDiscount <- v .: "discount"
        ecpMinimumOrder <- v .: "minimumOrder"
        ecpExpirationDate <- v .: "expires" >>= parseMaybeLocalTime
        ecpTotalUses <- v .: "totalUses"
        ecpUsesPerCustomer <- v .: "usesPerCustomer"
        return EditCouponParameters {..}

instance Validation EditCouponParameters where
    validators EditCouponParameters {..} = do
        codeCheck <- case ecpCode of
            Nothing -> return []
            Just code -> do
                -- TODO: Similar to slugCheck in EditCouponParameters
                doesntExist <- V.doesntExist $ UniqueCoupon code
                return
                    [ ( "code"
                      , [ V.required code
                        , ( "A Coupon with this Code already exists."
                          , doesntExist
                          )
                        ]
                      )
                    ]
        return $ catMaybes
            [ V.mapCheck ecpName $ \name ->
                ( "name"
                , [ V.required name ]
                )
            ]
            ++ codeCheck

editCouponRoute :: WrappedAuthToken -> EditCouponParameters -> App (Cookied ())
editCouponRoute = validateAdminAndParameters $ \_ parameters -> do
    timezone <- liftIO getCurrentTimeZone
    let maybeExpires = localTimeToUTC timezone <$> ecpExpirationDate parameters
        updates = makeUpdates parameters maybeExpires
    unless (null updates) $
        runDB $ update (ecpId parameters) updates
  where
    makeUpdates :: EditCouponParameters -> Maybe UTCTime -> [Update Coupon]
    makeUpdates EditCouponParameters {..} expires =
        catMaybes
            [ mapUpdate CouponCode ecpCode
            , mapUpdate CouponName ecpName
            , mapUpdate CouponDescription ecpDescription
            , mapUpdate CouponIsActive ecpIsActive
            , mapUpdate CouponDiscount ecpDiscount
            , mapUpdate CouponMinimumOrder ecpMinimumOrder
            , mapUpdate CouponExpirationDate expires
            , mapUpdate CouponTotalUses (coerce ecpTotalUses)
            , mapUpdate CouponUsesPerCustomer (coerce ecpUsesPerCustomer)
            ]
