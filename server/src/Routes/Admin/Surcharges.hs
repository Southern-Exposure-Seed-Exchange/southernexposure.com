{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Surcharges
    ( SurchargesAPI
    , surchargesRoutes
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), withObject, object)
import Data.Maybe (isNothing, mapMaybe)
import Database.Persist ((/<-.), Entity(..), replace, selectList, deleteWhere, insertMany_)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (Cents)
import Routes.CommonData (AdminCategorySelect, makeAdminCategorySelects, validateCategorySelect)
import Server
import Validation (Validation(..))

import qualified Data.List as L
import qualified Data.Text as T
import qualified Validation as V


type SurchargesAPI =
         "data" :> SurchargesDataRoute
    :<|> "update" :> SurchargesUpdateRoute

type SurchargeRoutes =
         (WrappedAuthToken -> App (Cookied SurchargesData))
    :<|> (WrappedAuthToken -> SurchagesUpdateParameters -> App (Cookied ()))

surchargesRoutes :: SurchargeRoutes
surchargesRoutes =
         surchargesDataRoute
    :<|> surchargesUpdateRoute


-- DATA

type SurchargesDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied SurchargesData)

data SurchargesData =
    SurchargesData
        { sdSurcharges :: [SurchargeData]
        , sdAllCategories :: [AdminCategorySelect]
        } deriving (Show)

instance ToJSON SurchargesData where
    toJSON SurchargesData {..} =
        object
            [ "surcharges" .= sdSurcharges
            , "categories" .= sdAllCategories
            ]

data SurchargeData =
    SurchargeData
        { sdId :: Maybe SurchargeId
        , sdDescription :: T.Text
        , sdSingleFee :: Cents
        , sdMultipleFee :: Cents
        , sdCategories :: [CategoryId]
        , sdIsActive :: Bool
        } deriving (Show)

instance ToJSON SurchargeData where
    toJSON SurchargeData {..} =
        object
            [ "id" .= sdId
            , "description" .= sdDescription
            , "singleFee" .= sdSingleFee
            , "multipleFee" .= sdMultipleFee
            , "categories" .= sdCategories
            , "isActive" .= sdIsActive
            ]

instance FromJSON SurchargeData where
    parseJSON = withObject "SurchargeData" $ \v -> do
        sdId <- v .: "id"
        sdDescription <- v .: "description"
        sdSingleFee <- v .: "singleFee"
        sdMultipleFee <- v .: "multipleFee"
        sdCategories <- v .: "categories"
        sdIsActive <- v .: "isActive"
        return SurchargeData {..}

surchargesDataRoute :: WrappedAuthToken -> App (Cookied SurchargesData)
surchargesDataRoute t = withAdminCookie t $ \_ -> do
    (categories, surcharges) <- runDB $
        (,) <$> makeAdminCategorySelects <*> selectList [] []
    return SurchargesData
        { sdSurcharges = map makeSurchargeData surcharges
        , sdAllCategories = categories
        }
  where
    makeSurchargeData :: Entity Surcharge -> SurchargeData
    makeSurchargeData (Entity sId sur) =
        SurchargeData
            { sdId = Just sId
            , sdDescription = surchargeDescription sur
            , sdSingleFee = surchargeSingleFee sur
            , sdMultipleFee = surchargeMultipleFee sur
            , sdCategories = surchargeCategoryIds sur
            , sdIsActive = surchargeIsActive sur
            }



-- UPDATE

type SurchargesUpdateRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] SurchagesUpdateParameters
    :> Post '[JSON] (Cookied ())

newtype SurchagesUpdateParameters =
    SurchagesUpdateParameters
        { supSurcharges :: [SurchargeData]
        } deriving (Show)

instance FromJSON SurchagesUpdateParameters where
    parseJSON = withObject "SurchagesUpdateParameters" $ \v -> do
        supSurcharges <- v .: "surcharges"
        return SurchagesUpdateParameters {..}

instance Validation SurchagesUpdateParameters where
    validators SurchagesUpdateParameters {..} =
        V.indexedValidation "surcharge" validateSurcharge supSurcharges
      where
        validateSurcharge :: SurchargeData -> App [(T.Text, [(T.Text, Bool)])]
        validateSurcharge SurchargeData {..} =
            validateCategorySelect True sdCategories

surchargesUpdateRoute :: WrappedAuthToken -> SurchagesUpdateParameters -> App (Cookied ())
surchargesUpdateRoute = validateAdminAndParameters $ \_ SurchagesUpdateParameters {..} -> do
    let (new, existing) = L.partition (isNothing . sdId) supSurcharges
    runDB $ do
        deleteWhere [SurchargeId /<-. mapMaybe sdId existing]
        insertMany_ $ map convert new
        mapM_ (\s -> mapM_ (\i -> replace i $ convert s ) $ sdId s ) existing
    return ()
  where
    convert :: SurchargeData -> Surcharge
    convert SurchargeData {..} =
        Surcharge
            { surchargeDescription = sdDescription
            , surchargeSingleFee = sdSingleFee
            , surchargeMultipleFee = sdMultipleFee
            , surchargeCategoryIds = sdCategories
            , surchargeIsActive = sdIsActive
            }
