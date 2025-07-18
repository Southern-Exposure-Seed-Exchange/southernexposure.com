{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.ShippingMethods
    ( ShippingMethodsAPI
    , shippingMethodsRoutes
    -- Tests
    , MethodData(..)
    , RateData(..)
    , RateType(..)
    ) where

import Control.Monad.Reader (forM_)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject, withText)
import Data.Maybe (mapMaybe, isJust)
import Database.Persist
    ( (/<-.), Entity(..), SelectOpt(Asc)
    , selectList, deleteWhere, insertMany_, replace)
import Numeric.Natural (Natural)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (Cents(..), Country(..), PriorityShippingFee(..), ShippingRate(..))
import Routes.CommonData (AdminCategorySelect, makeAdminCategorySelects, validateCategorySelect)
import Server
import Validation (Validation(..))

import qualified Data.List as L
import qualified Data.Text as T
import qualified Validation as V


type ShippingMethodsAPI =
         "data" :> ShippingDataRoute
    :<|> "update" :> ShippingUpdateRoute

type ShippingRoutes =
         (WrappedAuthToken -> App (Cookied ShippingData))
    :<|> (WrappedAuthToken -> ShippingUpdateParameters -> App (Cookied ()))

shippingMethodsRoutes :: ShippingRoutes
shippingMethodsRoutes =
         shippingDataRoute
    :<|> shippingUpdateRoute


-- COMMON

data MethodData =
    MethodData
        { mdId :: Maybe ShippingMethodId
        , mdDescription :: T.Text
        , mdCountries :: [Country]
        , mdRates :: [RateData]
        , mdPriorityFee :: Cents
        , mdPriorityRate :: Natural
        , mdPriorityEnabled :: Bool
        , mdCategories :: [CategoryId]
        , mdPriorityCategories :: [CategoryId]
        , mdPriority :: Natural
        , mdIsActive :: Bool
        } deriving (Show, Eq)

instance ToJSON MethodData where
    toJSON MethodData {..} =
        object
            [ "id" .= mdId
            , "description" .= mdDescription
            , "countries" .= mdCountries
            , "rates" .= mdRates
            , "priorityFee" .= mdPriorityFee
            , "priorityRate" .= mdPriorityRate
            , "isPriorityEnabled" .= mdPriorityEnabled
            , "categories" .= mdCategories
            , "priorityCategories" .= mdPriorityCategories
            , "priority" .= mdPriority
            , "isActive" .= mdIsActive
            ]

instance FromJSON MethodData where
    parseJSON = withObject "MethodData" $ \v -> do
        mdId <- v .: "id"
        mdDescription <- v .: "description"
        mdCountries <- v .: "countries"
        mdRates <- v .: "rates"
        mdPriorityFee <- v .: "priorityFee"
        mdPriorityRate <- v .: "priorityRate"
        mdPriorityEnabled <- v .: "isPriorityEnabled"
        mdCategories <- v .: "categories"
        mdPriorityCategories <- v .: "priorityCategories"
        mdPriority <- v .: "priority"
        mdIsActive <- v .: "isActive"
        return MethodData {..}

data RateData =
    RateData
        { rdThreshold :: Cents
        , rdAmount :: Natural
        , rdType :: RateType
        } deriving (Show, Eq)

instance ToJSON RateData where
    toJSON RateData {..} =
        object
            [ "threshold" .= rdThreshold
            , "amount" .= rdAmount
            , "type" .= rdType
            ]

instance FromJSON RateData where
    parseJSON = withObject "RateData" $ \v ->
        RateData
            <$> v .: "threshold"
            <*> v .: "amount"
            <*> v .: "type"

data RateType
    = FlatRate
    | PercentRate
    deriving (Show, Eq)

instance ToJSON RateType where
    toJSON = \case
        FlatRate -> "flat"
        PercentRate -> "percentage"

instance FromJSON RateType where
    parseJSON = withText "RateType" $ \case
        "flat" ->
            return FlatRate
        "percentage" ->
            return PercentRate
        str ->
            fail $ "Unknown rate type: " ++ T.unpack str


-- DATA

type ShippingDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ShippingData)

data ShippingData =
    ShippingData
        { sdMethods :: [MethodData]
        , sdAllCategories :: [AdminCategorySelect]
        } deriving (Show)

instance ToJSON ShippingData where
    toJSON ShippingData {..} =
        object
            [ "methods" .= sdMethods
            , "categories" .= sdAllCategories
            ]


shippingDataRoute :: WrappedAuthToken -> App (Cookied ShippingData)
shippingDataRoute = flip withAdminCookie $ \_ -> do
    (methods, categories) <- runDB $
        (,) <$> selectList [] [Asc ShippingMethodPriority]
            <*> makeAdminCategorySelects
    return $ ShippingData (map makeMethodData methods) categories
  where
    makeMethodData :: Entity ShippingMethod -> MethodData
    makeMethodData (Entity sId ShippingMethod {..}) =
        let
            PriorityShippingFee priorityFee priorityRate = shippingMethodPriorityRate
        in
        MethodData
            { mdId = Just sId
            , mdDescription = shippingMethodDescription
            , mdCountries = shippingMethodCountries
            , mdRates = map makeRateData shippingMethodRates
            , mdPriorityFee = priorityFee
            , mdPriorityRate = fromIntegral priorityRate
            , mdPriorityEnabled = shippingMethodIsPriorityEnabled
            , mdCategories = shippingMethodCategoryIds
            , mdPriorityCategories = shippingMethodExcludedPriorityCategoryIds
            , mdPriority = fromIntegral shippingMethodPriority
            , mdIsActive = shippingMethodIsActive
            }
    makeRateData :: ShippingRate -> RateData
    makeRateData = \case
        Flat t a ->
            RateData t (fromIntegral $ fromCents a) FlatRate
        Percentage t a ->
            RateData t (fromIntegral a) PercentRate


-- UPDATE


type ShippingUpdateRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] ShippingUpdateParameters
    :> Post '[JSON] (Cookied ())

newtype ShippingUpdateParameters =
    ShippingUpdateParameters
        { suParameters :: [MethodData]
        } deriving (Show)

instance FromJSON ShippingUpdateParameters where
    parseJSON v = ShippingUpdateParameters <$> parseJSON v

instance Validation ShippingUpdateParameters where
    validators ShippingUpdateParameters {..} =
        V.indexedValidation "method" validateMethod suParameters
      where
        validateMethod :: MethodData -> App [(T.Text, [(T.Text, Bool)])]
        validateMethod MethodData {..} = do
            idValidation <- case mdId of
                Nothing ->
                    return []
                Just methodId -> do
                    methodExists <- V.exists methodId
                    return
                        [ ( ""
                          , [ ( "Could not find this Shipping Method in the Database."
                              , methodExists
                              )
                            ]
                          )
                        ]
            categoryValidations <- validateCategorySelect False mdCategories
            priorityValidations <- validateCategorySelect False mdPriorityCategories
            return $ idValidation ++ categoryValidations ++ priorityValidations

shippingUpdateRoute :: WrappedAuthToken -> ShippingUpdateParameters -> App (Cookied ())
shippingUpdateRoute = validateAdminAndParameters $ \_ ShippingUpdateParameters {..} -> do
    let (toUpdate, toInsert) = L.partition (isJust . mdId) suParameters
        idsToUpdate = mapMaybe mdId toUpdate
    runDB $ do
        deleteWhere [ShippingMethodId /<-. idsToUpdate]
        insertMany_ $ map makeMethod toInsert
        forM_ toUpdate $ \method ->
            case mdId method of
                Nothing -> return ()
                Just mId ->
                    replace mId $ makeMethod method
  where
    makeMethod :: MethodData -> ShippingMethod
    makeMethod MethodData {..} =
        ShippingMethod
            { shippingMethodDescription = mdDescription
            , shippingMethodCountries = mdCountries
            , shippingMethodRates = map makeRate mdRates
            , shippingMethodPriorityRate = PriorityShippingFee mdPriorityFee (fromIntegral mdPriorityRate)
            , shippingMethodIsPriorityEnabled = mdPriorityEnabled
            , shippingMethodCategoryIds = mdCategories
            , shippingMethodExcludedPriorityCategoryIds = mdPriorityCategories
            , shippingMethodPriority = fromIntegral mdPriority
            , shippingMethodIsActive = mdIsActive
            }
    makeRate :: RateData -> ShippingRate
    makeRate RateData {..} = case rdType of
        FlatRate ->
            Flat rdThreshold $ Cents (fromIntegral rdAmount)
        PercentRate ->
            Percentage rdThreshold (fromIntegral rdAmount)
