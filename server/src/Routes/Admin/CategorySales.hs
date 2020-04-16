{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.CategorySales
    ( CategrySalesAPI
    , categorySalesRoutes
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Maybe (catMaybes)
import Data.Time (LocalTime, TimeZone, getCurrentTimeZone, localTimeToUTC)
import Database.Persist
    ( Entity(..), SelectOpt(Desc), Update, selectList, insert, get, update
    )
import Servant
    ( (:<|>)(..), (:>), AuthProtect, Capture, ReqBody, Get, Post, Patch, JSON
    , err404
    )

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (SaleType)
import Routes.CommonData
    ( AdminCategorySelect, makeAdminCategorySelects, validateCategorySelect
    )
import Routes.Utils (parseLocalTime, parseMaybeLocalTime, mapUpdate)
import Server (App, runDB, serverError)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Validation as V


type CategrySalesAPI =
         "list" :> CategorySalesListRoute
    :<|> "new" :> CategorySalesNewDataRoute
    :<|> "new" :> CategorySalesNewRoute
    :<|> "edit" :> CategorySalesEditDataRoute
    :<|> "edit" :> CategorySalesEditRoute

type CategorySalesRoutes =
         (WrappedAuthToken -> App (Cookied CategorySalesListData))
    :<|> (WrappedAuthToken -> App (Cookied CategorySalesNewData))
    :<|> (WrappedAuthToken -> CategorySalesNewParameters -> App (Cookied CategorySaleId))
    :<|> (WrappedAuthToken -> CategorySaleId -> App (Cookied CategorySalesEditData))
    :<|> (WrappedAuthToken -> CategorySalesEditParameters -> App (Cookied ()))

categorySalesRoutes :: CategorySalesRoutes
categorySalesRoutes =
         categorySalesListRoute
    :<|> categorySalesNewDataRoute
    :<|> categorySalesNewRoute
    :<|> categorySalesEditDataRoute
    :<|> categorySaleEditRoute


-- LIST

type CategorySalesListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CategorySalesListData)

data CategorySalesListData =
    CategorySalesListData
        { csldCategories :: [AdminCategorySelect]
        , csldSales :: [CategorySalesData]
        } deriving (Show)

instance ToJSON CategorySalesListData where
    toJSON CategorySalesListData {..} =
        object
            [ "categories" .= csldCategories
            , "sales" .= csldSales
            ]

newtype CategorySalesData =
    CategorySalesData
        { fromCategorySalesData :: Entity CategorySale
        } deriving (Show)

instance ToJSON CategorySalesData where
    toJSON CategorySalesData {..} =
        let (Entity saleId CategorySale {..}) = fromCategorySalesData
        in
        object
            [ "id" .= saleId
            , "name" .= categorySaleName
            , "type" .= categorySaleType
            , "start" .= categorySaleStartDate
            , "end" .= categorySaleEndDate
            , "categories" .= categorySaleCategoryIds
            ]

categorySalesListRoute :: WrappedAuthToken -> App (Cookied CategorySalesListData)
categorySalesListRoute = flip withAdminCookie $ \_ -> do
    (categories, sales) <- runDB $
        (,) <$> makeAdminCategorySelects
            <*> selectList [] [Desc CategorySaleEndDate]
    return CategorySalesListData
        { csldCategories = categories
        , csldSales = map CategorySalesData sales
        }


-- NEW

type CategorySalesNewDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CategorySalesNewData)

newtype CategorySalesNewData =
    CategorySalesNewData
        { csndCategories :: [AdminCategorySelect]
        } deriving (Show)

instance ToJSON CategorySalesNewData where
    toJSON CategorySalesNewData {..} =
        object [ "categories" .= csndCategories ]

categorySalesNewDataRoute :: WrappedAuthToken -> App (Cookied CategorySalesNewData)
categorySalesNewDataRoute = flip withAdminCookie $ \_ ->
    CategorySalesNewData <$> runDB makeAdminCategorySelects


type CategorySalesNewRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CategorySalesNewParameters
    :> Post '[JSON] (Cookied CategorySaleId)

data CategorySalesNewParameters =
    CategorySalesNewParameters
        { csnpName :: T.Text
        , csnpType :: SaleType
        , csnpStart :: LocalTime
        , csnpEnd :: LocalTime
        , csnpCategories :: [CategoryId]
        } deriving (Show)

instance FromJSON CategorySalesNewParameters where
    parseJSON = withObject "CategorySalesNewParameters" $ \v -> do
        csnpName <- v .: "name"
        csnpType <- v .: "type"
        csnpStart <- v .: "start" >>= parseLocalTime
        csnpEnd <- v .: "end" >>= parseLocalTime
        csnpCategories <- v .: "categories"
        return CategorySalesNewParameters {..}

instance Validation CategorySalesNewParameters where
    validators CategorySalesNewParameters {..} =
        validateCategorySelect True csnpCategories

categorySalesNewRoute :: WrappedAuthToken -> CategorySalesNewParameters -> App (Cookied CategorySaleId)
categorySalesNewRoute = validateAdminAndParameters $ \_ CategorySalesNewParameters {..} -> do
    timeZone <- liftIO getCurrentTimeZone
    runDB $ insert CategorySale
        { categorySaleName = csnpName
        , categorySaleType = csnpType
        , categorySaleStartDate = localTimeToUTC timeZone csnpStart
        , categorySaleEndDate = localTimeToUTC timeZone csnpEnd
        , categorySaleCategoryIds = csnpCategories
        }


-- EDIT

type CategorySalesEditDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" CategorySaleId
    :> Get '[JSON] (Cookied CategorySalesEditData)

data CategorySalesEditData =
    CategorySalesEditData
        { csedSale :: CategorySalesData
        , csedCategories :: [AdminCategorySelect]
        } deriving (Show)

instance ToJSON CategorySalesEditData where
    toJSON CategorySalesEditData {..} =
        object
            [ "sale" .= csedSale
            , "categories" .= csedCategories
            ]

categorySalesEditDataRoute :: WrappedAuthToken -> CategorySaleId -> App (Cookied CategorySalesEditData)
categorySalesEditDataRoute t saleId = withAdminCookie t $ \_ -> runDB $
    get saleId >>= \case
        Nothing ->
            lift $ serverError err404
        Just sale ->
            CategorySalesEditData (CategorySalesData $ Entity saleId sale)
                <$> makeAdminCategorySelects


type CategorySalesEditRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CategorySalesEditParameters
    :> Patch '[JSON] (Cookied ())

data CategorySalesEditParameters =
    CategorySalesEditParameters
        { csepId :: CategorySaleId
        , csepName :: Maybe T.Text
        , csepType :: Maybe SaleType
        , csepStart :: Maybe LocalTime
        , csepEnd :: Maybe LocalTime
        , csepCategories :: Maybe [CategoryId]
        }

instance FromJSON CategorySalesEditParameters where
    parseJSON = withObject "CategorySalesEditParameters" $ \v -> do
        csepId <- v .: "id"
        csepName <- v .: "name"
        csepType <- v .: "type"
        csepStart <- v .: "start" >>= parseMaybeLocalTime
        csepEnd <- v .: "end" >>= parseMaybeLocalTime
        csepCategories <- v .: "categories"
        return CategorySalesEditParameters {..}

instance Validation CategorySalesEditParameters where
    validators CategorySalesEditParameters {..} = do
        categories <- maybe (return []) (validateCategorySelect True) csepCategories
        saleExists <- V.exists csepId
        return $
            ( "", [("Could not find the Sale in the database.", saleExists)] )
            : categories

categorySaleEditRoute :: WrappedAuthToken -> CategorySalesEditParameters -> App (Cookied ())
categorySaleEditRoute = validateAdminAndParameters $ \_ parameters -> do
    timezone <- liftIO getCurrentTimeZone
    let updates = makeUpdates timezone parameters
    unless (null updates) $
        runDB $ update (csepId parameters) updates
  where
    makeUpdates :: TimeZone -> CategorySalesEditParameters -> [Update CategorySale]
    makeUpdates zone CategorySalesEditParameters {..} =
        let mapUpdateUTC f v = mapUpdate f $ localTimeToUTC zone <$> v
        in
        catMaybes
            [ mapUpdate CategorySaleName csepName
            , mapUpdate CategorySaleType csepType
            , mapUpdateUTC CategorySaleStartDate csepStart
            , mapUpdateUTC CategorySaleEndDate csepEnd
            , mapUpdate CategorySaleCategoryIds csepCategories
            ]
