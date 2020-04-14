{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.CategorySales
    ( CategrySalesAPI
    , categorySalesRoutes
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Time (LocalTime, getCurrentTimeZone, localTimeToUTC)
import Database.Persist (Entity(..), SelectOpt(Desc), selectList, insert)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (SaleType)
import Routes.CommonData (AdminCategorySelect, makeAdminCategorySelects, validateCategorySelect)
import Routes.Utils (parseLocalTime)
import Server (App, runDB)
import Validation (Validation(..))

import qualified Data.Text as T

type CategrySalesAPI =
         "list" :> CategorySalesListRoute
    :<|> "new" :> CategorySalesNewDataRoute
    :<|> "new" :> CategorySalesNewRoute

type CategorySalesRoutes =
         (WrappedAuthToken -> App (Cookied CategorySalesListData))
    :<|> (WrappedAuthToken -> App (Cookied CategorySalesNewData))
    :<|> (WrappedAuthToken -> CategorySalesNewParameters -> App (Cookied CategorySaleId))

categorySalesRoutes :: CategorySalesRoutes
categorySalesRoutes =
         categorySalesListRoute
    :<|> categorySalesNewDataRoute
    :<|> categorySalesNewRoute


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
