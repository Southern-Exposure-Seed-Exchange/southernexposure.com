{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.CategorySales
    ( CategrySalesAPI
    , categorySalesRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Database.Persist (Entity(..), SelectOpt(Desc), selectList)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models
import Routes.CommonData (AdminCategorySelect, makeAdminCategorySelects)


type CategrySalesAPI =
         "list" :> CategorySalesListRoute

type CategorySalesRoutes =
         (WrappedAuthToken -> App (Cookied CategorySalesListData))

categorySalesRoutes :: CategorySalesRoutes
categorySalesRoutes =
         categorySalesListRoute


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
