{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Products
    ( ProductAPI
    , productRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Maybe (mapMaybe)
import Database.Persist (Entity(..), SelectOpt(Asc), selectList)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (WrappedAuthToken, Cookied, withAdminCookie)
import Models (EntityField(..), Product(..), ProductId, Category(..), CategoryId)
import Server (App, runDB)

import qualified Data.Text as T
import qualified Data.Map.Strict as M


type ProductAPI =
         "list" :> ProductListRoute

type ProductRoutes =
         (WrappedAuthToken -> App (Cookied ProductListData))

productRoutes :: ProductRoutes
productRoutes =
         productListRoute


-- LIST


type ProductListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ProductListData)

newtype ProductListData =
    ProductListData
        { pldProducts :: [ListProduct]
        } deriving (Show)

instance ToJSON ProductListData where
    toJSON ProductListData {..} =
        object
            [ "products" .= pldProducts
            ]

data ListProduct =
    ListProduct
        { lpId :: ProductId
        , lpName :: T.Text
        , lpBaseSku :: T.Text
        , lpCategories :: [T.Text]
        , lpIsActive :: Bool
        } deriving (Show)

instance ToJSON ListProduct where
    toJSON ListProduct {..} =
        object
            [ "id" .= lpId
            , "name" .= lpName
            , "baseSKU" .= lpBaseSku
            , "categories" .= lpCategories
            , "isActive" .= lpIsActive
            ]

productListRoute :: WrappedAuthToken -> App (Cookied ProductListData)
productListRoute t = withAdminCookie t $ \_ -> runDB $ do
    categoryNameMap <- makeNameMap <$> selectList [] []
    ProductListData . map (makeProduct categoryNameMap) <$> selectList [] [Asc ProductBaseSku]
  where
    makeNameMap :: [Entity Category] -> M.Map CategoryId T.Text
    makeNameMap =
        foldr (\(Entity cId c) m -> M.insert cId (categoryName c) m) M.empty
    makeProduct :: M.Map CategoryId T.Text -> Entity Product -> ListProduct
    makeProduct nameMap (Entity pId prod) =
        ListProduct
            { lpId = pId
            , lpName = productName prod
            , lpBaseSku = productBaseSku prod
            , lpCategories = mapMaybe (`M.lookup` nameMap) $ productCategoryIds prod
            , lpIsActive = productIsActive prod
            }
