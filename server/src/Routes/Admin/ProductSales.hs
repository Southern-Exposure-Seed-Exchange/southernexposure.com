{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.ProductSales
    ( ProductSalesAPI
    , productSalesRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Monoid ((<>))
import Database.Persist (Entity(..), SelectOpt(Desc), selectList)
import Database.Persist.Sql (fromSqlKey)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models
import Models.Fields (LotSize)
import Server (App, runDB)

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E


type ProductSalesAPI =
         "list" :> ProductSalesListRoute


type ProductSalesRoutes =
         (WrappedAuthToken -> App (Cookied ProductSalesListData))


productSalesRoutes :: ProductSalesRoutes
productSalesRoutes =
         productSalesListRoute


-- LIST

type ProductSalesListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ProductSalesListData)


data ProductSalesListData =
    ProductSalesListData
        { psldSales :: [ListProductSales]
        , psldVariants :: M.Map ProductVariantId SaleVariantData
        } deriving (Show)

instance ToJSON ProductSalesListData where
    toJSON ProductSalesListData {..} =
        object
            [ "sales" .= psldSales
            , "variants" .= M.mapKeys (show . fromSqlKey) psldVariants
            ]

newtype ListProductSales =
    ListProductSales
        { fromListProductSales :: Entity ProductSale
        } deriving (Show)

instance ToJSON ListProductSales where
    toJSON ListProductSales {..} =
        let (Entity saleId ProductSale {..}) = fromListProductSales in
        object
            [ "id" .= saleId
            , "price" .= productSalePrice
            , "variant" .= productSaleProductVariantId
            , "start" .= productSaleStartDate
            , "end" .= productSaleEndDate
            ]

data SaleVariantData =
    SaleVariantData
        { svdId :: ProductVariantId
        , svdName :: T.Text
        , svdSku :: T.Text
        , svdLotSize :: Maybe LotSize
        , svdActive :: Bool
        } deriving (Show)

instance ToJSON SaleVariantData where
    toJSON SaleVariantData {..} =
        object
            [ "id" .= svdId
            , "name" .= svdName
            , "sku" .= svdSku
            , "lotSize" .= svdLotSize
            , "active" .= svdActive
            ]


productSalesListRoute :: WrappedAuthToken -> App (Cookied ProductSalesListData)
productSalesListRoute = flip withAdminCookie $ \_ -> runDB $ do
    sales <- map ListProductSales <$> selectList [] [Desc ProductSaleEndDate]
    variants <- E.select $ E.from $ \(v `E.InnerJoin` p) -> do
        E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
        return (v, p)
    return $ ProductSalesListData sales $ makeVariants variants
  where
    makeVariants :: [(Entity ProductVariant, Entity Product)] -> M.Map ProductVariantId SaleVariantData
    makeVariants =
        foldr
            (\(e@(Entity vId _), p) acc ->
                M.insert vId (toVariantData p e) acc
            ) M.empty
    toVariantData :: Entity Product -> Entity ProductVariant -> SaleVariantData
    toVariantData (Entity _ p) (Entity vId v) =
        SaleVariantData
            { svdId = vId
            , svdName = productName p
            , svdSku = productBaseSku p <> productVariantSkuSuffix v
            , svdLotSize = productVariantLotSize v
            , svdActive = productVariantIsActive v
            }
