{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.ProductSales
    ( ProductSalesAPI
    , productSalesRoutes
    ) where

import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Database.Persist (Entity(..), SelectOpt(Desc), selectList, insert)
import Database.Persist.Sql (fromSqlKey)
import Servant ((:<|>)(..), (:>), AuthProtect, Get, Post, JSON, ReqBody)

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (Cents, LotSize)
import Server (App, AppSQL, runDB)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E
import qualified Validation as V


type ProductSalesAPI =
         "list" :> ProductSalesListRoute
    :<|> "new" :> ProductSalesNewDataRoute
    :<|> "new" :> ProductSalesNewRoute


type ProductSalesRoutes =
         (WrappedAuthToken -> App (Cookied ProductSalesListData))
    :<|> (WrappedAuthToken -> App (Cookied ProductSalesNewData))
    :<|> (WrappedAuthToken -> ProductSalesNewParameters -> App (Cookied ProductSaleId))


productSalesRoutes :: ProductSalesRoutes
productSalesRoutes =
         productSalesListRoute
    :<|> productSalesNewDataRoute
    :<|> productSalesNewRoute


-- COMMON

data SaleVariantData =
    SaleVariantData
        { svdId :: ProductVariantId
        , svdName :: T.Text
        , svdSku :: T.Text
        , svdLotSize :: Maybe LotSize
        , svdActive :: Bool
        , svdPrice :: Cents
        } deriving (Show)

instance ToJSON SaleVariantData where
    toJSON SaleVariantData {..} =
        object
            [ "id" .= svdId
            , "name" .= svdName
            , "sku" .= svdSku
            , "lotSize" .= svdLotSize
            , "active" .= svdActive
            , "price" .= svdPrice
            ]

getProductsAndVariants :: AppSQL [(Entity ProductVariant, Entity Product)]
getProductsAndVariants =
    E.select $ E.from $ \(v `E.InnerJoin` p) -> do
        E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
        return (v, p)

toVariantData :: Entity Product -> Entity ProductVariant -> SaleVariantData
toVariantData (Entity _ p) (Entity vId v) =
    SaleVariantData
        { svdId = vId
        , svdName = productName p
        , svdSku = productBaseSku p <> productVariantSkuSuffix v
        , svdLotSize = productVariantLotSize v
        , svdActive = productVariantIsActive v
        , svdPrice = productVariantPrice v
        }


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


productSalesListRoute :: WrappedAuthToken -> App (Cookied ProductSalesListData)
productSalesListRoute = flip withAdminCookie $ \_ -> runDB $ do
    sales <- map ListProductSales <$> selectList [] [Desc ProductSaleEndDate]
    ProductSalesListData sales . makeVariants <$> getProductsAndVariants
  where
    makeVariants :: [(Entity ProductVariant, Entity Product)] -> M.Map ProductVariantId SaleVariantData
    makeVariants =
        foldr
            (\(e@(Entity vId _), p) acc ->
                M.insert vId (toVariantData p e) acc
            ) M.empty


-- NEW

type ProductSalesNewDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ProductSalesNewData)

newtype ProductSalesNewData =
    ProductSalesNewData
        { fromProductSalesNewData :: [SaleVariantData]
        } deriving (Show)

instance ToJSON ProductSalesNewData where
    toJSON ProductSalesNewData {..} =
        toJSON fromProductSalesNewData

productSalesNewDataRoute :: WrappedAuthToken -> App (Cookied ProductSalesNewData)
productSalesNewDataRoute = flip withAdminCookie $ \_ -> runDB $
    ProductSalesNewData . map (\(v, p) -> toVariantData p v) <$> getProductsAndVariants

type ProductSalesNewRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] ProductSalesNewParameters
    :> Post '[JSON] (Cookied ProductSaleId)

data ProductSalesNewParameters =
    ProductSalesNewParameters
        { psnpPrice :: Cents
        , psnpVariant :: ProductVariantId
        , psnpStart :: UTCTime
        , psnpEnd :: UTCTime
        } deriving (Show)

instance FromJSON ProductSalesNewParameters where
    parseJSON = withObject "ProductSalesNewParameters" $ \v -> do
        psnpPrice <- v .: "price"
        psnpVariant <- v .: "variant"
        psnpStart <- v .: "start"
        psnpEnd <- v .: "end"
        return ProductSalesNewParameters {..}

instance Validation ProductSalesNewParameters where
    validators ProductSalesNewParameters {..} = do
        variantExists <- V.exists psnpVariant
        return
            [ ( "variant"
              , [ ( "Could not find this Product in the database", variantExists )
                ]
              )
            ]

productSalesNewRoute :: WrappedAuthToken -> ProductSalesNewParameters -> App (Cookied ProductSaleId)
productSalesNewRoute = validateAdminAndParameters $ \_ ->
    runDB . insert . makeSale
  where
    makeSale :: ProductSalesNewParameters -> ProductSale
    makeSale ProductSalesNewParameters {..} =
        ProductSale
            { productSalePrice = psnpPrice
            , productSaleProductVariantId = psnpVariant
            , productSaleStartDate = psnpStart
            , productSaleEndDate = psnpEnd
            }
