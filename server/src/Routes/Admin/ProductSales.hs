{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.ProductSales
    ( ProductSalesAPI
    , productSalesRoutes
    ) where

import Control.Monad (unless)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson ((.=), (.:), ToJSON(..), FromJSON(..), object, withObject)
import Data.Maybe (catMaybes)
import Data.Time (LocalTime, TimeZone, getCurrentTimeZone, localTimeToUTC)
import Database.Persist
    ( Entity(..), SelectOpt(Desc), Update, selectList, insert, get, update
    )
import Database.Persist.Sql (fromSqlKey)
import Servant
    ( (:<|>)(..), (:>), AuthProtect, Capture, Get, Post, Patch, JSON, ReqBody
    , err404
    )

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Models
import Models.Fields (Cents, LotSize)
import Routes.Utils (mapUpdate, parseLocalTime, parseMaybeLocalTime)
import Server (App, AppSQL, runDB, serverError)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Database.Esqueleto as E
import qualified Validation as V


type ProductSalesAPI =
         "list" :> ProductSalesListRoute
    :<|> "new" :> ProductSalesNewDataRoute
    :<|> "new" :> ProductSalesNewRoute
    :<|> "edit" :> ProductSalesEditDataRoute
    :<|> "edit" :> ProductSaleEditRoute


type ProductSalesRoutes =
         (WrappedAuthToken -> App (Cookied ProductSalesListData))
    :<|> (WrappedAuthToken -> App (Cookied ProductSalesNewData))
    :<|> (WrappedAuthToken -> ProductSalesNewParameters -> App (Cookied ProductSaleId))
    :<|> (WrappedAuthToken -> ProductSaleId -> App (Cookied ProductSalesEditData))
    :<|> (WrappedAuthToken -> ProductSaleEditParameters -> App (Cookied ()))


productSalesRoutes :: ProductSalesRoutes
productSalesRoutes =
         productSalesListRoute
    :<|> productSalesNewDataRoute
    :<|> productSalesNewRoute
    :<|> productSalesEditDataRoute
    :<|> productSalesEditRoute


-- COMMON

newtype ProductSaleData =
    ProductSaleData
        { fromProductSaleData :: Entity ProductSale
        } deriving (Show)

instance ToJSON ProductSaleData where
    toJSON ProductSaleData {..} =
        let (Entity saleId ProductSale {..}) = fromProductSaleData in
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

toVariantData :: Entity ProductVariant -> Entity Product -> SaleVariantData
toVariantData (Entity vId v) (Entity _ p) =
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
        { psldSales :: [ProductSaleData]
        , psldVariants :: M.Map ProductVariantId SaleVariantData
        } deriving (Show)

instance ToJSON ProductSalesListData where
    toJSON ProductSalesListData {..} =
        object
            [ "sales" .= psldSales
            , "variants" .= M.mapKeys (show . fromSqlKey) psldVariants
            ]

productSalesListRoute :: WrappedAuthToken -> App (Cookied ProductSalesListData)
productSalesListRoute = flip withAdminCookie $ \_ -> runDB $ do
    sales <- map ProductSaleData <$> selectList [] [Desc ProductSaleEndDate]
    ProductSalesListData sales . makeVariants <$> getProductsAndVariants
  where
    makeVariants :: [(Entity ProductVariant, Entity Product)] -> M.Map ProductVariantId SaleVariantData
    makeVariants =
        foldr
            (\(e@(Entity vId _), p) acc ->
                M.insert vId (toVariantData e p) acc
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
    ProductSalesNewData . map (uncurry toVariantData) <$> getProductsAndVariants

type ProductSalesNewRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] ProductSalesNewParameters
    :> Post '[JSON] (Cookied ProductSaleId)

data ProductSalesNewParameters =
    ProductSalesNewParameters
        { psnpPrice :: Cents
        , psnpVariant :: ProductVariantId
        , psnpStart :: LocalTime
        , psnpEnd :: LocalTime
        } deriving (Show)

instance FromJSON ProductSalesNewParameters where
    parseJSON = withObject "ProductSalesNewParameters" $ \v -> do
        psnpPrice <- v .: "price"
        psnpVariant <- v .: "variant"
        psnpStart <- v .: "start" >>= parseLocalTime
        psnpEnd <- v .: "end" >>= parseLocalTime
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
productSalesNewRoute = validateAdminAndParameters $ \_ params -> do
    timeZone <- liftIO getCurrentTimeZone
    runDB . insert $ makeSale timeZone params
  where
    makeSale :: TimeZone -> ProductSalesNewParameters -> ProductSale
    makeSale timeZone ProductSalesNewParameters {..} =
        ProductSale
            { productSalePrice = psnpPrice
            , productSaleProductVariantId = psnpVariant
            , productSaleStartDate = localTimeToUTC timeZone psnpStart
            , productSaleEndDate = localTimeToUTC timeZone psnpEnd
            }


-- EDIT

type ProductSalesEditDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" ProductSaleId
    :> Get '[JSON] (Cookied ProductSalesEditData)

data ProductSalesEditData =
    ProductSalesEditData
        { psedSale :: ProductSaleData
        , psedVariants :: [SaleVariantData]
        } deriving (Show)

instance ToJSON ProductSalesEditData where
    toJSON ProductSalesEditData {..} =
        object
            [ "sale" .= psedSale
            , "variants" .= psedVariants
            ]

productSalesEditDataRoute :: WrappedAuthToken -> ProductSaleId -> App (Cookied ProductSalesEditData)
productSalesEditDataRoute t saleId = withAdminCookie t $ \_ -> runDB $
    get saleId >>= \case
        Nothing ->
            lift $ serverError err404
        Just productSale -> do
            variants <- map (uncurry toVariantData) <$> getProductsAndVariants
            return ProductSalesEditData
                { psedSale = ProductSaleData $ Entity saleId productSale
                , psedVariants = variants
                }


type ProductSaleEditRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] ProductSaleEditParameters
    :> Patch '[JSON] (Cookied ())

data ProductSaleEditParameters =
    ProductSaleEditParameters
        { psepId :: ProductSaleId
        , psepPrice :: Maybe Cents
        , psepVariant :: Maybe ProductVariantId
        , psepStart :: Maybe LocalTime
        , psepEnd :: Maybe LocalTime
        } deriving (Show)

instance FromJSON ProductSaleEditParameters where
    parseJSON = withObject "ProductSaleEditParameters" $ \v -> do
        psepId <- v .: "id"
        psepPrice <- v .: "price"
        psepVariant <- v .: "variant"
        psepStart <- v .: "start" >>= parseMaybeLocalTime
        psepEnd <- v .: "end" >>= parseMaybeLocalTime
        return ProductSaleEditParameters {..}

instance Validation ProductSaleEditParameters where
    validators ProductSaleEditParameters {..} = do
        exists <- V.exists psepId
        variantCheck <- case psepVariant of
            Nothing -> return []
            Just variantId -> do
                variantExists <- V.exists variantId
                return
                    [ ( "variant"
                      , [ ( "Could not find this Product in the database."
                          , variantExists
                          )
                        ]
                      )
                    ]
        return $
            ( ""
            , [ ( "Could not find this Product Sale in the database", exists )
              ]
            )
            : variantCheck

productSalesEditRoute :: WrappedAuthToken -> ProductSaleEditParameters -> App (Cookied ())
productSalesEditRoute = validateAdminAndParameters $ \_ params -> do
    timeZone <- liftIO getCurrentTimeZone
    let updates = makeUpdates params timeZone
    unless (null updates) $
        runDB $ update (psepId params) updates
  where
    makeUpdates :: ProductSaleEditParameters -> TimeZone -> [Update ProductSale]
    makeUpdates ProductSaleEditParameters {..} zone =
        let mapUpdateUTC f v = mapUpdate f $ localTimeToUTC zone <$> v
        in
        catMaybes
            [ mapUpdate ProductSalePrice psepPrice
            , mapUpdate ProductSaleProductVariantId psepVariant
            , mapUpdateUTC ProductSaleStartDate psepStart
            , mapUpdateUTC ProductSaleEndDate psepEnd
            ]
