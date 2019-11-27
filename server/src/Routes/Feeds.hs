{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Feeds
    ( FeedAPI
    , feedRoutes
    ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader (asks, liftIO)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Scientific (Scientific, scientific)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist ((==.), (>=.), (<=.), Entity(..), selectList)
import Servant ((:<|>)(..), (:>), Get)

import Cache (Caches(..), CategoryPredecessorCache, queryCategoryPredecessorCache)
import Config (Config(getCaches))
import Sitemap
    ( Sitemap(..), SitemapUrl(..), ChangeFrequency(..), renderSitemap
    , SitemapIndex(..), IndexEntry(..), renderSitemapIndex
    )
import Models
import Models.Fields (Milligrams(..), LotSize(..), renderLotSize, toDollars)
import Routes.Utils (XML)
import Server (App, runDB)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified MerchantFeed as GMerch


type FeedAPI =
         "sitemap.xml" :> SitemapRoute
    :<|> "sitemap-index.xml" :> SitemapIndexRoute
    :<|> "merchant-feed.xml" :> MerchantFeedRoute


type FeedRoutes =
         App LBS.ByteString
    :<|> App LBS.ByteString
    :<|> App LBS.ByteString

feedRoutes :: FeedRoutes
feedRoutes =
         sitemapRoute
    :<|> sitemapIndexRoute
    :<|> merchantFeedRoute


-- SITEMAPS

type SitemapRoute =
       Get '[XML] LBS.ByteString

sitemapRoute :: App LBS.ByteString
sitemapRoute = do
    (categories, products, pages) <- runDB $
        (,,)
            <$> selectList [] []
            <*> selectList [ProductIsActive ==. True] []
            <*> selectList [] []
    let staticPages =
            [ "all-products"
            , "organic"
            , "heirloom"
            , "south-east"
            , "small-grower"
            , "search/advanced"
            , "account/login"
            , "account/create"
            , "account/reset-password"
            , "quick-order"
            , "cart"
            ]
    return $ renderSitemap $ Sitemap $
        map makeCategoryUrl categories
            <> map makeProductUrl products
            <> map makePageUrl pages
            <> map makeUrl staticPages
  where
    makeCategoryUrl :: Entity Category -> SitemapUrl
    makeCategoryUrl (Entity _ Category {..}) =
        SitemapUrl
            { sitemapLocation = "/categories/" <> categorySlug <> "/"
            , sitemapLastModified = Just categoryUpdatedAt
            , sitemapChangeFrequency = Just Monthly
            , sitemapPriority = Nothing
            }
    makeProductUrl :: Entity Product -> SitemapUrl
    makeProductUrl (Entity _ Product {..}) =
        SitemapUrl
            { sitemapLocation = "/products/" <> productSlug <> "/"
            , sitemapLastModified = Just productUpdatedAt
            , sitemapChangeFrequency = Just Monthly
            , sitemapPriority = Just 0.75
            }
    makePageUrl :: Entity Page -> SitemapUrl
    makePageUrl (Entity _ Page {..}) =
        SitemapUrl
            { sitemapLocation =
                if pageSlug == "home" then
                    "/"
                else
                    "/" <> pageSlug <> "/"
            , sitemapLastModified = Just pageUpdatedAt
            , sitemapChangeFrequency = Just Monthly
            , sitemapPriority = Just 0.7
            }
    makeUrl :: T.Text -> SitemapUrl
    makeUrl url =
        SitemapUrl
            { sitemapLocation = "/" <> url <> "/"
            , sitemapLastModified = Nothing
            , sitemapChangeFrequency = Just Monthly
            , sitemapPriority = Just 0.55
            }


type SitemapIndexRoute =
          Get '[XML] LBS.ByteString

sitemapIndexRoute :: Monad m => m LBS.ByteString
sitemapIndexRoute =
    return $ renderSitemapIndex $ SitemapIndex
        [ IndexEntry { indexLocation = "/sitemap.xml", indexLastModified = Nothing }
        , IndexEntry { indexLocation = "/blog/sitemap.xml.gz", indexLastModified = Nothing }
        ]


-- GOOGLE MERCHANT FEED


type MerchantFeedRoute =
       Get '[XML] LBS.ByteString

merchantFeedRoute :: App LBS.ByteString
merchantFeedRoute = do
    currentTime <- liftIO getCurrentTime
    (variantSales, categorySales, products, categories) <- runDB $ do
        vs <- map entityVal <$> selectList
            [ ProductSaleStartDate <=. currentTime
            , ProductSaleEndDate >=. currentTime
            ]
            []
        catSale <- map entityVal <$> selectList
            [ CategorySaleStartDate <=. currentTime
            , CategorySaleEndDate >=. currentTime
            ]
            []
        ps <- E.select $ E.from $ \(p `E.InnerJoin` v) -> do
            E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
            E.where_ $ p E.^. ProductIsActive E.||. v E.^. ProductVariantIsActive
            return (p, v)
        cs <- selectList [] []
        return (vs, catSale, ps, cs)
    let categoryMap = foldr (\(Entity cId c) m -> M.insert cId c m) M.empty categories
    categoryCache <- asks getCaches >>= fmap getCategoryPredecessorCache . liftIO . readTVarIO
    let convert = convertProduct categoryCache categoryMap variantSales categorySales
    return $ GMerch.renderFeed $ map convert products

convertProduct
    :: CategoryPredecessorCache
    -> M.Map CategoryId Category
    -> [ProductSale]
    -> [CategorySale]
    -> (Entity Product, Entity ProductVariant)
    -> GMerch.Product
convertProduct predCache categoryMap prodSales catSales (Entity _ prod, Entity variantId variant) =
    let
        fullSku =
            productBaseSku prod <> productVariantSkuSuffix variant
        lotSize =
            maybe "" (\ls -> ", " <> renderLotSize ls)
                $ productVariantLotSize variant
        baseUrl =
            "https://www.southernexposure.com"
        productUrl =
            baseUrl <> "/products/"
        imageUrl =
            baseUrl <> "/media/products/originals/"
        categoryHierarchy =
            maybe [] makeHierarchy $ listToMaybe (productCategoryIds prod)
        measure =
            productVariantLotSize variant >>= lotSizeToMeasurement
        googleCategory
            | hierarchyContains "Supplies" =
                "3173"
            | hierarchyContains "Books" =
                "783"
            | otherwise =
                "2802"
        hierarchyContains search =
            any (search `T.isInfixOf`) categoryHierarchy
    in
    GMerch.Product
        { pId = fullSku
        , pTitle = productName prod <> lotSize
        , pDescription = productLongDescription prod
        , pLink = productUrl <> productSlug prod <> "/"
        , pImageLink = imageUrl <> productImageUrl prod
        , pProductType = categoryHierarchy
        , pGoogleProductType = Just googleCategory
        , pPriceData =
            GMerch.PriceData
                { pdAvailability =
                    if productVariantQuantity variant > 0 then
                        GMerch.InStock
                    else
                        GMerch.OutOfStock
                , pdPrice =
                    (toDollars $ productVariantPrice variant, "USD")
                , pdPricingMeasure = measure
                , pdSalePrice = fst getSale
                , pdSaleDate = snd getSale
                }
        , pIdentifiers =
            GMerch.ProductIdentifiers
                { piBrand = "Southern Exposure Seed Exchange"
                , piMPN = fullSku
                }
        , pProductDescription =
            GMerch.ProductDescription
                { pdCondition = GMerch.New
                , pdAdult = False
                , pdIsBundle = any ("Mixes" `T.isInfixOf`) categoryHierarchy
                , pdItemGroupId = Just $ productBaseSku prod
                }
        }
  where
    -- TODO: Eventually move some of this into CommonData to reduce query
    -- count during product price calculations(Issue #1570).
    getSale :: (Maybe (Scientific, T.Text), Maybe (UTCTime, UTCTime))
    getSale =
        let
            variantSale =
                listToMaybe
                    $ filter
                        ((== variantId) . productSaleProductVariantId)
                        prodSales
            categoryIds =
                concatMap (\cId -> map entityKey $ queryCategoryPredecessorCache cId predCache)
                    $ productCategoryIds prod
            categorySale =
                listToMaybe
                    $ filter (any (`elem` categoryIds) . categorySaleCategoryIds)
                        catSales
            price =
                productVariantPrice variant
            makeVariantSale vSale =
                ( Just
                    ( toDollars $ productSalePrice vSale
                    , "USD"
                    )
                , Just
                    ( productSaleStartDate vSale
                    , productSaleEndDate vSale
                    )
                )
            makeCategorySale cSale =
                ( Just
                    ( toDollars $ categorySalePrice price cSale
                    , "USD"
                    )
                , Just
                    ( categorySaleStartDate cSale
                    , categorySaleEndDate cSale
                    )
                )
        in
        case (variantSale, categorySale) of
            (Just vSale, Just cSale) ->
                if productSalePrice vSale < categorySalePrice price cSale then
                    makeVariantSale vSale
                else
                    makeCategorySale cSale
            (Just vSale, Nothing) ->
                makeVariantSale vSale
            (_, Just cSale) ->
                makeCategorySale cSale
            _ ->
                (Nothing, Nothing)
    makeHierarchy :: CategoryId -> [T.Text]
    makeHierarchy cId =
        let
            name = maybe "" categoryName $ M.lookup cId categoryMap
            parents = queryCategoryPredecessorCache cId predCache
            parentNames = map (categoryName . entityVal) parents
        in
        reverse $ name : parentNames


lotSizeToMeasurement :: LotSize -> Maybe GMerch.PricingMeasure
lotSizeToMeasurement = \case
        CustomLotSize _ ->
            Nothing
        Mass (Milligrams mg) ->
            Just $ GMerch.Grams (scientific (fromIntegral mg) (-3))
        Bulbs ct ->
            Just $ GMerch.Count ct
        Slips ct ->
            Just $ GMerch.Count ct
        Plugs ct ->
            Just $ GMerch.Count ct
