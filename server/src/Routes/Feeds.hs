{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Feeds
    ( FeedAPI
    , feedRoutes
    ) where

import Data.Monoid ((<>))
import Database.Persist ((==.), Entity(..), selectList)
import Servant ((:<|>)(..), (:>), Get)

import Sitemap
    ( Sitemap(..), SitemapUrl(..), ChangeFrequency(..), renderSitemap
    , SitemapIndex(..), IndexEntry(..), renderSitemapIndex
    )
import Models
import Routes.Utils (XML)
import Server (App, runDB)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T


type FeedAPI =
         "sitemap.xml" :> SitemapRoute
    :<|> "sitemap-index.xml" :> SitemapIndexRoute


type FeedRoutes =
         App LBS.ByteString
    :<|> App LBS.ByteString

feedRoutes :: FeedRoutes
feedRoutes =
         sitemapRoute
    :<|> sitemapIndexRoute


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
            , sitemapLastModified = Nothing
            , sitemapChangeFrequency = Just Monthly
            , sitemapPriority = Nothing
            }
    makeProductUrl :: Entity Product -> SitemapUrl
    makeProductUrl (Entity _ Product {..}) =
        SitemapUrl
            { sitemapLocation = "/products/" <> productSlug <> "/"
            , sitemapLastModified = Nothing
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
            , sitemapLastModified = Nothing
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
