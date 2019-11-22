{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Sitemap
    ( -- * Sitemaps
      Sitemap(..)
    , renderSitemap
    , SitemapUrl(..)
    , renderSitemapUrl
    , ChangeFrequency(..)
    , renderChangeFrequency
      -- * Sitemap Indexes
    , SitemapIndex(..)
    , renderSitemapIndex
    , IndexEntry(..)
    , renderIndexEntry
      -- * Utilities
    , sitemapNamespace
    , formatSitemapTime
    , renderLastModified
    ) where

{-| This module contains types & XML rendering functions based on the
sitemaps.org Sitemap specification: https://www.sitemaps.org/protocol.html

-}

import Data.Maybe (catMaybes)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Text.XML.Generator
    ( Xml, Elem, XmlOutput, Namespace, xrender, doc, defaultDocInfo, xelemQ
    , namespace, xelems, xelemWithText, xtext, xelem
    )

import qualified Data.Text as T


-- SITEMAPS


newtype Sitemap =
    Sitemap
        { sitemapUrls :: [SitemapUrl]
        } deriving (Show, Read, Eq, Generic)

renderSitemap :: XmlOutput x => Sitemap -> x
renderSitemap sitemap =
    xrender $ doc defaultDocInfo $
        xelemQ sitemapNamespace "urlset" $
            xelems $ map renderSitemapUrl $ sitemapUrls sitemap

data SitemapUrl =
    SitemapUrl
        { sitemapLocation :: T.Text
        , sitemapLastModified :: Maybe UTCTime
        , sitemapChangeFrequency :: Maybe ChangeFrequency
        , sitemapPriority :: Maybe Double
        } deriving (Show, Read, Eq, Generic)

renderSitemapUrl :: SitemapUrl -> Xml Elem
renderSitemapUrl url =
    xelem "url" $ xelems $ catMaybes
        [ Just $ xelemWithText "loc" $ sitemapLocation url
        , renderLastModified <$> sitemapLastModified url
        , xelem "changefreq" . renderChangeFrequency <$> sitemapChangeFrequency url
        , xelemWithText "priority" . T.pack . show <$> sitemapPriority url
        ]

data ChangeFrequency
    = Always
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
    | Never
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

renderChangeFrequency :: ChangeFrequency -> Xml Elem
renderChangeFrequency = xtext . \case
    Always ->
        "always"
    Hourly ->
        "hourly"
    Daily ->
        "daily"
    Weekly ->
        "weekly"
    Monthly ->
        "monthly"
    Yearly ->
        "yearly"
    Never ->
        "never"


-- INDEXES


newtype SitemapIndex =
    SitemapIndex
        { indexEntries :: [IndexEntry]
        } deriving (Show, Read, Eq, Generic)

renderSitemapIndex :: XmlOutput x => SitemapIndex -> x
renderSitemapIndex index =
    xrender $ doc defaultDocInfo $
        xelemQ sitemapNamespace "sitemapindex" $
            xelems $ map renderIndexEntry $ indexEntries index

data IndexEntry =
    IndexEntry
        { indexLocation :: T.Text
        , indexLastModified :: Maybe UTCTime
        } deriving (Show, Read, Eq, Generic)

renderIndexEntry :: IndexEntry -> Xml Elem
renderIndexEntry entry =
    xelem "sitemap" $ xelems $ catMaybes
        [ Just $ xelemWithText "loc" $ indexLocation entry
        , renderLastModified <$> indexLastModified entry
        ]

sitemapNamespace :: Namespace
sitemapNamespace =
    namespace "" "http://www.sitemaps.org/schemas/sitemap/0.9"

-- | Render the 'UTCTime' in @YYYY-MM-DDTHH:MM:SS+00:00@ format.
formatSitemapTime :: UTCTime -> T.Text
formatSitemapTime =
    T.pack . formatTime defaultTimeLocale "%FT%T+00:00"

renderLastModified :: UTCTime -> Xml Elem
renderLastModified =
    xelemWithText "lastmod" . formatSitemapTime
