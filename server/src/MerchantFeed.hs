{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module MerchantFeed
    ( renderFeed
    , Product(..)
    , PriceData(..)
    , Availability(..)
    , PricingMeasure(..)
    , ProductIdentifiers(..)
    , ProductDescription(..)
    , Condition(..)
    ) where

{-  This module provides types & render functions for generating a Google
Merchant Product Feed in RSS2.0 format:

https://support.google.com/merchants/answer/7052112

-}

import Data.Scientific (Scientific, FPFormat(..), formatScientific)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Text.XML.Generator
    ( (<#>), Xml, Elem, XmlOutput, Namespace, xrender, doc, defaultDocInfo, xelemQ
    , namespace, xelems, xelemWithText, xtext, xelem, xattr, xempty
    )

import qualified Data.Text as T


renderFeed :: XmlOutput x => [Product] -> x
renderFeed ps =
    xrender $ doc defaultDocInfo $
        xelem "rss" (xattr "version" "2.0" <#> channel)
  where
    channel =
        xelem "channel" $ xelems $
            [ xelemWithText "title" "Southern Exposure Seed Exchange"
            , xelemWithText "link" "https://www.southernexposure.com"
            , xelemWithText "description" "Southern Exposure Seed Exchange's Google Products Feed"
            ]
            ++ map renderProduct ps


gNS :: Namespace
gNS = namespace "g" "http://base.google.com/ns/1.0"

gElemWithText :: T.Text -> T.Text -> Xml Elem
gElemWithText name content =
    xelemQ gNS name $ xtext content


data Product =
    Product
        { pId :: T.Text
        , pTitle :: T.Text
        , pDescription :: T.Text
        , pLink :: T.Text
        , pImageLink :: T.Text
        , pProductType :: [T.Text]
        -- ^ Internal Category Hierarchy
        , pGoogleProductType :: Maybe T.Text
        -- ^ Google Category Code or Hierarchy
        , pPriceData :: PriceData
        , pIdentifiers :: ProductIdentifiers
        , pProductDescription :: ProductDescription
        } deriving (Show, Read, Eq, Generic)

renderProduct :: Product -> Xml Elem
renderProduct Product {..} =
    xelem "item" $ xelems $
        [ gElemWithText "id" pId
        , gElemWithText "title" pTitle
        , gElemWithText "description" pDescription
        , gElemWithText "link" pLink
        , gElemWithText "image_link" pImageLink
        , gElemWithText "product_type" $ T.intercalate " > " pProductType
        , maybe xempty (gElemWithText "google_product_category") pGoogleProductType
        ]
        ++ renderPriceData pPriceData
        ++ renderProductIdentifiers pIdentifiers
        ++ renderProductDescription pProductDescription


data PriceData =
    PriceData
        { pdAvailability :: Availability
        , pdPrice :: (Scientific, T.Text)
        -- ^ Price & Currency Code
        , pdSalePrice :: Maybe (Scientific, T.Text)
        -- ^ Sale Price & Currency Code
        , pdSaleDate :: Maybe (UTCTime, UTCTime)
        -- ^ Sale Start & End Dates
        , pdPricingMeasure :: Maybe PricingMeasure
        } deriving (Show, Read, Eq, Generic)

renderPriceData :: PriceData -> [Xml Elem]
renderPriceData PriceData {..} =
    [ gElemWithText "availability" $ renderAvailability pdAvailability
    , gElemWithText "price" $ renderPrice pdPrice
    , maybe xempty (gElemWithText "sale_price" . renderPrice) pdSalePrice
    , flip (maybe xempty) pdSaleDate $ \(start, end) ->
        gElemWithText "sale_price_effective_date"
            $ renderTime start <> " / " <> renderTime end
    , maybe xempty (gElemWithText "unit_pricing_measure" . renderPricingMeasure) pdPricingMeasure
    ]

renderPrice :: (Scientific, T.Text) -> T.Text
renderPrice (price, currency) =
    T.pack (formatScientific Fixed Nothing price) <> " " <> currency

renderTime :: UTCTime -> T.Text
renderTime =
    T.pack . formatTime defaultTimeLocale "%FT%T%z"


data Availability
    = InStock
    | OutOfStock
    | PreOrder
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

renderAvailability :: Availability -> T.Text
renderAvailability = \case
    InStock ->
        "in stock"
    OutOfStock ->
        "out of stock"
    PreOrder ->
        "preorder"

data PricingMeasure
    = Grams Scientific
    | Count Integer
    deriving (Show, Read, Eq, Generic)

renderPricingMeasure :: PricingMeasure -> T.Text
renderPricingMeasure = \case
    Grams g ->
        T.pack (formatScientific Fixed Nothing g) <> "g"
    Count i ->
        T.pack (show i) <> "ct"


data ProductIdentifiers =
    ProductIdentifiers
        { piBrand :: T.Text
        , piMPN :: T.Text
        } deriving (Show, Read, Eq, Generic)

renderProductIdentifiers :: ProductIdentifiers -> [Xml Elem]
renderProductIdentifiers ProductIdentifiers {..} =
    [ gElemWithText "brand" piBrand
    , gElemWithText "MPN" piMPN
    ]

data ProductDescription =
    ProductDescription
        { pdCondition :: Condition
        , pdAdult :: Bool
        , pdIsBundle :: Bool
        , pdItemGroupId :: Maybe T.Text
        } deriving (Show, Read, Eq, Generic)

renderProductDescription :: ProductDescription -> [Xml Elem]
renderProductDescription ProductDescription {..} =
    [ gElemWithText "condition" $ renderCondition pdCondition
    , gElemWithText "adult" $ renderBool pdAdult
    , gElemWithText "is_bundle" $ renderBool pdIsBundle
    , maybe xempty (gElemWithText "item_group_id") pdItemGroupId
    ]

data Condition
    = New
    | Refurbished
    | Used
    deriving (Show, Read, Eq, Bounded, Enum, Generic)

renderCondition :: Condition -> T.Text
renderCondition = \case
    New ->
        "new"
    Refurbished ->
        "refurbished"
    Used ->
        "used"

renderBool :: Bool -> T.Text
renderBool b =
    if b then
        "yes"
    else
        "no"
