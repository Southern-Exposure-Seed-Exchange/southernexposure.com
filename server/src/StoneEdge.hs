{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-| This module contains types and functions required for integrating with
the StoneEdge Order Manager.

Compared to the full Order Manager Developer Specification, this is a very
incomplete integration implementation. We've only implemented the bits that
we actually use.
-}
module StoneEdge
    ( -- * Errors
      renderSimpleSETIError
    , renderXmlSETIError
      -- * General Response Rendering
    , renderXmlSETIResponse
      -- * Authentication
    , StoneEdgeCredentials(..)
    , getStoneEdgeRequestCredentials
    , HasStoneEdgeCredentials(..)
      -- * Shared Types
    , StoneEdgeCents(..)
    , TypeOfDownload(..)
    , LastOrder(..)
    , LastDate(..)
      -- * Integration Endpoints
      -- ** Send Version
    , SendVersionRequest(..)
    , SendVersionResponse(..)
    , renderSendVersionResponse
      -- ** Order Count
    , OrderCountRequest(..)
    , OrderCountResponse(..)
    , renderOrderCountResponse
      -- ** Order Download
    , DownloadOrdersRequest(..)
    , DownloadOrdersResponse(..)
    , renderDownloadOrdersResponse
      -- *** Order Download Sub-Types
    , StoneEdgeOrder(..)
    , renderStoneEdgeOrder
    , StoneEdgeOrderBilling(..)
    , renderStoneEdgeOrderBilling
    , StoneEdgeOrderAddress(..)
    , renderStoneEdgeOrderAddress
    , StoneEdgeOrderShipping(..)
    , renderStoneEdgeOrderShipping
    , StoneEdgeOrderProduct(..)
    , renderStoneEdgeOrderProduct
    , StoneEdgeProductType(..)
    , renderStoneEdgeProductType
    , StoneEdgeOrderPayment(..)
    , renderStoneEdgeOrderPayment
    , StoneEdgePaymentCreditCard(..)
    , renderStoneEdgePaymentCreditCard
    , StoneEdgePaymentStoreCredit(..)
    , renderStoneEdgePaymentStoreCredit
    , StoneEdgeTotals(..)
    , renderStoneEdgeTotals
    , StoneEdgeDiscount(..)
    , renderStoneEdgeDiscount
    , StoneEdgeDiscountType(..)
    , renderStoneEdgeDiscountType
    , StoneEdgeTax(..)
    , renderStoneEdgeTax
    , StoneEdgeSurcharge(..)
    , renderStoneEdgeSurcharge
    , StoneEdgeShippingTotal(..)
    , renderStoneEdgeShippingTotal
    , StoneEdgeCoupon(..)
    , renderStoneEdgeCoupon
    , StoneEdgeOtherData(..)
    , renderStoneEdgeOtherData
    ) where

import Data.Scientific (Scientific, FPFormat(Fixed), formatScientific, scientific)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (LocalTime, Day, parseTimeM, defaultTimeLocale, formatTime)
import Text.Read (readMaybe)
import Text.XML.Generator
    ( Xml, Elem, doc, defaultDocInfo, xelem, xelems, xelemWithText, xempty
    , xrender
    )
import Web.FormUrlEncoded (FromForm(..), Form, parseUnique, parseMaybe)

import qualified Data.ByteString as BS
import qualified Data.Text as T


-- Errors

-- | Render a StoneEdge error in the simple plaintext format:
--
--  @SETIError: <error message>@
renderSimpleSETIError :: T.Text -> T.Text
renderSimpleSETIError message =
    "SETIError: " <> message

-- | Render a StoneEdge error in the XML format.
renderXmlSETIError :: TypeOfDownload -> T.Text -> BS.ByteString
renderXmlSETIError downloadType message =
    renderXmlSETIResponse downloadType 3 message xempty


-- Authentication

-- | Credentials passed by StoneEdge to authenticate requests.
data StoneEdgeCredentials
    = StoneEdgeCredentials
        { secUsername :: T.Text
        , secPassword :: T.Text
        , secStoreCode :: T.Text
        } deriving (Eq, Show, Read)

-- | Typeclass for pulling authentication credentials from Request types
-- that contain them.
class HasStoneEdgeCredentials a where
    -- | Get the @setiuser@ parameter
    userParam :: a -> T.Text
    -- | Get the @password@ parameter
    passwordParam :: a -> T.Text
    -- | Get the @code@ parameter
    storeCodeParam :: a -> T.Text

-- | Build a 'StoneEdgeCredentials' from a Request.
getStoneEdgeRequestCredentials
    :: HasStoneEdgeCredentials a
    => a -> StoneEdgeCredentials
getStoneEdgeRequestCredentials request =
    StoneEdgeCredentials
        { secUsername = userParam request
        , secPassword = passwordParam request
        , secStoreCode = storeCodeParam request
        }


-- Send Version

-- | Requests the version number of the integration script.
newtype SendVersionRequest
    = SendVersionRequest
        { orderManagerVersion :: T.Text
        } deriving (Eq, Show, Read)

instance FromForm SendVersionRequest where
    fromForm = matchFunction "sendversion" $
        fmap SendVersionRequest . parseUnique "omversion"

-- | Respond with the version of the integration script. Should be 4 digits
-- formatted as either @####@ or @#.###@.
newtype SendVersionResponse
    = SendVersionResponse
        { integrationScriptVersion :: T.Text
        } deriving (Eq, Show, Read)

renderSendVersionResponse :: SendVersionResponse -> T.Text
renderSendVersionResponse SendVersionResponse {..} =
    "SETIResponse: version=" <> integrationScriptVersion


-- Order Count

-- | Requests the number of new Orders that we have to send.
data OrderCountRequest
    = OrderCountRequest
        { ocrUser :: T.Text
        , ocrPass :: T.Text
        , ocrStoreCode :: T.Text
        , ocrLastOrder :: LastOrder
        , ocrLastDate :: LastDate
        , ocrOMVersion :: T.Text
        } deriving (Eq, Show, Read)

instance FromForm OrderCountRequest where
    fromForm = matchFunction "ordercount" $ \f ->
        OrderCountRequest
            <$> parseUnique "setiuser" f
            <*> parseUnique "password" f
            <*> parseUnique "code" f
            <*> fromForm f
            <*> fromForm f
            <*> parseUnique "omversion" f

instance HasStoneEdgeCredentials OrderCountRequest where
    userParam = ocrUser
    passwordParam = ocrPass
    storeCodeParam = ocrStoreCode

-- | Respond with a count of Orders that will be exported to StoneEdge.
newtype OrderCountResponse
    = OrderCountResponse
        { ocrOrderCount :: Integer
        } deriving (Eq, Show, Read)

renderOrderCountResponse :: OrderCountResponse -> T.Text
renderOrderCountResponse OrderCountResponse {..} =
    "SETIResponse: ordercount=" <> T.pack (show ocrOrderCount)


-- Order Download

-- | Requests Order Information to export into StoneEdge.
data DownloadOrdersRequest
    = DownloadOrdersRequest
        { dorUser :: T.Text
        , dorPass :: T.Text
        , dorStoreCode :: T.Text
        , dorLastOrder :: LastOrder
        , dorLastDate :: LastDate
        , dorStartNumber :: Maybe Integer
        , dorBatchSize :: Maybe Integer
        , dorDecryptionKey :: Maybe T.Text
        , dorOMVersion :: T.Text
        } deriving (Eq, Show, Read)

instance FromForm DownloadOrdersRequest where
    fromForm = matchFunction "downloadorders" $ \f ->
        DownloadOrdersRequest
            <$> parseUnique "setiuser" f
            <*> parseUnique "password" f
            <*> parseUnique "code" f
            <*> fromForm f
            <*> fromForm f
            <*> parseMaybe "startnum" f
            <*> parseMaybe "batchsize" f
            <*> parseMaybe "dkey" f
            <*> parseUnique "omversion" f

instance HasStoneEdgeCredentials DownloadOrdersRequest where
    userParam = dorUser
    passwordParam = dorPass
    storeCodeParam = dorStoreCode


-- | Respond with either no Orders to export to StoneEdge or a list of
-- Orders to export.
data DownloadOrdersResponse
    = NoOrdersToDownload
    | DownloadOrdersResponse [StoneEdgeOrder]
    deriving (Eq, Show, Read)

renderDownloadOrdersResponse :: DownloadOrdersResponse -> BS.ByteString
renderDownloadOrdersResponse = \case
    NoOrdersToDownload ->
        renderXmlSETIResponse Orders 2 "Success" xempty
    DownloadOrdersResponse orders ->
        renderXmlSETIResponse Orders 1 "Success"
            . xelems $ map renderStoneEdgeOrder orders

data StoneEdgeOrder
    = StoneEdgeOrder
        { seoOrderNumber :: Integer
        , seoOrderDate :: LocalTime
        , seoOrderStatus :: Maybe T.Text
        , seoBilling :: StoneEdgeOrderBilling
        , seoShipping :: StoneEdgeOrderShipping
        , seoPayment :: [StoneEdgeOrderPayment]
        , seoTotals :: StoneEdgeTotals
        , seoCoupon :: [StoneEdgeCoupon]
        , seoOtherData :: StoneEdgeOtherData
        } deriving (Eq, Show, Read)

renderStoneEdgeOrder :: StoneEdgeOrder -> Xml Elem
renderStoneEdgeOrder StoneEdgeOrder {..} =
    xelem "Order" $ xelems $ concat
        [ [ xelemWithText "OrderNumber" $ showText seoOrderNumber
          , xelemWithText "OrderDate" $ renderDate seoOrderDate
          , maybeXelemText "OrderStatus" seoOrderStatus
          , renderStoneEdgeOrderBilling seoBilling
          , renderStoneEdgeOrderShipping seoShipping
          ]
        , map renderStoneEdgeOrderPayment seoPayment
        , [ renderStoneEdgeTotals seoTotals ]
        , map renderStoneEdgeCoupon seoCoupon
        , [ renderStoneEdgeOtherData seoOtherData ]
        ]
  where
    renderDate =
        T.pack . formatTime defaultTimeLocale "%h/%d/%Y %T"

data StoneEdgeOrderBilling
    = StoneEdgeOrderBilling
        { seobFullName :: T.Text
        , seobCompany :: Maybe T.Text
        , seobPhone :: Maybe T.Text
        , seobEmail :: Maybe T.Text
        , seobAddress :: StoneEdgeOrderAddress
        } deriving (Eq, Show, Read)

renderStoneEdgeOrderBilling :: StoneEdgeOrderBilling -> Xml Elem
renderStoneEdgeOrderBilling StoneEdgeOrderBilling {..} =
    xelem "Billing" $ xelems
        [ xelemWithText "FullName" seobFullName
        , maybeXelemText "Company" seobCompany
        , maybeXelemText "Phone" seobPhone
        , maybeXelemText "Email" seobEmail
        , renderStoneEdgeOrderAddress seobAddress
        ]

data StoneEdgeOrderAddress
    = StoneEdgeOrderAddress
        { seoaStreetOne :: T.Text
        , seoaStreetTwo :: Maybe T.Text
        , seoaCity :: T.Text
        , seoaState :: T.Text
        -- ^ 2 Characters
        , seoaPostalCode :: T.Text
        , seoaCountry :: Maybe T.Text
        -- ^ 2 Characters
        } deriving (Eq, Show, Read)

renderStoneEdgeOrderAddress :: StoneEdgeOrderAddress -> Xml Elem
renderStoneEdgeOrderAddress StoneEdgeOrderAddress {..} =
    xelem "Address" $ xelems
        [ xelemWithText "Street1" seoaStreetOne
        , maybeXelemText "Street2" seoaStreetTwo
        , xelemWithText "City" seoaCity
        , xelemWithText "State" seoaState
        , xelemWithText "Code" seoaPostalCode
        , maybeXelemText "Country" seoaCountry
        ]

data StoneEdgeOrderShipping
    = StoneEdgeOrderShipping
        { seosFullName :: T.Text
        , seosCompany :: Maybe T.Text
        , seosPhone :: Maybe T.Text
        , seosEmail :: Maybe T.Text
        , seosAddress :: StoneEdgeOrderAddress
        , seosProduct :: [StoneEdgeOrderProduct]
        } deriving (Eq, Show, Read)

renderStoneEdgeOrderShipping :: StoneEdgeOrderShipping -> Xml Elem
renderStoneEdgeOrderShipping StoneEdgeOrderShipping {..} =
    xelem "Shipping" $ xelems $
        [ xelemWithText "FullName" seosFullName
        , maybeXelemText "Company" seosCompany
        , maybeXelemText "Phone" seosPhone
        , maybeXelemText "Email" seosEmail
        , renderStoneEdgeOrderAddress seosAddress
        ] ++ map renderStoneEdgeOrderProduct seosProduct

data StoneEdgeOrderProduct
    = StoneEdgeOrderProduct
        { seopSKU :: T.Text
        , seopName :: T.Text
        , seopQuantity :: Integer
        , seopItemPrice :: StoneEdgeCents
        , seopProductType :: Maybe StoneEdgeProductType
        , seopTaxable :: Maybe Bool
        , seopLineId :: Maybe Integer
        , seopTotal :: Maybe StoneEdgeCents
        } deriving (Eq, Show, Read)

renderStoneEdgeOrderProduct :: StoneEdgeOrderProduct -> Xml Elem
renderStoneEdgeOrderProduct StoneEdgeOrderProduct {..} =
    xelem "Product" $ xelems
        [ xelemWithText "SKU" seopSKU
        , xelemWithText "Name" seopName
        , xelemWithText "Quantity" $ showText seopQuantity
        , xelemWithText "ItemPrice" $ renderStoneEdgeCents seopItemPrice
        , maybeXelemTextWith "ProdType" renderStoneEdgeProductType
            seopProductType
        , maybeXelemTextWith "Taxable" renderBoolAsYesNo
            seopTaxable
        , maybeXelemTextWith "LineID" showText seopLineId
        , maybeXelemTextWith "Total" renderStoneEdgeCents seopTotal
        ]

data StoneEdgeProductType
    = TangibleProduct
    | DownloadProduct
    deriving (Eq, Show, Read)

-- | Render the *content* for the ProdType element.
renderStoneEdgeProductType :: StoneEdgeProductType -> T.Text
renderStoneEdgeProductType = \case
    TangibleProduct ->
        "Tangible"
    DownloadProduct ->
        "Download"

data StoneEdgeOrderPayment
    = StoneEdgeOrderCreditCard StoneEdgePaymentCreditCard
    | StoneEdgeOrderStoreCredit StoneEdgePaymentStoreCredit
    deriving (Eq, Show, Read)

renderStoneEdgeOrderPayment :: StoneEdgeOrderPayment -> Xml Elem
renderStoneEdgeOrderPayment = xelem "Payment" . \case
    StoneEdgeOrderCreditCard payment ->
        renderStoneEdgePaymentCreditCard payment
    StoneEdgeOrderStoreCredit payment ->
        renderStoneEdgePaymentStoreCredit payment

data StoneEdgePaymentCreditCard
    = StoneEdgePaymentCreditCard
        { sepccIssuer :: T.Text
        -- ^ Must match a "credit card" payment method name in StoneEdge
        , sepccCardNumber :: Maybe T.Text
        -- ^ Last 4 digits
        , sepccTransactionId :: Maybe T.Text
        -- ^ Required for payment to show up in Transactions table
        -- & Payment tab.
        , sepccAmount :: Maybe StoneEdgeCents
        } deriving (Eq, Show, Read)

renderStoneEdgePaymentCreditCard :: StoneEdgePaymentCreditCard -> Xml Elem
renderStoneEdgePaymentCreditCard StoneEdgePaymentCreditCard {..} =
    xelem "CreditCard" $ xelems
        [ xelemWithText "Issuer" sepccIssuer
        , maybeXelemText "Number" sepccCardNumber
        , maybeXelemText "TransID" sepccTransactionId
        , maybeXelemTextWith "Amount" renderStoneEdgeCents sepccAmount
        ]

data StoneEdgePaymentStoreCredit
    = StoneEdgePaymentStoreCredit
        { sepscTotal :: StoneEdgeCents
        , sepscDescription :: Maybe T.Text
        } deriving (Eq, Show, Read)

renderStoneEdgePaymentStoreCredit :: StoneEdgePaymentStoreCredit -> Xml Elem
renderStoneEdgePaymentStoreCredit StoneEdgePaymentStoreCredit {..} =
    xelem "StoreCredit" $ xelems
        [ xelemWithText "Total" $ renderStoneEdgeCents sepscTotal
        , maybeXelemText "Description" sepscDescription
        ]

data StoneEdgeTotals
    = StoneEdgeTotals
        { setProductTotal :: StoneEdgeCents
        , setDiscount :: [StoneEdgeDiscount]
        , setSubTotal :: StoneEdgeCents
        , setTax :: Maybe StoneEdgeTax
        , setGrandTotal :: StoneEdgeCents
        , setSurcharge :: [StoneEdgeSurcharge]
        , setShippingTotal :: Maybe StoneEdgeShippingTotal
        } deriving (Eq, Read, Show)

renderStoneEdgeTotals :: StoneEdgeTotals -> Xml Elem
renderStoneEdgeTotals StoneEdgeTotals {..} =
    xelem "Totals" $ xelems $ concat
        [ [ xelemWithText "ProductTotal" $ renderStoneEdgeCents setProductTotal ]
        , map renderStoneEdgeDiscount setDiscount
        , [ xelemWithText "SubTotal" $ renderStoneEdgeCents setSubTotal
          , maybe xempty renderStoneEdgeTax setTax
          , xelemWithText "GrandTotal" $ renderStoneEdgeCents setGrandTotal
          ]
        , map renderStoneEdgeSurcharge setSurcharge
        , [ maybe xempty renderStoneEdgeShippingTotal setShippingTotal ]
        ]

data StoneEdgeDiscount
    = StoneEdgeDiscount
        { sedType :: Maybe StoneEdgeDiscountType
        -- ^ Assumed to be 'SEFlatDiscount' if not present
        , sedDescription :: Maybe T.Text
        , sedPercent :: Maybe Scientific
        -- ^ Actual percentage(e.g., `0.05` for 5% discounts)
        , sedAmount :: StoneEdgeCents
        , sedAppliedPreTax :: Maybe Bool
        -- ^ Assumed to be 'True' if not present
        } deriving (Eq, Show, Read)

renderStoneEdgeDiscount :: StoneEdgeDiscount -> Xml Elem
renderStoneEdgeDiscount StoneEdgeDiscount {..} =
    xelem "Discount" $ xelems
        [ maybeXelemTextWith "Type" renderStoneEdgeDiscountType
            sedType
        , maybeXelemText "Description" sedDescription
        , maybeXelemTextWith "Percent" renderScientific
            sedPercent
        , xelemWithText "Amount" $ renderStoneEdgeCents sedAmount
        , maybeXelemTextWith "ApplyDiscount" renderBoolAppliedPreTax
            sedAppliedPreTax
        ]

data StoneEdgeDiscountType
    = SEFlatDiscount
    | SEPercentDiscount
    deriving (Eq, Show, Read)

renderStoneEdgeDiscountType :: StoneEdgeDiscountType -> T.Text
renderStoneEdgeDiscountType = \case
    SEFlatDiscount ->
        "Flat"
    SEPercentDiscount ->
        "Percent"

data StoneEdgeTax
    = StoneEdgeTax
        { setAmount :: StoneEdgeCents
        , setRate :: Maybe Scientific
        -- ^ Actual percentage(e.g., `0.05` for 5% tax)
        , setShippingTaxed :: Maybe Bool
        -- ^ Was shipping costs taxed? Assumes 'True' if not present
        , setTaxExempt :: Maybe Bool
        -- ^ Is billing party tax exempt? Assumes 'False' if not present
        , setTaxId :: Maybe T.Text
        -- ^ Required if setTaxExempt is True
        } deriving (Eq, Show, Read)

renderStoneEdgeTax :: StoneEdgeTax -> Xml Elem
renderStoneEdgeTax StoneEdgeTax {..} =
    xelem "Tax" $ xelems
        [ xelemWithText "TaxAmount" $ renderStoneEdgeCents setAmount
        , maybeXelemTextWith "TaxRate" renderScientific setRate
        , maybeXelemTextWith "TaxShipping" renderBoolAsYesNo
            setShippingTaxed
        , maybeXelemTextWith "TaxExempt" renderBoolAsYesNo
            setTaxExempt
        , maybeXelemText "TaxID" setTaxId
        ]

data StoneEdgeSurcharge
    = StoneEdgeSurcharge
        { sesTotal :: StoneEdgeCents
        , sesDescription :: Maybe T.Text
        } deriving (Eq, Show, Read)

renderStoneEdgeSurcharge :: StoneEdgeSurcharge -> Xml Elem
renderStoneEdgeSurcharge StoneEdgeSurcharge {..} =
    xelem "Surcharge" $ xelems
        [ xelemWithText "Total" $ renderStoneEdgeCents sesTotal
        , maybeXelemText "Description" sesDescription
        ]

data StoneEdgeShippingTotal
    = StoneEdgeShippingTotal
        { sestTotal :: StoneEdgeCents
        , sestDescription :: Maybe T.Text
        } deriving (Eq, Show, Read)

renderStoneEdgeShippingTotal :: StoneEdgeShippingTotal -> Xml Elem
renderStoneEdgeShippingTotal StoneEdgeShippingTotal {..} =
    xelem "ShippingTotal" $ xelems
        [ xelemWithText "Total" $ renderStoneEdgeCents sestTotal
        , maybeXelemText "Description" sestDescription
        ]

data StoneEdgeCoupon
    = StoneEdgeCoupon
        { secName :: T.Text
        , secStatus :: Maybe T.Text
        -- ^ Information about coupon
        , secTotal :: StoneEdgeCents
        , secAppliedPreTax :: Maybe Bool
        -- ^ Assumed to be True if not present
        } deriving (Eq, Show, Read)

renderStoneEdgeCoupon :: StoneEdgeCoupon -> Xml Elem
renderStoneEdgeCoupon StoneEdgeCoupon {..} =
    xelem "Coupon" $ xelems
        [ xelemWithText "Name" secName
        , maybeXelemText "Status" secStatus
        , xelemWithText "Total" $ renderStoneEdgeCents secTotal
        , maybeXelemTextWith "ApplyCoupon" renderBoolAppliedPreTax
            secAppliedPreTax
        ]

data StoneEdgeOtherData
    = StoneEdgeOtherData
        { seodOrderInstructions :: Maybe T.Text
        , seodComments :: Maybe T.Text
        , seodCustomerId :: Maybe Integer
        } deriving (Eq, Show, Read)

renderStoneEdgeOtherData :: StoneEdgeOtherData -> Xml Elem
renderStoneEdgeOtherData StoneEdgeOtherData {..} =
    xelem "Other" $ xelems
        [ maybeXelemText "OrderInstructions" seodOrderInstructions
        , maybeXelemText "Comments" seodComments
        , maybeXelemTextWith "CustomerID" showText seodCustomerId
        ]



-- Shared Types

-- | Track prices/totals as integer cents which will be rendered as
-- decimals when exported.
newtype StoneEdgeCents
    = StoneEdgeCents
        { secCents :: Integer
        } deriving (Eq, Show, Read)

-- | Render the cents as decimal dollars for XML rendering.
renderStoneEdgeCents :: StoneEdgeCents -> T.Text
renderStoneEdgeCents StoneEdgeCents {..} =
    renderScientific $ scientific secCents (-2)


-- | An enumeration of the exports StoneEdge will accept.
data TypeOfDownload
    = Orders
    | Products
    | Customers
    deriving (Eq, Show, Read)


-- | Identifies the last Order Number StoneEdge has imported.
data LastOrder
    = LastOrderNumber Integer
    | NoOrderNumber
    -- ^ Used when StoneEdge has no Orders. In this case, you should export
    -- all your Orders.
    deriving (Eq, Show, Read)

instance FromForm LastOrder where
    fromForm f = (T.toLower <$> parseUnique "lastorder" f) >>= \case
        "all" ->
            return NoOrderNumber
        orderStr ->
            case readMaybe (T.unpack orderStr) of
                Nothing ->
                    Left $ "Could not parse order number as integer: " <> orderStr
                Just i ->
                    return $ LastOrderNumber i


-- | Identifies the date of the last Order StoneEdge has imported.
data LastDate
    = LastDate Day
    | NoDate
    -- ^ Used when there StoneEdge has no Orders. In this case, you should
    -- export all your Orders.
    deriving (Eq, Show, Read)

instance FromForm LastDate where
    fromForm f = (T.toLower <$> parseUnique "lastdate" f) >>= \case
        "all" ->
            return NoDate
        dateStr ->
            fmap LastDate . runFormatError . parseTimeM True defaultTimeLocale "%e-%h-%Y"
                $ T.unpack dateStr


newtype FormatError a = MkFormatError { runFormatError :: Either T.Text a }
    deriving newtype (Functor, Applicative, Monad)

instance MonadFail FormatError where
    fail = MkFormatError . Left . T.pack

-- Utils

-- | Render a Bool as either "Yes" or "No".
renderBoolAsYesNo :: Bool -> T.Text
renderBoolAsYesNo b =
    if b then "Yes" else "No"

-- | Render a Bool as either "Pre" for 'True' or "Post" for 'False'
renderBoolAppliedPreTax :: Bool -> T.Text
renderBoolAppliedPreTax b =
    if b then "Pre" else "Post"

-- | Render 'Scientific' values as decimals with no limit on the decimal
-- places.
renderScientific :: Scientific -> T.Text
renderScientific = T.pack . formatScientific Fixed Nothing

-- | Render a 'Show'able type as Text.Read
showText :: Show a => a -> T.Text
showText = T.pack . show

-- | Render a potential Text as an XML Element if a Text value is present
-- & non-empty.
maybeXelemText :: T.Text -> Maybe T.Text -> Xml Elem
maybeXelemText tagName = \case
    Nothing ->
        xempty
    Just "" ->
        xempty
    Just v ->
        xelemWithText tagName v

-- | Render a potential value as an XML Element using a rendering function.
maybeXelemTextWith :: T.Text -> (a -> T.Text) -> Maybe a -> Xml Elem
maybeXelemTextWith tagName renderer =
    maybe xempty (xelemWithText tagName . renderer)


-- | Render an XML Response to StoneEdge, with a response code/description
-- & root element based on the 'TypeOfDownload'.
--
-- Note that StoneEdge's XML parser does not like the newlines that the
-- xml-gen package inserts into the rendered output, so we strip them out.
renderXmlSETIResponse :: TypeOfDownload -> Integer -> T.Text -> Xml Elem -> BS.ByteString
renderXmlSETIResponse downloadType responseCode responseDescription responseData =
    let rootElement = case downloadType of
            Orders ->
                "SETIOrders"
            Products ->
                "SETIProducts"
            Customers ->
                "SETICustomers"
    in filterNewlines . xrender $ doc defaultDocInfo $
        xelem rootElement $
            xelems
                [ xelem "Response" $
                    xelems
                        [ xelemWithText "ResponseCode" . T.pack $ show responseCode
                        , xelemWithText "ResponseDescription" responseDescription
                        ]
                , responseData
                ]
  where
    filterNewlines =
        encodeUtf8 . T.filter (/= '\n') . decodeUtf8

-- | Ensure the @setifunction@ parameter of the form matches the given
-- value. If so, run the parser on the 'Form'.
matchFunction :: T.Text -> (Form -> Either T.Text a) -> Form -> Either T.Text a
matchFunction value parser form =
    parseUnique "setifunction" form >>= \function ->
        if function == value then
            parser form
        else
            Left $ "Unsupported SETI function: " <> function
