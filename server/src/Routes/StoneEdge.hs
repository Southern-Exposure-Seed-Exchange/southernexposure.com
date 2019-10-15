{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Routes.StoneEdge
    (
    -- * API
      StoneEdgeAPI
    , stoneEdgeRoutes
    , StoneEdgeRequest(..)
    -- * Test Exports
    , transformCoupon
    , transformStoreCredit
    , transformTax
    , transformDiscount
    , transformSurcharge
    , transformShippingTotal
    ) where

import Control.Monad ((<=<), when, forM)
import Control.Monad.Reader (asks, liftIO)
import Control.Exception.Safe (MonadCatch, Exception, Typeable, throwM, handle)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (LocalTime, getTimeZone, utcToLocalTime)
import Database.Persist.Sql
    ( (>.), (<-.), (==.), Entity(..), count, toSqlKey, fromSqlKey, selectList
    )
import Text.Read (readMaybe)
import Servant ((:>), ReqBody, FormUrlEncoded, Post, PlainText, MimeRender(..))
import Web.FormUrlEncoded (FromForm(..), Form, parseMaybe)

import Config
import Models
import Models.Fields
    ( Cents(..), Country(..), Region(..), OrderStatus(..), LineItemType(..)
    , StripeChargeId(..), renderLotSize
    )
import Server
import StoneEdge

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Web.Stripe.Types as Stripe


-- TODO: PlainText return type is OK? Or Custom StoneEdge one?
-- Need to add Content-Type request header?
-- Determine this when testing against test-version of StoneEdge
type StoneEdgeAPI =
    ReqBody '[FormUrlEncoded] StoneEdgeRequest :> Post '[PlainText] StoneEdgeResponse


-- | The integration script version we pass to StoneEdge.
integrationVersion :: Text
integrationVersion = "1.000"


-- Request Dispatch & Response Rendering

data StoneEdgeIntegrationError
    = StoneEdgeAuthError
    -- ^ Request authentication does not match values from the 'Config'.
    deriving (Show, Typeable)

instance Exception StoneEdgeIntegrationError

-- | Call the proper sub-route depending on the request that was decoded by
-- the request body.
stoneEdgeRoutes :: StoneEdgeRequest -> App StoneEdgeResponse
stoneEdgeRoutes = \case
    SVRq rq ->
        SVRs <$> sendVersionRoute rq
    OCRq rq ->
        handle simpleError $ checkAuth rq >> OCRs <$> orderCountRoute rq
    DORq rq ->
        handle (xmlError Orders) $ checkAuth rq >> DORs <$> downloadOrdersRoute rq
    UnexpectedFunction function _ ->
        return . ErrorResponse $ "Integration has no support for function: " <> function
    InvalidRequest form ->
        return . ErrorResponse $ "Expecting a setifunction parameter, received: " <> T.pack (show form)
  where
    xmlError :: MonadCatch m => TypeOfDownload -> StoneEdgeIntegrationError -> m StoneEdgeResponse
    xmlError type_ = \case
        StoneEdgeAuthError ->
            return $ XmlErrorResponse type_ "Invalid username, password, or store code."
    simpleError :: MonadCatch m => StoneEdgeIntegrationError -> m StoneEdgeResponse
    simpleError = \case
        StoneEdgeAuthError ->
            return $ ErrorResponse "Invalid username, password, or store code."


-- | Joins all the StoneEdge Integraton Function Request Types.
data StoneEdgeRequest
    = SVRq SendVersionRequest
    | OCRq OrderCountRequest
    | DORq DownloadOrdersRequest
    | UnexpectedFunction Text Form
    | InvalidRequest Form
    deriving (Eq, Show)

-- | Decode the Request into the proper endpoint or error.
instance FromForm StoneEdgeRequest where
    fromForm f = parseMaybe @Text "setifunction" f >>= \case
        Just "sendversion" ->
            wrapError $ SVRq <$> fromForm f
        Just "ordercount" ->
            wrapError $ OCRq <$> fromForm f
        Just "downloadorders" ->
            wrapErrorXml Orders $ DORq <$> fromForm f
        Just unexpected ->
            return $ UnexpectedFunction unexpected f
        Nothing ->
            return $ InvalidRequest f
      where
        wrapError :: Either T.Text a -> Either T.Text a
        wrapError = first renderSimpleSETIError
        wrapErrorXml :: TypeOfDownload -> Either T.Text a -> Either T.Text a
        wrapErrorXml type_ = first (decodeUtf8 . renderXmlSETIError type_)

-- | Joins all the StoneEdge Integraton Function Response Types.
data StoneEdgeResponse
    = SVRs SendVersionResponse
    | OCRs OrderCountResponse
    | DORs DownloadOrdersResponse
    | ErrorResponse Text
    | XmlErrorResponse TypeOfDownload Text

-- | Render the responses using the StoneEdge module's rendering functions.
instance MimeRender PlainText StoneEdgeResponse where
    mimeRender pt = \case
        SVRs resp ->
            mimeRender pt $ renderSendVersionResponse resp
        OCRs resp ->
            mimeRender pt $ renderOrderCountResponse resp
        DORs resp ->
            LBS.fromStrict $ renderDownloadOrdersResponse resp
        ErrorResponse errorMessage ->
            mimeRender pt $ renderSimpleSETIError errorMessage
        XmlErrorResponse type_ errorMessage ->
            LBS.fromStrict $ renderXmlSETIError type_ errorMessage


-- Request Handling

-- | Simply return the version of the StoneEdge integration script.
sendVersionRoute :: SendVersionRequest -> App SendVersionResponse
sendVersionRoute = const . return $ SendVersionResponse integrationVersion


-- | Count the Orders with Payments or No Payments necessary that have IDs
-- above the last order StoneEdge imported.
orderCountRoute :: OrderCountRequest -> App OrderCountResponse
orderCountRoute OrderCountRequest { ocrLastOrder } = do
    let orderFilter = case ocrLastOrder of
            NoOrderNumber ->
                [OrderStatus <-. [PaymentReceived, OrderReceived]]
            LastOrderNumber orderNum ->
                [ OrderId >. toSqlKey (fromIntegral orderNum)
                , OrderStatus <-. [PaymentReceived, OrderReceived]
                ]
    OrderCountResponse . fromIntegral <$> runDB (count orderFilter)


-- | Query for the orders to export, transform them into the StoneEdge
-- types and assemble them into a resposne.
downloadOrdersRoute :: DownloadOrdersRequest -> App DownloadOrdersResponse
downloadOrdersRoute DownloadOrdersRequest { dorLastOrder, dorStartNumber, dorBatchSize } = do
    let orderIdFilter o =
            case dorLastOrder of
                NoOrderNumber ->
                    E.val True
                LastOrderNumber lastOrderId ->
                    o E.^. OrderId E.>. E.val (toSqlKey (fromIntegral lastOrderId))
        limitAndOffset =
            case (,) <$> dorBatchSize <*> dorStartNumber of
                Just (limit, offset) ->
                    E.limit (fromIntegral limit) >> E.offset (fromIntegral offset)
                Nothing ->
                    return ()
    rawOrderData <- runDB $ do
        orders <- E.select $ E.from $ \(o `E.InnerJoin` c `E.InnerJoin` sa `E.LeftOuterJoin` ba `E.LeftOuterJoin` cp) -> do
            E.on (o E.^. OrderCouponId E.==. cp E.?. CouponId)
            E.on (o E.^. OrderBillingAddressId E.==. ba E.?. AddressId)
            E.on (o E.^. OrderShippingAddressId E.==. sa E.^. AddressId)
            E.on (o E.^. OrderCustomerId E.==. c E.^. CustomerId)
            E.where_ $
                o E.^. OrderStatus `E.in_` E.valList [PaymentReceived, OrderReceived]
                E.&&. orderIdFilter o
            E.orderBy [E.asc $ o E.^. OrderId]
            limitAndOffset
            return (o, c, sa, ba, cp)
        forM orders $ \(o, c, sa, ba, cp) -> do
            let utcCreated = orderCreatedAt $ entityVal o
            timeZone <- liftIO $ getTimeZone utcCreated
            let createdAt = utcToLocalTime timeZone utcCreated
            lineItems <- selectList [OrderLineItemOrderId ==. entityKey o] []
            products <- E.select $ E.from $ \(op `E.InnerJoin` p `E.InnerJoin` v) -> do
                E.on (op E.^. OrderProductProductVariantId E.==. v E.^. ProductVariantId)
                E.on (v E.^. ProductVariantProductId E.==. p E.^. ProductId)
                return (op, p, v)
            return (o, createdAt, c, sa, ba, cp, lineItems, products)
    return . DownloadOrdersResponse $ map transformOrder rawOrderData

transformOrder
    :: ( Entity Order
       , LocalTime
       , Entity Customer
       , Entity Address
       , Maybe (Entity Address)
       , Maybe (Entity Coupon)
       , [Entity OrderLineItem]
       , [( Entity OrderProduct, Entity Product, Entity ProductVariant )]
       )
    -> StoneEdgeOrder
transformOrder (order, createdAt, customer, shipping, maybeBilling, maybeCoupon, items, products) =
    StoneEdgeOrder
        { seoOrderNumber = fromIntegral . fromSqlKey $ entityKey order
        , seoOrderDate = createdAt
        , seoOrderStatus = Just . T.pack . show . orderStatus $ entityVal order
        , seoBilling = transformBilling $ fromMaybe shipping maybeBilling
        , seoShipping = transformShipping
        , seoPayment = mapMaybe transformStoreCredit items
            ++ maybeToList transformCreditCard
        , seoCoupon = mapMaybe (maybe (const Nothing) transformCoupon maybeCoupon) items
        , seoTotals = transformTotals
        , seoOtherData = transformOtherData
        }
  where
    transformBilling :: Entity Address -> StoneEdgeOrderBilling
    transformBilling (Entity _ addr) =
        StoneEdgeOrderBilling
            { seobFullName = addressFirstName addr <> " " <> addressLastName addr
            , seobCompany = nothingIfNull $ addressCompanyName addr
            , seobPhone = Nothing
            , seobEmail = Just . customerEmail $ entityVal customer
            , seobAddress = transformAddress addr
            }
    transformShipping :: StoneEdgeOrderShipping
    transformShipping =
        let (Entity _ addr) = shipping
        in StoneEdgeOrderShipping
            { seosFullName = addressFirstName addr <> " " <> addressLastName addr
            , seosCompany = nothingIfNull $ addressCompanyName addr
            , seosPhone = Nothing
            , seosEmail = Just . customerEmail $ entityVal customer
            , seosAddress = transformAddress addr
            , seosProduct = map transformProduct products
            }
    transformAddress :: Address -> StoneEdgeOrderAddress
    transformAddress addr =
        let stateCode = case addressState addr of
                USState code ->
                    T.pack $ show code
                USArmedForces code ->
                    T.pack $ show code
                CAProvince code ->
                    T.pack $ show code
                CustomRegion name ->
                    name
        in StoneEdgeOrderAddress
            { seoaStreetOne = addressAddressOne addr
            , seoaStreetTwo = nothingIfNull $ addressAddressTwo addr
            , seoaCity = addressCity addr
            , seoaState = stateCode
            , seoaPostalCode = addressZipCode addr
            , seoaCountry = Just . T.pack . show . fromCountry $ addressCountry addr
            }
    transformProduct
        :: (Entity OrderProduct, Entity Product, Entity ProductVariant)
        -> StoneEdgeOrderProduct
    transformProduct (Entity lineId orderProd, Entity _ prod, Entity _ variant) =
        let q = fromIntegral $ orderProductQuantity orderProd
            p = orderProductPrice orderProd
            t = Cents $ fromIntegral q * fromCents p
        in StoneEdgeOrderProduct
            { seopSKU = productBaseSku prod <> productVariantSkuSuffix variant
            , seopName = productName prod <> lotSuffix variant
            , seopQuantity = q
            , seopItemPrice = convertCents p
            , seopProductType = Just TangibleProduct
            , seopTaxable = Just True
            , seopLineId = Just . fromIntegral $ fromSqlKey lineId
            , seopTotal = Just $ convertCents t
            }
    lotSuffix :: ProductVariant -> T.Text
    lotSuffix pv = case productVariantLotSize pv of
        Nothing ->
            ""
        Just ls ->
            ", " <> renderLotSize ls
    transformCreditCard :: Maybe StoneEdgeOrderPayment
    transformCreditCard =
        case orderStripeChargeId $ entityVal order of
            Nothing ->
                Nothing
            Just transactionId ->
                let maybeCredit = listToMaybe $ mapMaybe (\i ->
                            if orderLineItemType (entityVal i) == StoreCreditLine then
                                Just $ orderLineItemAmount $ entityVal i
                            else Nothing
                        ) items
                in Just . StoneEdgeOrderCreditCard $ StoneEdgePaymentCreditCard
                    { sepccTransactionId =
                        Just . (\(Stripe.ChargeId i) -> i)
                            $ fromStripeChargeId transactionId
                    , sepccCardNumber = orderStripeLastFour $ entityVal order
                    , sepccIssuer = fromMaybe "" . orderStripeIssuer $ entityVal order
                    , sepccAmount = Just . StoneEdgeCents $
                        secCents (setGrandTotal transformTotals)
                            - maybe 0 (fromIntegral . fromCents) maybeCredit
                    }
    transformTotals :: StoneEdgeTotals
    transformTotals =
        let productTotal =
                sum $ map (\(Entity _ op, _, _) ->
                        Cents $ fromIntegral (orderProductQuantity op) * fromCents (orderProductPrice op)
                    )
                    products
            taxTotal =
                sum $ map (\(Entity _ op, _, _) -> orderProductTax op) products
            (discounts, surcharges, maybeShipping, maybeCouponLine) = foldl
                (\(ds, ss, mShip, mCoupon) item ->
                    case orderLineItemType $ entityVal item of
                        SurchargeLine ->
                            (ds, item : ss, mShip, mCoupon)
                        PriorityShippingLine ->
                            (ds, item : ss, mShip, mCoupon)
                        ShippingLine ->
                            (ds, ss, Just item, mCoupon)
                        CouponDiscountLine ->
                            (ds, ss, mShip, Just item)
                        StoreCreditLine ->
                            (ds, ss, mShip, mCoupon)
                        MemberDiscountLine ->
                            (item : ds, ss, mShip, mCoupon)
                ) ([], [], Nothing, Nothing) items
            subTotal = productTotal - sum (map (orderLineItemAmount . entityVal) discounts)
            grandTotal =
                productTotal
                    + taxTotal
                    + maybe 0 (orderLineItemAmount . entityVal) maybeShipping
                    + sum (map (orderLineItemAmount . entityVal) surcharges)
                    - sum (map (orderLineItemAmount . entityVal) discounts)
                    - maybe 0 (orderLineItemAmount . entityVal) maybeCouponLine
        in StoneEdgeTotals
            { setProductTotal = convertCents productTotal
            , setDiscount = map transformDiscount discounts
            , setSubTotal = convertCents subTotal
            , setTax = transformTax (orderTaxDescription $ entityVal order) taxTotal
            , setSurcharge = map transformSurcharge surcharges
            , setShippingTotal = fmap transformShippingTotal maybeShipping
            , setGrandTotal = convertCents grandTotal
            }
    transformOtherData :: StoneEdgeOtherData
    transformOtherData =
        let hasPriority =
                any ((== PriorityShippingLine) . orderLineItemType . entityVal) items
            orderComment = orderCustomerComment $ entityVal order
            comments =
                if hasPriority then
                     Just $ orderComment <> "\r\n\r\n---\r\n\r\n" <> "PRIORITY!"
                else
                    nothingIfNull orderComment

        in StoneEdgeOtherData
            { seodOrderInstructions = Nothing
            , seodComments = comments
            , seodCustomerId = Just . fromIntegral . fromSqlKey $ entityKey customer
            }

transformCoupon :: Entity Coupon -> Entity OrderLineItem -> Maybe StoneEdgeCoupon
transformCoupon (Entity _ coupon) (Entity _ item) =
    case orderLineItemType item of
        CouponDiscountLine ->
            Just $ StoneEdgeCoupon
                { secName = couponName coupon
                , secStatus = nothingIfNull $ couponDescription coupon
                , secTotal = convertCents $ orderLineItemAmount item
                , secAppliedPreTax = Just True
                }
        _ ->
            Nothing

transformStoreCredit :: Entity OrderLineItem -> Maybe StoneEdgeOrderPayment
transformStoreCredit (Entity _ item) =
    case orderLineItemType item of
        StoreCreditLine ->
            Just $ StoneEdgeOrderStoreCredit $ StoneEdgePaymentStoreCredit
                { sepscTotal = convertCents $ orderLineItemAmount item
                , sepscDescription = nothingIfNull $ orderLineItemDescription item
                }
        _ ->
            Nothing

-- | Build the StoneEdgeTax, attempting to pull a percentage out of the
-- description.
transformTax :: T.Text -> Cents -> Maybe StoneEdgeTax
transformTax taxDescription taxTotal =
    let maybeRate =
            (fmap (/ 100) . readMaybe . T.unpack) <=<
                fmap (T.takeWhile (\c -> isDigit c || c == '.'))
                . listToMaybe
                . T.split (== '%')
                $ T.dropWhile (not . isDigit) taxDescription
    in if taxTotal == 0 then
            Nothing
        else
            Just StoneEdgeTax
                { setAmount = convertCents taxTotal
                , setRate = maybeRate
                , setShippingTaxed = Just False
                , setTaxExempt = Just False
                , setTaxId = Nothing
                }

transformDiscount :: Entity OrderLineItem -> StoneEdgeDiscount
transformDiscount (Entity _ item) =
    StoneEdgeDiscount
        { sedType = Just SEFlatDiscount
        , sedDescription = nothingIfNull $ orderLineItemDescription item
        , sedPercent = Nothing
        , sedAmount = convertCents $ orderLineItemAmount item
        , sedAppliedPreTax = Just True
        }

transformSurcharge :: Entity OrderLineItem -> StoneEdgeSurcharge
transformSurcharge (Entity _ item) =
    StoneEdgeSurcharge
        { sesTotal = convertCents $ orderLineItemAmount item
        , sesDescription = nothingIfNull $ orderLineItemDescription item
        }

transformShippingTotal :: Entity OrderLineItem -> StoneEdgeShippingTotal
transformShippingTotal (Entity _ item) =
    StoneEdgeShippingTotal
        { sestTotal = convertCents $ orderLineItemAmount item
        , sestDescription = nothingIfNull $ orderLineItemDescription item
        }

-- Utils

-- | Convert the Cents DB Field to StoneEdgeCents.
convertCents :: Cents -> StoneEdgeCents
convertCents =
    StoneEdgeCents . fromIntegral . fromCents

-- | Return nothing if the Text is empty.
nothingIfNull :: T.Text -> Maybe T.Text
nothingIfNull t =
    if T.null t then Nothing else Just t

-- | Pull the credentials from a StoneEdgeRequest & throw
-- a 'StoneEdgeAuthError' if they do not match the values in the 'Config'.
checkAuth :: HasStoneEdgeCredentials a => a -> App ()
checkAuth request = do
    let credentials = getStoneEdgeRequestCredentials request
    expectedCredentials <- asks getStoneEdgeAuth
    when (expectedCredentials /= credentials) $
        throwM StoneEdgeAuthError
