{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Emails.OrderPlaced (Parameters(..), fetchData, get) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Time (formatTime, defaultTimeLocale, hoursToTimeZone, utcToZonedTime)
import Database.Persist (Entity(..))
import Text.Blaze.Renderer.Text (renderMarkup)

import Models
import Models.Fields (Cents(..), regionName, formatCents, LineItemType(..))

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


data Parameters =
    Parameters
        { customer :: Customer
        , billingAddress :: Maybe Address
        , shippingAddress :: Address
        , order :: Entity Order
        , lineItems :: [OrderLineItem]
        , products :: ProductData
        }

type ProductData = [(OrderProduct, Product, ProductVariant)]


fetchData :: (Monad m, MonadIO m) => OrderId -> E.SqlReadT m (Maybe Parameters)
fetchData orderId = do
    maybeCustomerOrderAddresses <- fmap listToMaybe . E.select . E.from $
        \(o `E.InnerJoin` c `E.InnerJoin` sa `E.LeftOuterJoin` ba) -> do
            E.on $ ba E.?. AddressId E.==. o E.^. OrderBillingAddressId
            E.on $ sa E.^. AddressId E.==. o E.^. OrderShippingAddressId
            E.on $ c E.^. CustomerId E.==. o E.^. OrderCustomerId
            E.where_ $ o E.^. OrderId E.==. E.val orderId
            return (c, o, sa, ba)
    lineItems <- map entityVal <$> P.selectList [OrderLineItemOrderId P.==. orderId] []
    productData <- fmap productDataFromEntities . E.select . E.from $
        \(op `E.InnerJoin` pv `E.InnerJoin` p) -> do
            E.on $ p E.^. ProductId E.==. pv E.^. ProductVariantProductId
            E.on $ pv E.^. ProductVariantId E.==. op E.^. OrderProductProductVariantId
            E.where_ $ op E.^. OrderProductOrderId E.==. E.val orderId
            return (op, p, pv)
    return $ case maybeCustomerOrderAddresses of
        Nothing ->
            Nothing
        Just (Entity _ customer, order, Entity _ shipping, maybeBilling) ->
            Just $ Parameters customer (entityVal <$> maybeBilling) shipping order lineItems productData
    where productDataFromEntities =
            map (\(a, b, c) -> (entityVal a, entityVal b, entityVal c))


get :: Parameters -> (String, L.Text)
get Parameters { shippingAddress, billingAddress, order, products, lineItems } =
    ( "Southern Exposure Seed Exchange - Order #" <> showOrderId (entityKey order) <> " Confirmation"
    , renderMarkup $ render shippingAddress billingAddress order products lineItems
    )

showOrderId :: OrderId -> String
showOrderId = show . E.unSqlBackendKey . unOrderKey


render :: Address
       -> Maybe Address
       -> Entity Order
       -> [(OrderProduct, Product, ProductVariant)]
       -> [OrderLineItem]
       -> H.Html
render shippingAddress billingAddress (Entity orderId order) productData lineItems =
    let
        customerName =
            addressName $ fromMaybe shippingAddress billingAddress
        orderNumber =
            T.pack $ showOrderId orderId
        orderDate =
            T.pack
                . formatTime defaultTimeLocale "%l:%M%P EST on %x"
                . utcToZonedTime (hoursToTimeZone (-5))
                $ orderCreatedAt order
        comment =
            orderCustomerComment order
        customerComments =
            if comment /= "" then
                H.text $ "Your comments: " <> comment <> "\n\n"
            else
                H.text ""
    in
        H.docTypeHtml $ do
            H.head . H.style $
                "table, td, th { border: solid black 1px; } "
                    <> "table { margin-bottom: 10px; } "
                    <> "td, th { padding: 0.25rem; }"
                    <> "address { font-weight: bold; font-style: normal; }"
                    <> "address p { margin: 0; }"
            H.body $ do
                H.p . H.text $ "Dear " <> customerName <> ","
                H.p $
                    "Thanks for ordering from Southern Exposure Seed Exchange! Your items " <>
                    "will be shipped via the US Postal Service, most likely within the next " <>
                    "four days. When your order is shipped, you will receive a second e-mail " <>
                    "from us.\n\n"
                H.p $
                    "If you have questions about your order, please reply to this e-mail or " <>
                    "call us at 540-894-9480.\n\n"
                H.h3 . H.text $
                    "Order Number " <> orderNumber <> ", placed at " <> orderDate
                customerComments
                addressTable shippingAddress billingAddress
                orderTable productData lineItems
                H.br
                H.p "Thank You,"
                H.p "Southern Exposure Seed Exchange"


addressTable :: Address -> Maybe Address -> H.Html
addressTable shippingAddress maybeBillingAddress =
    let
        shippingText = do
            "Ship to:"
            addressHtml shippingAddress
        billingText billingAddress = do
            "Bill to:"
            addressHtml billingAddress
            H.strong . H.text $ "Payment Method: Credit Card"
    in
        H.table $ do
            H.thead $ H.tr $ do
                H.th "Shipping Information"
                when (isJust maybeBillingAddress) $
                    H.th "Billing Information"
            H.tbody $ H.tr $ do
                H.td H.! A.style "vertical-align:top;" $ shippingText
                maybe (return ()) ((H.td H.! A.style "vertical-align:top;") . billingText)
                    maybeBillingAddress
    where addressHtml address = H.address $ do
            addressLine $ addressName address
            addressLine $ addressCompanyName address
            addressLine $ addressAddressOne address
            addressLine $ addressAddressTwo address
            addressLine $
                addressCity address <> ", " <> regionName (addressState address) <>
                " " <> addressZipCode address
          addressLine t =
            unless (t == "") . H.p $ H.text t


addressName :: Address -> T.Text
addressName addr =
    addressFirstName addr <> " " <> addressLastName addr


-- TODO: Pandoc doesn't convert the colspan to markdown nicely. Find a workaround.
orderTable :: [(OrderProduct, Product, ProductVariant)] -> [OrderLineItem] -> H.Html
orderTable productData lineItems =
    let
        subTotal =
            sum $ map (\(op, _, _) -> orderProductPrice op * Cents (orderProductQuantity op))
                productData
        maybeTaxLine =
            listToMaybe $ filter ((== TaxLine) . orderLineItemType) lineItems
        -- TODO: turn this tuple into a type, & do this in a separate
        -- function in the Models.Utils module. Use that function & type in
        -- the OrderDetails route so we don't have to do the same exact
        -- thing in the client code.
        (maybeShippingLine, maybePriorityCharge, maybeStoreCredit, maybeMemberDiscount, maybeCouponDiscount, refunds, surcharges) =
            foldl (\(shippingLine, priorityLine, creditLine, memberLine, couponLine, refundLines, surchargeLines) lineItem ->
                case orderLineItemType lineItem of
                    ShippingLine ->
                        (Just lineItem, priorityLine, creditLine, memberLine, couponLine, refundLines, surchargeLines)
                    PriorityShippingLine ->
                        (shippingLine, Just lineItem, creditLine, memberLine, couponLine, refundLines, surchargeLines)
                    StoreCreditLine ->
                        (shippingLine, priorityLine, Just lineItem, memberLine, couponLine, refundLines, surchargeLines)
                    SurchargeLine ->
                        (shippingLine, priorityLine, creditLine, memberLine, couponLine, refundLines, lineItem : surchargeLines)
                    MemberDiscountLine ->
                        (shippingLine, priorityLine, creditLine, Just lineItem, couponLine, refundLines, surchargeLines)
                    CouponDiscountLine ->
                        (shippingLine, priorityLine, creditLine, memberLine, Just lineItem, refundLines, surchargeLines)
                    RefundLine ->
                        (shippingLine, priorityLine, creditLine, memberLine, couponLine, lineItem : refundLines, surchargeLines)
                    TaxLine ->
                        (shippingLine, priorityLine, creditLine, memberLine, couponLine, refundLines, surchargeLines)

            ) (Nothing, Nothing, Nothing, Nothing, Nothing, [], []) lineItems
        total =
            getOrderTotal lineItems $ map (\(op, _, _) -> op) productData
    in
        H.table $ do
            H.thead $ H.tr $ do
                H.th "SKU"
                H.th "Name"
                H.th "Price"
                H.th H.! A.style "text-align: center;" $ "Quantity"
                H.th "Total"
            H.tbody $ mapM_ productRow productData
            H.tfoot $ do
                H.tr $ do
                    H.th H.! alignRight H.! A.colspan "4" $ "Sub-Total"
                    H.th . H.text $ formatCents subTotal
                maybe (return ()) lineRow maybeShippingLine
                maybe (return ()) lineRow maybePriorityCharge
                mapM_ lineRow surcharges
                maybe (return ()) lineRow maybeTaxLine
                discountRow maybeStoreCredit
                discountRow maybeMemberDiscount
                discountRow maybeCouponDiscount
                mapM_ (discountRow . Just) refunds
                H.tr $ do
                    H.th H.! A.colspan "4" H.! alignRight $ "Total"
                    H.th H.! alignLeft $ H.text $ formatCents total
    where alignRight =
            A.style "text-align:right;"
          alignLeft =
            A.style "text-align:left;"
          productRow (orderProd, prod, variant) =
            H.tr $ do
                let price = orderProductPrice orderProd
                    quantity = orderProductQuantity orderProd
                    total = price * Cents quantity
                H.td . H.text $ productBaseSku prod <> productVariantSkuSuffix variant
                -- TODO: Add Variant Description, Category, & Link to Product
                H.td . H.text $ productName prod
                H.td . H.text . formatCents $ price
                H.td H.! A.style "text-align: center;" $ H.text . T.pack . show $ quantity
                (H.td H.! alignLeft) . H.text . formatCents $ total
          discountRow =
              maybe (return ())
                (\l -> lineRow $ l { orderLineItemAmount = negate $ orderLineItemAmount l })
          lineRow item =
                H.tr $ do
                    H.th H.! A.colspan "4" H.! alignRight $ H.text $ orderLineItemDescription item
                    (H.th H.! alignLeft) . H.text . formatCents $ orderLineItemAmount item
