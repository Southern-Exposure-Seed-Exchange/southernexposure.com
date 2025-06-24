{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Emails.OrderPlaced (Parameters(..), fetchData, get) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Time (formatTime, defaultTimeLocale, hoursToTimeZone, utcToZonedTime)
import Database.Persist (Entity(..), selectFirst)
import Text.Blaze.Renderer.Text (renderMarkup)

import Models
import Models.Fields (mkCents, regionName, formatCents, LineItemType(..))

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Esqueleto.Experimental as E
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
        , messageText :: T.Text
        }

type ProductData = [(OrderProduct, Product, ProductVariant, Maybe SeedAttribute)]


fetchData :: (Monad m, MonadIO m) => OrderId -> E.SqlPersistT m (Maybe Parameters)
fetchData orderId = do
    messageText <- maybe "" (settingsOrderPlacedEmailMessage . entityVal)
        <$> selectFirst [] []
    maybeCustomerOrderAddresses <- fmap listToMaybe . E.select $ do 
        (c E.:& o E.:& sa E.:& ba) <- E.from $ E.table @Customer
            `E.innerJoin` E.table @Order
                `E.on` (\(c E.:& o) -> c E.^. CustomerId E.==. o E.^. OrderCustomerId)
            `E.innerJoin` E.table @Address
                `E.on` (\(_ E.:& o E.:& sa) -> sa E.^. AddressId E.==. o E.^. OrderShippingAddressId)
            `E.leftJoin` E.table @Address
                `E.on` (\(_ E.:& o E.:& _ E.:& ba) -> ba E.?. AddressId E.==. o E.^. OrderBillingAddressId) 
            
        E.where_ $ o E.^. OrderId E.==. E.val orderId
        return (c, o, sa, ba)
    lineItems <- map entityVal <$> P.selectList [OrderLineItemOrderId P.==. orderId] []
    productData <- fmap productDataFromEntities . E.select $ do 
        (op E.:& pv E.:& p E.:& msa) <- E.from $ E.table 
            `E.innerJoin` E.table 
                `E.on` (\(op E.:& pv) -> pv E.^. ProductVariantId E.==. op E.^. OrderProductProductVariantId)
            `E.innerJoin` E.table 
                `E.on` (\(_ E.:& pv E.:& p) -> p E.^. ProductId E.==. pv E.^. ProductVariantProductId)
            `E.leftJoin` E.table
                `E.on` (\(_ E.:& p E.:& msa) -> 
                    msa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId))
                
        E.where_ $ op E.^. OrderProductOrderId E.==. E.val orderId
        return (op, p, pv, msa)
    return $ case maybeCustomerOrderAddresses of
        Nothing ->
            Nothing
        Just (Entity _ customer, order, Entity _ shipping, maybeBilling) ->
            Just $ Parameters customer (entityVal <$> maybeBilling) shipping order lineItems productData messageText
    where productDataFromEntities =
            map (\(a, b, c, d) -> (entityVal a, entityVal b, entityVal c, entityVal <$> d))


get :: Parameters -> (String, L.Text)
get Parameters { shippingAddress, billingAddress, order, products, lineItems, messageText } =
    ( "Southern Exposure Seed Exchange - Order #" <> showOrderId (entityKey order) <> " Confirmation"
    , renderMarkup $ render shippingAddress billingAddress order products lineItems messageText
    )

showOrderId :: OrderId -> String
showOrderId = show . E.unSqlBackendKey . unOrderKey


render :: Address
       -> Maybe Address
       -> Entity Order
       -> [(OrderProduct, Product, ProductVariant, Maybe SeedAttribute)]
       -> [OrderLineItem]
       -> T.Text
       -> H.Html
render shippingAddress billingAddress (Entity orderId order) productData lineItems preamble =
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
                H.preEscapedText preamble
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
orderTable :: [(OrderProduct, Product, ProductVariant, Maybe SeedAttribute)] -> [OrderLineItem] -> H.Html
orderTable productData lineItems =
    let
        subTotal =
            sum $ map (\(op, _, _, _) -> orderProductPrice op * mkCents (orderProductQuantity op))
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
            getOrderTotal lineItems $ map (\(op, _, _, _) -> op) productData
    in
        H.table $ do
            H.thead $ H.tr $ do
                H.th "SKU"
                H.th "Name"
                H.th "Organic"
                H.th "Price"
                H.th H.! alignCenter $ "Quantity"
                H.th "Total"
            H.tbody $ mapM_ productRow productData
            H.tfoot $ do
                H.tr $ do
                    H.th H.! alignRight H.! footerColspan $ "Sub-Total"
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
                    H.th H.! footerColspan H.! alignRight $ "Total"
                    H.th H.! alignLeft $ H.text $ formatCents total
    where alignRight =
            A.style "text-align:right;"
          alignLeft =
            A.style "text-align:left;"
          alignCenter =
            A.style "text-align:center;"
          footerColspan =
            A.colspan "5"
          productRow (orderProd, prod, variant, mAttr) =
            H.tr $ do
                let price = orderProductPrice orderProd
                    quantity = orderProductQuantity orderProd
                    total = price * mkCents quantity
                    isOrganic = flip (maybe "") mAttr $ \attr ->
                        if seedAttributeIsOrganic attr then "✔" else ""
                H.td . H.text $ productBaseSku prod <> productVariantSkuSuffix variant
                -- TODO: Add Variant Description, Category, & Link to Product
                H.td . H.text $ productName prod
                H.td H.! alignCenter $ H.text isOrganic
                H.td . H.text . formatCents $ price
                H.td H.! alignCenter $ H.text . T.pack . show $ quantity
                (H.td H.! alignLeft) . H.text . formatCents $ total
          discountRow =
              maybe (return ())
                (\item ->
                    H.tr $ do
                        H.th H.! footerColspan H.! alignRight $ H.text $ orderLineItemDescription item
                        (H.th H.! alignLeft) . H.text . ("-" <>) . formatCents $ orderLineItemAmount item
                )
          lineRow item =
                H.tr $ do
                    H.th H.! footerColspan H.! alignRight $ H.text $ orderLineItemDescription item
                    (H.th H.! alignLeft) . H.text . formatCents $ orderLineItemAmount item
