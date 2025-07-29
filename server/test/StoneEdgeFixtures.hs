{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| The expected string version of rendered XML. This is separated out into
it's own module due to the extraneous vertical height of our XML generator
& to reduce compile times for the test suite caused by the QuasiQuoter.
-}
module StoneEdgeFixtures where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.RawString.QQ (r)



-- | Strip New Lines
sNL :: BS.ByteString -> BS.ByteString
sNL = encodeUtf8 . T.filter (/= '\n') . decodeUtf8

ordersErrorXml :: BS.ByteString
ordersErrorXml =
    sNL [r|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SETIOrders>
<Response>
<ResponseCode>3</ResponseCode>
<ResponseDescription>test error message</ResponseDescription>
</Response>
</SETIOrders>
|]

ordersParseErrorXml :: BS.ByteString
ordersParseErrorXml =
    sNL [r|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SETIOrders>
<Response>
<ResponseCode>3</ResponseCode>
<ResponseDescription>Could not find key &quot;setiuser&quot;</ResponseDescription>
</Response>
</SETIOrders>
|]

noOrdersXml :: BS.ByteString
noOrdersXml =
    sNL [r|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SETIOrders>
<Response>
<ResponseCode>2</ResponseCode>
<ResponseDescription>Success</ResponseDescription>
</Response>
</SETIOrders>
|]

downloadOrdersXml :: BS.ByteString
downloadOrdersXml =
    sNL $ [r|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SETIOrders
><Response
><ResponseCode
>1</ResponseCode
><ResponseDescription
>Success</ResponseDescription
></Response
><Order
><OrderNumber
>9001</OrderNumber
><OrderDate
>Jun/10/2003 00:00:00</OrderDate
><OrderStatus
>Payment Received</OrderStatus
>|] <> orderBillingXml <> orderShippingXml <> paymentCreditCardXml
    <> paymentStoreCreditXml <> orderTotalsXml <> orderCouponXml <> orderOtherDataXml
    <> [r|</Order
></SETIOrders
>|]


orderBillingXml :: BS.ByteString
orderBillingXml =
    [r|<Billing
><FullName
>Kevin Smith</FullName
><Company
>Stone Edge Technologies  Inc.</Company
><Phone
>215-641-1837</Phone
><Email
>kevin@stoneedge.com</Email
><Address
><Street1
>One Valley Square</Street1
><Street2
>Suite 130</Street2
><City
>Blue Bell</City
><State
>PA</State
><Code
>19422</Code
><Country
>US</Country
></Address
></Billing
>|]

orderShippingXml :: BS.ByteString
orderShippingXml =
    [r|<Shipping
><FullName
>Kevin Smith</FullName
><Company
>Stone Edge Technologies  Inc.</Company
><Phone
>215-641-1837</Phone
><Email
>kevin@stoneedge.com</Email
><Address
><Street1
>One Valley Square</Street1
><Street2
>Suite 130</Street2
><City
>Blue Bell</City
><State
>PA</State
><Code
>19422</Code
><Country
>US</Country
></Address
><Product
><SKU
>SHRT</SKU
><Name
>MyShirt</Name
><Quantity
>1</Quantity
><ItemPrice
>5.0</ItemPrice
><ProdType
>Tangible</ProdType
><Taxable
>Yes</Taxable
><LineID
>125487</LineID
><Total
>5.0</Total
></Product
></Shipping
>|]

paymentCreditCardXml :: BS.ByteString
paymentCreditCardXml =
    [r|<Payment
><CreditCard
><Issuer
>Visa</Issuer
><Number
>9001</Number
><TransID
>4729238728739452876</TransID
><Amount
>90.01</Amount
></CreditCard
></Payment
>|]

paymentStoreCreditXml :: BS.ByteString
paymentStoreCreditXml =
    [r|<Payment
><StoreCredit
><Total
>90.01</Total
><Description
>Store Credit Description!</Description
></StoreCredit
></Payment
>|]

orderTotalsXml :: BS.ByteString
orderTotalsXml =
    [r|<Totals
><ProductTotal
>25.0</ProductTotal
><Discount
><Type
>Flat</Type
><Description
>5 Dollars Off</Description
><Amount
>5.0</Amount
><ApplyDiscount
>Pre</ApplyDiscount
></Discount
><SubTotal
>20.0</SubTotal
><Tax
><TaxAmount
>2.68</TaxAmount
><TaxRate
>0.05</TaxRate
><TaxShipping
>No</TaxShipping
></Tax
><GrandTotal
>64.43</GrandTotal
><Surcharge
><Total
>4.0</Total
><Description
>Fall Item Surcharge</Description
></Surcharge
><ShippingTotal
><Total
>8.25</Total
><Description
>Ground</Description
></ShippingTotal
></Totals
>|]

orderCouponXml :: BS.ByteString
orderCouponXml =
    [r|<Coupon
><Name
>ABCCOUPON123</Name
><Status
>5% Off to HHF Customers</Status
><Total
>4.2</Total
><ApplyCoupon
>Pre</ApplyCoupon
></Coupon
>|]

orderOtherDataXml :: BS.ByteString
orderOtherDataXml =
    [r|<Other
><OrderInstructions
>Priority Shipping</OrderInstructions
><Comments
>Long, Multiline
Customer Comments</Comments
><WebCustomerID
>9001</WebCustomerID
></Other
>|]

downloadProdsXml :: BS.ByteString
downloadProdsXml =
    sNL [r|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SETIProducts>
<Response>
<ResponseCode>1</ResponseCode>
<ResponseDescription>Success</ResponseDescription>
</Response>
<Product>
<Code>1</Code>
<Name>test</Name>
<Price>0.0</Price>
<Description>test description</Description>
<Weight>10.0</Weight>
<Discontinued>No</Discontinued>
<QOH>10</QOH>
<CustomFields>
<CustomField>
<FieldName>Organic</FieldName>
<FieldValue>False</FieldValue>
</CustomField>
<CustomField>
<FieldName>Heirloom</FieldName>
<FieldValue>False</FieldValue>
</CustomField>
</CustomFields>
</Product>
<Product>
<Code>2</Code>
<Name>Non-free</Name>
<Price>5.0</Price>
<Weight>0.0</Weight>
<Discontinued>No</Discontinued>
<QOH>20</QOH>
<CustomFields>
<CustomField>
<FieldName>Organic</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
<CustomField>
<FieldName>Heirloom</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
</CustomFields>
</Product>
<Product>
<Code>2A</Code>
<Name>Non-free</Name>
<Price>6.0</Price>
<Weight>0.0</Weight>
<Discontinued>No</Discontinued>
<QOH>30</QOH>
<CustomFields>
<CustomField>
<FieldName>Organic</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
<CustomField>
<FieldName>Heirloom</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
</CustomFields>
</Product>
<Product>
<Code>2B</Code>
<Name>Non-free</Name>
<Price>7.0</Price>
<Weight>0.0</Weight>
<Discontinued>Yes</Discontinued>
<QOH>0</QOH>
<CustomFields>
<CustomField>
<FieldName>Organic</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
<CustomField>
<FieldName>Heirloom</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
</CustomFields>
</Product>
<Product>
<Code>3</Code>
<Name>Another product</Name>
<Price>10.0</Price>
<Weight>0.0</Weight>
<Discontinued>No</Discontinued>
<QOH>40</QOH>
<CustomFields>
<CustomField>
<FieldName>Organic</FieldName>
<FieldValue>True</FieldValue>
</CustomField>
<CustomField>
<FieldName>Heirloom</FieldName>
<FieldValue>False</FieldValue>
</CustomField>
</CustomFields>
</Product>
</SETIProducts>|]
