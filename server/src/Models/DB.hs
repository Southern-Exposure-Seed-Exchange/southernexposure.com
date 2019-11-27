{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Models.DB where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Database.Persist.TH
import Numeric.Natural (Natural)

import Models.Fields

import qualified Data.Text as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Category
    name T.Text
    slug T.Text
    parentId CategoryId Maybe
    description T.Text
    imageUrl T.Text
    order Int
    updatedAt UTCTime
    UniqueCategorySlug slug
    deriving Show

Product
    name T.Text
    slug T.Text
    categoryIds [CategoryId]
    baseSku T.Text
    shortDescription T.Text
    longDescription T.Text
    imageUrl T.Text
    isActive Bool
    createdAt UTCTime
    updatedAt UTCTime
    UniqueProductSlug slug
    UniqueBaseSku baseSku
    deriving Show

ProductVariant json
    productId ProductId
    skuSuffix T.Text
    price Cents
    quantity Int64
    lotSize LotSize Maybe
    isActive Bool
    UniqueSku productId skuSuffix
    deriving Show

SeedAttribute json
    productId ProductId
    isOrganic Bool
    isHeirloom Bool
    isSmallGrower Bool
    isRegional Bool
    UniqueAttribute productId
    deriving Show


Page json
    name T.Text
    slug T.Text
    content T.Text
    updatedAt UTCTime
    UniquePageSlug slug
    deriving Show


Customer
    email T.Text
    storeCredit Cents
    memberNumber T.Text
    encryptedPassword T.Text
    authToken T.Text
    stripeId StripeCustomerId Maybe
    avalaraCode AvalaraCustomerCode Maybe
    isAdmin Bool default=false
    UniqueToken authToken
    UniqueAvalaraCustomer avalaraCode !force
    UniqueEmail email

PasswordReset
    customerId CustomerId
    expirationTime UTCTime
    code T.Text
    UniquePasswordReset customerId
    UniqueResetCode code

Review
    customerId CustomerId
    variantId ProductVariantId
    customerName T.Text
    rating Int
    content T.Text
    isApproved Bool
    createdAt UTCTime


Cart
    customerId CustomerId Maybe
    sessionToken T.Text Maybe
    expirationTime UTCTime Maybe
    UniqueCustomerCart customerId !force
    UniqueAnonymousCart sessionToken !force

CartItem
    cartId CartId
    productVariantId ProductVariantId
    quantity Natural
    UniqueCartItem cartId productVariantId


Surcharge
    description T.Text
    singleFee Cents
    multipleFee Cents
    categoryIds [CategoryId]
    isActive Bool
    UniqueSurcharge description

ShippingMethod
    description T.Text
    countries [Country]
    rates [ShippingRate]
    priorityRate PriorityShippingFee
    categoryIds [CategoryId]
    excludedPriorityCategoryIds [CategoryId]
    isActive Bool
    priority Natural


Coupon
    code T.Text
    name T.Text
    description T.Text
    isActive Bool
    discount CouponType
    minimumOrder Cents
    expirationDate UTCTime
    totalUses Natural
    usesPerCustomer Natural
    createdAt UTCTime
    UniqueCoupon code
    deriving Show


Address
    firstName T.Text
    lastName T.Text
    companyName T.Text
    addressOne T.Text
    addressTwo T.Text
    city T.Text
    state Region
    zipCode T.Text
    country Country
    type AddressType
    customerId CustomerId
    isActive Bool
    isDefault Bool
    deriving Show


Order
    customerId CustomerId
    status OrderStatus
    billingAddressId AddressId Maybe
    shippingAddressId AddressId
    customerComment T.Text
    adminComments [AdminOrderComment]
    couponId CouponId Maybe
    stripeChargeId StripeChargeId Maybe
    stripeLastFour T.Text Maybe
    stripeIssuer T.Text Maybe
    avalaraTransactionCode AvalaraTransactionCode Maybe
    createdAt UTCTime
    deriving Show

OrderLineItem json
    orderId OrderId
    type LineItemType
    description T.Text
    amount Cents
    deriving Show

OrderProduct
    orderId OrderId
    productVariantId ProductVariantId
    quantity Natural
    price Cents
    UniqueOrderProduct orderId productVariantId
    deriving Show


ProductSale
    price Cents
    productVariantId ProductVariantId
    startDate UTCTime
    endDate UTCTime
    deriving Show

CategorySale
    name T.Text
    type SaleType
    startDate UTCTime
    endDate UTCTime
    categoryIds [CategoryId]
    deriving Show
|]
