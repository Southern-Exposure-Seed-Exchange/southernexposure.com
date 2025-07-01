{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS -Wno-deprecations #-}

module Models.DB where

import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (SqlPersistT, Entity(..), OverflowNatural, selectList, insertEntity, delete, replace)
import Database.Persist.TH

import Models.Fields
import Models.PersistJSON (JSONValue)

import qualified Data.Text as T
import qualified Helcim.API.Types.Payment as Helcim (TransactionId)
import qualified Helcim.API.Types.Customer as Helcim (CustomerId)

-- TODO sand-witch: mkDeleteCascade was deprecated
share [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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
    mainCategory CategoryId
    baseSku T.Text
    shortDescription T.Text
    longDescription T.Text
    imageUrl T.Text
    createdAt UTCTime
    updatedAt UTCTime
    keywords T.Text default=''
    shippingRestrictions [Region] default='[]'
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

ProductToCategory
    productId ProductId
    categoryId CategoryId
    UniqueProductCategory productId categoryId
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
    quantity OverflowNatural
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
    priority OverflowNatural
    isPriorityEnabled Bool default=True


Coupon
    code T.Text
    name T.Text
    description T.Text
    isActive Bool
    discount CouponType
    minimumOrder Cents
    expirationDate UTCTime
    totalUses OverflowNatural
    usesPerCustomer OverflowNatural
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
    phoneNumber T.Text
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
    helcimTransactionId Helcim.TransactionId Maybe
    helcimCardNumber T.Text Maybe
    helcimCardType T.Text Maybe
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
    quantity OverflowNatural
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


Job
    action JSONValue
    queuedAt UTCTime
    runAt UTCTime Maybe
    retries Int
    deriving Show

HelcimPaymentConfirmation
    checkoutToken T.Text
    secretToken T.Text
    UniqueCheckoutToken checkoutToken

HelcimCustomer
    customerId Helcim.CustomerId
    addressId AddressId
    UniqueHelcimCustomer customerId addressId

Settings
    disableCheckout Bool
    disabledCheckoutMessage T.Text
    orderPlacedEmailMessage T.Text default=''
    deriving Show
|]


-- Global Settings

-- | Settings to fallback to if none exist in the database.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsDisableCheckout = False
    , settingsDisabledCheckoutMessage = T.pack ""
    , settingsOrderPlacedEmailMessage = T.pack ""
    }

-- | Fetch settngs from the database, ensuring only one set of values
-- exists, and inserting the default settings if none exist.
getSettings :: MonadIO m => SqlPersistT m (Entity Settings)
getSettings =
    selectList [] [] >>= \case
        [] ->
            insertEntity defaultSettings
        settings : rest ->
            mapM_ (delete . entityKey) rest >> return settings

-- | Update the 'Settings' in the database, ensuring only one set of values
-- exists, and inserting the new value if none exist.
updateSettings :: MonadIO m => Settings -> SqlPersistT m (Entity Settings)
updateSettings newValue =
    selectList [] [] >>= \case
        [] ->
            insertEntity newValue
        Entity settingsId _ : rest ->
            mapM_ (delete . entityKey) rest
                >> replace settingsId newValue
                >> return (Entity settingsId newValue)
