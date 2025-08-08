{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS -Wno-deprecations #-}

module Models.DB where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Database.Persist.Migration
    ( Column(..), ColumnProp(..), Migration, MigrationPath(..), MigrateSql(..), Operation(..), SqlType(..), TableConstraint(..)
    , checkMigration, (~>)
    )
import qualified Database.Persist.Migration as Migration (defaultSettings)
import Database.Persist.Migration.Postgres (runMigration,)
import Database.Persist.Sql
    (Entity(..), PersistField(..), SqlPersistT, selectList, insertEntity, delete, replace)
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
    imageUrls [T.Text]
    createdAt UTCTime
    updatedAt UTCTime
    keywords T.Text default=''
    shippingRestrictions [Region] default='[]'
    deleted Bool default=false
    -- deletion is soft, so we can ensure SKU uniqueness, as well as keep the product in the database
    -- and restore it later if needed
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
    encryptedPassword T.Text Maybe
    authToken T.Text
    stripeId StripeCustomerId Maybe
    helcimCustomerId Helcim.CustomerId Maybe
    avalaraCode AvalaraCustomerCode Maybe
    isAdmin Bool default=false
    verified Bool default=true
    UniqueToken authToken
    UniqueAvalaraCustomer avalaraCode !force
    UniqueEmail email

Verification
    code T.Text
    customerId CustomerId OnDeleteCascade
    createdAt UTCTime
    UniqueVerificationCode code
    UniqueCustomerToVerify customerId

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
    quantity Int64
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
    priority Int64
    isPriorityEnabled Bool default=true


Coupon
    code T.Text
    name T.Text
    description T.Text
    isActive Bool
    discount CouponType
    minimumOrder Cents
    expirationDate UTCTime
    totalUses Int64
    usesPerCustomer Int64
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
    guestToken T.Text Maybe
    status OrderStatus
    stoneEdgeStatus T.Text Maybe
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
    UniqueGuestToken guestToken !force -- NB: two NULL values are not considered equal
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
    quantity Int64
    price Cents
    UniqueOrderProduct orderId productVariantId
    deriving Show

OrderDelivery
    orderId OrderId
    trackNumber T.Text
    trackCarrier T.Text
    trackPickupDate T.Text
    UniqueOrderDelivery orderId trackNumber trackCarrier
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

-- Migrations

runMigrations :: SqlPersistT IO ()
runMigrations = do
    runMigration Migration.defaultSettings migrations
    checkMigration migrateAll

migrations :: Migration
migrations =
    [ 0 ~> 1 :=
        -- Initial migration to create the DB schema as of '38f3ccffba4dafab9b6028db878305c9f27a402c' revision.
        [ CreateTable
            { name = "category"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "name" SqlString [NotNull]
                , Column "slug" SqlString [NotNull]
                , Column "parent_id" SqlInt64 [References ("category", "id")]
                , Column "description" SqlString [NotNull]
                , Column "image_url" SqlString [NotNull]
                , Column "order" SqlInt64 [NotNull]
                , Column "updated_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_category_slug" ["slug"]
                ]
            }
        , CreateTable
            { name = "product"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "name" SqlString [NotNull]
                , Column "slug" SqlString [NotNull]
                , Column "main_category" SqlInt64 [References ("category", "id"), NotNull]
                , Column "base_sku" SqlString [NotNull]
                , Column "short_description" SqlString [NotNull]
                , Column "long_description" SqlString [NotNull]
                , Column "image_url" SqlString [NotNull]
                , Column "created_at" SqlDayTime [NotNull]
                , Column "updated_at" SqlDayTime [NotNull]
                , Column "keywords" SqlString [NotNull, Default $ toPersistValue ("" :: T.Text)]
                , Column "shipping_restrictions" SqlString [NotNull, Default $ toPersistValue ("[]" :: T.Text)]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_product_slug" ["slug"]
                , Unique "unique_base_sku" ["base_sku"]
                ]
            }
        , CreateTable
            { name = "product_variant"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "product_id" SqlInt64 [References ("product", "id"), NotNull]
                , Column "sku_suffix" SqlString [NotNull]
                , Column "price" SqlInt64 [NotNull]
                , Column "quantity" SqlInt64 [NotNull]
                , Column "lot_size" SqlString []
                , Column "is_active" SqlBool [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_sku" ["product_id", "sku_suffix"]
                ]
            }
        , CreateTable
            { name = "seed_attribute"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "product_id" SqlInt64 [References ("product", "id"), NotNull]
                , Column "is_organic" SqlBool [NotNull]
                , Column "is_heirloom" SqlBool [NotNull]
                , Column "is_small_grower" SqlBool [NotNull]
                , Column "is_regional" SqlBool [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_attribute" ["product_id"]
                ]
            }
        , CreateTable
            { name = "product_to_category"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "product_id" SqlInt64 [References ("product", "id"), NotNull]
                , Column "category_id" SqlInt64 [References ("category", "id"), NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_product_category" ["product_id", "category_id"]
                ]
            }
        , CreateTable
            { name = "page"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "name" SqlString [NotNull]
                , Column "slug" SqlString [NotNull]
                , Column "content" SqlString [NotNull]
                , Column "updated_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_page_slug" ["slug"]
                ]
            }
        , CreateTable
            { name = "customer"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "email" SqlString [NotNull]
                , Column "store_credit" SqlInt64 [NotNull]
                , Column "member_number" SqlString [NotNull]
                , Column "encrypted_password" SqlString []
                , Column "auth_token" SqlString [NotNull]
                , Column "stripe_id" SqlString []
                , Column "helcim_customer_id" SqlInt64 []
                , Column "avalara_code" SqlString []
                , Column "is_admin" SqlBool [NotNull, Default $ toPersistValue False]
                , Column "verified" SqlBool [NotNull, Default $ toPersistValue True]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_token" ["auth_token"]
                , Unique "unique_avalara_customer" ["avalara_code"]
                , Unique "unique_email" ["email"]
                ]
            }
        , CreateTable
            { name = "verification"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "code" SqlString [NotNull]
                , Column "customer_id" SqlInt64 [References ("customer", "id"), NotNull]
                , Column "created_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_verification_code" ["code"]
                , Unique "unique_customer_to_verify" ["customer_id"]
                ]
            }
        -- persistent-migration doesn't support non-default 'ON DELETE *' 'ON UPDATE *' for foreign keys,
        -- so we need to manually set it
        , RawOperation "" $ return
            [MigrateSql "ALTER TABLE \"verification\" DROP CONSTRAINT \"verification_customer_id_fkey\"" []]
        , RawOperation "" $ return
            [MigrateSql "ALTER TABLE \"verification\" ADD CONSTRAINT \"verification_customer_id_fkey\" FOREIGN KEY(\"customer_id\") REFERENCES \"customer\"(\"id\") ON DELETE CASCADE  ON UPDATE RESTRICT" []]
        , CreateTable
            { name = "password_reset"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "customer_id" SqlInt64 [References ("customer", "id"), NotNull]
                , Column "expiration_time" SqlDayTime [NotNull]
                , Column "code" SqlString [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_password_reset" ["customer_id"]
                , Unique "unique_reset_code" ["code"]
                ]
            }
        , CreateTable
            { name = "review"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "customer_id" SqlInt64 [References ("customer", "id"), NotNull]
                , Column "variant_id" SqlInt64 [References ("product_variant", "id"), NotNull]
                , Column "customer_name" SqlString [NotNull]
                , Column "rating" SqlInt64 [NotNull]
                , Column "content" SqlString [NotNull]
                , Column "is_approved" SqlBool [NotNull]
                , Column "created_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "cart"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "customer_id" SqlInt64 [References ("customer", "id")]
                , Column "session_token" SqlString []
                , Column "expiration_time" SqlDayTime []
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_customer_cart" ["customer_id"]
                , Unique "unique_anonymous_cart" ["session_token"]
                ]
            }
        , CreateTable
            { name = "cart_item"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "cart_id" SqlInt64 [References ("cart", "id"), NotNull]
                , Column "product_variant_id" SqlInt64 [References ("product_variant", "id"), NotNull]
                , Column "quantity" SqlInt64 [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_cart_item" ["cart_id", "product_variant_id"]
                ]
            }
        , CreateTable
            { name = "surcharge"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "description" SqlString [NotNull]
                , Column "single_fee" SqlInt64 [NotNull]
                , Column "multiple_fee" SqlInt64 [NotNull]
                , Column "category_ids" SqlString [NotNull]
                , Column "is_active" SqlBool [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_surcharge" ["description"]
                ]
            }
        , CreateTable
            { name = "shipping_method"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "description" SqlString [NotNull]
                , Column "countries" SqlString [NotNull]
                , Column "rates" SqlString [NotNull]
                , Column "priority_rate" SqlString [NotNull]
                , Column "category_ids" SqlString [NotNull]
                , Column "excluded_priority_category_ids" SqlString [NotNull]
                , Column "is_active" SqlBool [NotNull]
                , Column "priority" SqlInt64 [NotNull]
                , Column "is_priority_enabled" SqlBool [NotNull, Default $ toPersistValue True]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "coupon"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "code" SqlString [NotNull]
                , Column "name" SqlString [NotNull]
                , Column "description" SqlString [NotNull]
                , Column "is_active" SqlBool [NotNull]
                , Column "discount" SqlString [NotNull]
                , Column "minimum_order" SqlInt64 [NotNull]
                , Column "expiration_date" SqlDayTime [NotNull]
                , Column "total_uses" SqlInt64 [NotNull]
                , Column "uses_per_customer" SqlInt64 [NotNull]
                , Column "created_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_coupon" ["code"]
                ]
            }
        , CreateTable
            { name = "address"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "first_name" SqlString [NotNull]
                , Column "last_name" SqlString [NotNull]
                , Column "company_name" SqlString [NotNull]
                , Column "address_one" SqlString [NotNull]
                , Column "address_two" SqlString [NotNull]
                , Column "city" SqlString [NotNull]
                , Column "state" SqlString [NotNull]
                , Column "zip_code" SqlString [NotNull]
                , Column "country" SqlString [NotNull]
                , Column "phone_number" SqlString [NotNull]
                , Column "type" SqlString [NotNull]
                , Column "customer_id" SqlInt64 [References ("customer", "id"), NotNull]
                , Column "is_active" SqlBool [NotNull]
                , Column "is_default" SqlBool [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "order"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "customer_id" SqlInt64 [References ("customer", "id"), NotNull]
                , Column "guest_token" SqlString []
                , Column "status" SqlString [NotNull]
                , Column "stone_edge_status" SqlString []
                , Column "billing_address_id" SqlInt64 [References ("address", "id")]
                , Column "shipping_address_id" SqlInt64 [References ("address", "id"), NotNull]
                , Column "customer_comment" SqlString [NotNull]
                , Column "admin_comments" SqlString [NotNull]
                , Column "coupon_id" SqlInt64 [References ("coupon", "id")]
                , Column "stripe_charge_id" SqlString []
                , Column "stripe_last_four" SqlString []
                , Column "stripe_issuer" SqlString []
                , Column "helcim_transaction_id" SqlInt64 []
                , Column "helcim_card_number" SqlString []
                , Column "helcim_card_type" SqlString []
                , Column "avalara_transaction_code" SqlString []
                , Column "created_at" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_guest_token" ["guest_token"] -- NB: two NULL values are not considered equal
                ]
            }
        , CreateTable
            { name = "order_line_item"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "order_id" SqlInt64 [References ("order", "id"), NotNull]
                , Column "type" SqlString [NotNull]
                , Column "description" SqlString [NotNull]
                , Column "amount" SqlInt64 [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "order_product"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "order_id" SqlInt64 [References ("order", "id"), NotNull]
                , Column "product_variant_id" SqlInt64 [References ("product_variant", "id"), NotNull]
                , Column "quantity" SqlInt64 [NotNull]
                , Column "price" SqlInt64 [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_order_product" ["order_id", "product_variant_id"]
                ]
            }
        , CreateTable
            { name = "order_delivery"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "order_id" SqlInt64 [References ("order", "id"), NotNull]
                , Column "track_number" SqlString [NotNull]
                , Column "track_carrier" SqlString [NotNull]
                , Column "track_pickup_date" SqlString [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                , Unique "unique_order_delivery" ["order_id", "track_number", "track_carrier"]
                ]
            }
        , CreateTable
            { name = "product_sale"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "price" SqlInt64 [NotNull]
                , Column "product_variant_id" SqlInt64 [References ("product_variant", "id"), NotNull]
                , Column "start_date" SqlDayTime [NotNull]
                , Column "end_date" SqlDayTime [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "category_sale"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "name" SqlString [NotNull]
                , Column "type" SqlString [NotNull]
                , Column "start_date" SqlDayTime [NotNull]
                , Column "end_date" SqlDayTime [NotNull]
                , Column "category_ids" SqlString [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "job"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "action" (SqlOther "JSONB") [NotNull]
                , Column "queued_at" SqlDayTime [NotNull]
                , Column "run_at" SqlDayTime []
                , Column "retries" SqlInt64 [NotNull]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        , CreateTable
            { name = "settings"
            , schema =
                [ Column "id" SqlInt64 [NotNull, AutoIncrement]
                , Column "disable_checkout" SqlBool [NotNull]
                , Column "disabled_checkout_message" SqlString [NotNull]
                , Column "order_placed_email_message" SqlString [NotNull, Default $ toPersistValue ("" :: T.Text)]
                ]
            , constraints =
                [ PrimaryKey ["id"]
                ]
            }
        ]
    , 1 ~> 2 :=
        -- Add 'imageUrls' to 'Product' and move items from 'imageUrl' to 'imageUrls'
        [ AddColumn "product" (Column "image_urls" SqlString [NotNull]) (Just $ toPersistValue ([] :: [T.Text]))
        , RawOperation "Move urls from imageUrl to imageUrls" $
            -- "<url>" -> "[s<url>]"
            -- '''''' is an escaped "''" string which is used to represent an empty 'Text' value in the database
            -- since image_url is not nullable, the default value is an empty string.
            -- We migrate only if image_url is not an empty string.
            --
            -- [Text] will be stored as a JSON array in the database. Each string value has to start with 's' according
            -- to ToJSON instance for PersistentValue
            return [MigrateSql "UPDATE product SET image_urls = CONCAT('[\"s', image_url, '\"]') WHERE image_url != ''''''" []]
        , DropColumn ("product", "image_url")
        ]
    , 2 ~> 3 :=
        -- Add 'deleted' to 'Product'
        [ AddColumn "product" (Column "deleted" SqlBool [NotNull, Default $ toPersistValue False]) (Just $ toPersistValue False)
        ]
    ]
