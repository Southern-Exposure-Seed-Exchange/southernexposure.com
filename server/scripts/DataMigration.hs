{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (lines, product)

import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Control.Exception (evaluate)
import Control.Monad (foldM, void, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isAlpha)
import Data.Int (Int32)
import Data.List (nubBy, partition, intercalate)
import Data.Maybe (maybeToList, fromMaybe, isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Pool (destroyAllResources)
import Data.Ratio ((%))
import Data.Scientific (Scientific)
import Data.Time
    ( LocalTime(..), Day(..), UTCTime(..), hoursToTimeZone, localTimeToUTC
    , getCurrentTimeZone, midnight
    )
import Database.MySQL.Base
    ( MySQLConn, Query(..), query_, close, MySQLValue(..), prepareStmt
    , queryStmt, closeStmt
    )
import Database.Persist
    ( (<-.), (+=.), (=.), (==.), Entity(..), Filter, getBy, insert, insertMany_
    , upsert, deleteWhere, selectKeysList, insert_, selectList, update, upsertBy
    , selectFirst
    )
import Database.Persist.Postgresql
    ( ConnectionPool, SqlWriteT, createPostgresqlPool, toSqlKey, fromSqlKey
    , runSqlPool, runMigration
    )
import Numeric.Natural (Natural)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

import Models
import Models.Fields
import Utils

import qualified Data.CAProvinceCodes as CACodes
import qualified Data.HashMap.Strict as M
import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.IntMap as IntMap
import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified System.IO.Streams as Streams


-- | A list of old product ids, base skus, and sku suffixes for products
-- that were deleted in ZenCart and then recreated with the same SKU. These
-- product IDs are still present in the orders_products table so we add an
-- entry in the variant map after inserting all variants.
recreatedProducts :: [(Int, T.Text, T.Text)]
recreatedProducts =
    [ (1039, "92504", "")       -- Allium Mix
    , (1641, "92506", "")       -- Asiatic & Turban Garlic Sampler
    ]

-- | A list of old product ids, new Products, & new Variants for products
-- that were deleted in ZenCart. These are necessary because the product
-- IDs still exist in ZenCart's orders_products table. They are inserted
-- and added to the variant map.
deletedProducts :: [(Int, Product, ProductVariant)]
deletedProducts =
    let createdAt = UTCTime (ModifiedJulianDay 0) 0 in
    [ ( 1639
      , Product
            { productName = "Free 2013 Catalog & Garden Guide"
            , productSlug = slugify "Free 2013 Catalog & Garden Guide"
            , productCategoryIds = []
            , productBaseSku = "99001"
            , productShortDescription = ""
            , productLongDescription = ""
            , productImageUrl = ""
            , productIsActive = False
            , productCreatedAt = createdAt
            }
      , ProductVariant
            { productVariantProductId = toSqlKey 0
            , productVariantSkuSuffix = ""
            , productVariantPrice = 0
            , productVariantQuantity = 0
            , productVariantLotSize = Nothing
            , productVariantIsActive = False
            }
      )
    , ( 1811
      , Product
            { productName = "Lahontan White Softneck Garlic 8 oz."
            , productSlug = slugify "Lahontan White Softneck Garlic 8 oz."
            , productCategoryIds = []
            , productBaseSku = "99965348"
            , productShortDescription = ""
            , productLongDescription = ""
            , productImageUrl = ""
            , productIsActive = False
            , productCreatedAt = createdAt
            }
      , ProductVariant
            { productVariantProductId = toSqlKey 0
            , productVariantSkuSuffix = ""
            , productVariantPrice = 1195
            , productVariantQuantity = 0
            , productVariantLotSize = Just . Mass $ Milligrams 226
            , productVariantIsActive = False
            }
      )
    ]


main :: IO ()
main = do
    mysqlConn <- connectToMysql
    psqlConn <- connectToPostgres
    mysqlProducts <- makeProducts mysqlConn
    putStrLn "Making Categories"
    categories <- makeCategories mysqlConn
    putStrLn "Making Category Sales"
    categorySales <- makeCategorySales mysqlConn
    putStrLn "Making Products/Variants"
    let products = mergeProducts
            $ map (\(_, catId, _, _, _, _, _, p) -> (catId, p)) mysqlProducts
        variants = makeVariants mysqlProducts
    putStrLn "Making Products Sales"
    productSales <- makeProductSales mysqlConn
    putStrLn "Making Seed Attributes"
    attributes <- makeSeedAttributes mysqlConn
    putStrLn "Making Pages"
    pages <- makePages mysqlConn
    putStrLn "Making Customers"
    customers <- makeCustomers mysqlConn
    putStrLn "Making Addresses"
    addresses <- makeAddresses mysqlConn
    putStrLn "Making Carts"
    carts <- makeCustomerCarts mysqlConn
    putStrLn "Making Coupons"
    coupons <- makeCoupons mysqlConn
    putStrLn "Making Orders"
    orders <- makeOrders mysqlConn
    flip runSqlPool psqlConn $ do
        runMigration migrateAll
        liftPutStrLn "Clearing Database"
        dropNewDatabaseRows
        liftPutStrLn "Inserting Categories"
        categoryMap <- insertCategories categories
        liftPutStrLn "Inserting Category Sales"
        insertCategorySales categorySales categoryMap
        liftPutStrLn "Inserting Products"
        insertProducts products categoryMap
        liftPutStrLn "Inserting Variants"
        variantMap <- insertVariants variants
        liftPutStrLn "Inserting Seed Attributes"
        insertAttributes attributes
        liftPutStrLn "Inserting Product Sales"
        insertProductSales productSales variantMap
        liftPutStrLn "Inserting Pages"
        insertPages pages
        liftPutStrLn "Inserting Customers"
        customerMap <- insertCustomers customers
        liftPutStrLn "Inserting Addresses"
        insertAddresses customerMap addresses
        liftPutStrLn "Inserting Charges"
        insertCharges
        liftPutStrLn "Inserting Carts"
        insertCustomerCarts variantMap customerMap carts
        deleteInactiveCartItems
        liftPutStrLn "Inserting Coupons"
        insertCoupons coupons
        liftPutStrLn "Inserting Orders"
        insertOrders customerMap variantMap orders
    close mysqlConn
    destroyAllResources psqlConn
  where
    liftPutStrLn = lift . putStrLn
    mergeProducts = nubByWith
        (\(_, p1) (_, p2) -> productBaseSku p1 == productBaseSku p2)
        (\(cat, prod) (_, nextProd) ->
            let base = if productIsActive prod then prod else nextProd in
            ( cat
            , base
                { productIsActive =
                    productIsActive prod || productIsActive nextProd
                }
            )
        )




type OldIdMap a = IntMap.IntMap a


-- DB Utility Functions

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 4


dropNewDatabaseRows :: SqlWriteT IO ()
dropNewDatabaseRows =
    deleteWhere ([] :: [Filter SeedAttribute])
        >> deleteWhere ([] :: [Filter ProductSale])
        >> deleteWhere ([] :: [Filter CategorySale])
        >> deleteWhere ([] :: [Filter Coupon])
        >> deleteWhere ([] :: [Filter TaxRate])
        >> deleteWhere ([] :: [Filter Surcharge])
        >> deleteWhere ([] :: [Filter ShippingMethod])
        >> deleteWhere ([] :: [Filter CartItem])
        >> deleteWhere ([] :: [Filter Cart])
        >> deleteWhere ([] :: [Filter OrderProduct])
        >> deleteWhere ([] :: [Filter OrderLineItem])
        >> deleteWhere ([] :: [Filter Order])
        >> deleteWhere ([] :: [Filter Address])
        >> deleteWhere ([] :: [Filter ProductVariant])
        >> deleteWhere ([] :: [Filter Product])
        >> deleteWhere ([] :: [Filter Category])
        >> deleteWhere ([] :: [Filter Page])
        >> deleteWhere ([] :: [Filter Customer])


-- MySQL -> Persistent Functions

makeCategories :: MySQLConn -> IO [(Int, Int, Category)]
makeCategories mysql = do
    categories <- mysqlQuery mysql $
        "SELECT c.categories_id, categories_image, parent_id, sort_order,"
        <> "    categories_name, categories_description "
        <> "FROM categories as c "
        <> "LEFT JOIN categories_description as cd ON c.categories_id=cd.categories_id "
        <> "WHERE categories_status=1 "
        <> "ORDER BY parent_id ASC"
    mapM toData categories
    where toData [ MySQLInt32 catId, nullableImageUrl
                 , MySQLInt32 parentId, MySQLInt32 catOrder
                 , MySQLText name, MySQLText description
                 ] =
            let imgUrl = fromNullableText "" nullableImageUrl in
                return
                    ( fromIntegral catId
                    , fromIntegral parentId
                    , Category name (slugify name) Nothing description (T.pack . takeFileName $ T.unpack imgUrl) (fromIntegral catOrder)
                    )
          toData r = print r >> error "Category Lambda Did Not Match"


makeCategorySales :: MySQLConn -> IO [([Int], CategorySale)]
makeCategorySales mysql = do
    sales <- mysqlQuery mysql $
        "SELECT sale_name, sale_deduction_value, sale_deduction_type,"
        <> "    sale_categories_selected, sale_date_start, sale_date_end "
        <> "FROM salemaker_sales"
    filter (\(_, cs) -> categorySaleName cs /= "" && categorySaleName cs /= "GuardN Inoculant")
        <$> mapM makeCategorySale sales
    where
        makeCategorySale [ MySQLText name, MySQLDecimal deduction, MySQLInt8 deductionType
                        , MySQLText categoryIds, MySQLDate startDay, MySQLDate endDay
                        ] = do
            utcStart <- dayToUTC startDay
            utcEnd <- dayToUTC endDay
            return
                ( fixCategories categoryIds
                , CategorySale
                    { categorySaleName = name
                    , categorySaleType = saleType deduction deductionType
                    , categorySaleStartDate = utcStart
                    , categorySaleEndDate = utcEnd
                    , categorySaleCategoryIds = []
                    }
                )
        makeCategorySale _ = error "Invalid arguments to makeCategorySale"
        saleType amount type_ = case type_ of
            0 -> FlatSale . Cents $ floor amount
            1 -> PercentSale $ floor amount
            _ -> error $ "Could not read category sale type: " <> show type_
        fixCategories str =
            map readCategory . filter (/= "") $ T.split (== ',') str
        readCategory str =
            fromMaybe
                (error $ "Could not read sale category ID: " <> T.unpack str)
                (readMaybe $ T.unpack str)



makeProducts :: MySQLConn -> IO [(Int32, Int, T.Text, Scientific, Float, Float, Bool, Product)]
makeProducts mysql = do
    products <- mysqlQuery mysql $
        "SELECT products_id, master_categories_id, products_price,"
        <> "    products_quantity, products_weight, products_model,"
        <> "    products_image, products_status, products_date_added "
        <> "FROM products"
    mapM makeProduct products
    where
        makeProduct
         [ MySQLInt32 prodId, MySQLInt32 catId, MySQLDecimal prodPrice
         , MySQLFloat prodQty, MySQLFloat prodWeight, MySQLText prodSKU
         , MySQLText prodImg, MySQLInt8 prodStatus, MySQLDateTime created
         ] = do
             -- TODO: Just use join query?
            queryString <- prepareStmt mysql . Query $
                "SELECT products_id, products_name, products_description "
                <> "FROM products_description WHERE products_id=?"
            (_, descriptionStream) <- queryStmt mysql queryString [MySQLInt32 prodId]
            [_, MySQLText dbName, MySQLText description] <- head <$> Streams.toList descriptionStream
            closeStmt mysql queryString
            let name = if dbName == ""
                    then "Inactive Product - " <> T.pack (show prodId)
                    else dbName
            let (baseSku, skuSuffix) = splitSku prodSKU
                isActive = prodStatus == 1
            let blankDate = UTCTime (ModifiedJulianDay 0) 0
            createdAt <- if show created == "0001-01-01 00:00:00"
                then return blankDate
                else convertLocalTimeToUTC created
            return ( prodId, fromIntegral catId, skuSuffix
                   , prodPrice, prodQty, prodWeight, isActive
                   , Product
                        { productName = name
                        , productSlug = slugify name
                        , productCategoryIds = []
                        , productBaseSku = T.toUpper baseSku
                        , productShortDescription = ""
                        , productLongDescription = description
                        , productImageUrl = T.pack . takeFileName $ T.unpack prodImg
                        , productIsActive = isActive
                        , productCreatedAt = createdAt
                        }
                   )
        makeProduct _ = error "Invalid arguments to makeProduct."


makeVariants :: [(Int32, Int, T.Text, Scientific, Float, Float, Bool, Product)] -> [(Int, T.Text, ProductVariant)]
makeVariants =
    map makeVariant
    where
        makeVariant (productId, _, suffix, price, qty, weight, isActive, prod) =
            (fromIntegral productId, productBaseSku prod,) $
                ProductVariant
                    (toSqlKey 0)
                    (T.toUpper suffix)
                    (dollarsToCents price)
                    (floor qty)
                    (makeLotSize weight)
                    isActive
        makeLotSize :: Float -> Maybe LotSize
        makeLotSize weight =
            if weight == 0 then
                Nothing
            else
                Just . Mass . Milligrams . round $ 1000 * weight



makeSeedAttributes :: MySQLConn -> IO [(T.Text, SeedAttribute)]
makeSeedAttributes mysql = do
    attributes <- mysqlQuery mysql $
        "SELECT p.products_id, products_model, is_eco AS small_grower,"
        <> "    is_organic, is_heirloom, is_southern "
        <> "FROM sese_products_icons as i "
        <> "RIGHT JOIN products AS p "
        <> "ON p.products_id=i.products_id"
    nubBy (\a1 a2 -> fst a1 == fst a2) <$> mapM toData attributes
    where toData [ MySQLInt32 _, MySQLText prodSku, MySQLInt8 isSmallGrower
                 , MySQLInt8 isOrg, MySQLInt8 isHeir, MySQLInt8 isRegion
                 ] =
            return . (fst $ splitSku prodSku,) $
                SeedAttribute
                    { seedAttributeProductId = toSqlKey 0
                    , seedAttributeIsOrganic = toBool isOrg
                    , seedAttributeIsHeirloom = toBool isHeir
                    , seedAttributeIsSmallGrower = toBool isSmallGrower
                    , seedAttributeIsRegional = toBool isRegion
                    }
          toData r = print r >> error "seed attribute lambda did not match"
          toBool = (==) 1


splitSku :: T.Text -> (T.Text, T.Text)
splitSku fullSku =
    case T.split isAlpha fullSku of
         baseSku : "" : _ ->
            case T.stripPrefix baseSku fullSku of
                Just skuSuffix ->
                    (baseSku, skuSuffix)
                Nothing ->
                    (fullSku, "")
         _ ->
            (fullSku, "")


makeProductSales :: MySQLConn -> IO [(Int, ProductSale)]
makeProductSales mysql = do
    sales <- mysqlQuery mysql $
        "SELECT products_id, specials_new_products_price, expires_date,"
        <> "    specials_date_available "
        <> "FROM specials"
    mapM makeProductSale sales
    where
        makeProductSale [ MySQLInt32 productId, MySQLDecimal salePrice, MySQLDate endDate
                        , MySQLDate startDate
                        ] = do
            utcStart <- dayToUTC startDate
            utcEnd <- dayToUTC endDate
            return
                ( fromIntegral productId
                , ProductSale (dollarsToCents salePrice) (toSqlKey 0)
                    utcStart utcEnd
                )
        makeProductSale _ = error "Invalid arguemnts to makeProductSale."


makePages :: MySQLConn -> IO [Page]
makePages mysql =
    (map makePage <$>) . Streams.toList . snd
        =<< (query_ mysql . Query
            $ "SELECT pages_title, pages_html_text"
            <> "    FROM ezpages WHERE pages_html_text <> \"\"")
    where
        makePage [MySQLText name, MySQLText content] =
            Page name (slugify name) content
        makePage _ = error "Invalid arguments to makePage."


makeCustomers :: MySQLConn -> IO [([Int], Customer)]
makeCustomers mysql = do
    storeCreditMap <- getStoreCreditMap mysql
    customersWithAccounts <- customersToMap
        <$> (customerQuery False >>= mapM (makeCustomer storeCreditMap))
    customersNoAccounts <- customersToMap
        <$> (customerQuery True >>= mapM (makeCustomer storeCreditMap))
    let allCustomers = M.unionWith mergeCustomers customersWithAccounts customersNoAccounts
    return $ M.elems allCustomers
    where
        customerQuery checkoutWithoutAccount = do
            queryString <- prepareStmt mysql . Query $
                "SELECT customers_id, customers_email_address "
                <> "FROM customers WHERE COWOA_account=?"
            let cowoaVal = if checkoutWithoutAccount then 1 else 0
            (_, customerStream) <- queryStmt mysql queryString [MySQLInt8 cowoaVal]
            cs <- Streams.toList customerStream
            closeStmt mysql queryString
            return cs
        customersToMap =
            M.fromListWith mergeCustomers . map (\c@(_, cust) -> (customerEmail cust, c))
        mergeCustomers (ids1, c1) (ids2, c2) =
            ( ids1 <> ids2
            , c1
                { customerStoreCredit =
                    customerStoreCredit c1 + customerStoreCredit c2
                }
            )
        makeCustomer :: IntMap.IntMap Cents -> [MySQLValue] -> IO ([Int], Customer)
        makeCustomer creditMap [MySQLInt32 customerId, MySQLText email ] = do
            let storeCredit = fromMaybe 0
                    $ IntMap.lookup (fromIntegral customerId) creditMap
            token <- generateToken
            return . ([fromIntegral customerId],) $ Customer
                { customerEmail = email
                , customerStoreCredit = storeCredit
                -- TODO: Get an export of the latest member numbers from stonedge?
                , customerMemberNumber = ""
                , customerEncryptedPassword = ""
                , customerAuthToken = token
                , customerStripeId = Nothing
                , customerIsAdmin = email == "gardens@southernexposure.com"
                }
        makeCustomer _ _ = error "Invalid arguments to makeCustomer."

-- | Generate a Map from MySQL Customer IDs to Store Credit Amounts.
getStoreCreditMap :: MySQLConn -> IO (IntMap.IntMap Cents)
getStoreCreditMap mysql = do
    customersAndAmounts <- mysqlQuery mysql
        $ "SELECT customer_id, amount "
        <> "FROM coupon_gv_customer "
        <> "WHERE amount > 0"
    return $ foldl updateCreditMap IntMap.empty customersAndAmounts
    where
        updateCreditMap m [MySQLInt32 customerId, MySQLDecimal amount] =
            IntMap.insert (fromIntegral customerId) (dollarsToCents amount) m
        updateCreditMap _ _ = error "Invalid arguments to updateCreditMap."


-- | Build the Shipping Addresses for Customers.
makeAddresses :: MySQLConn -> IO [Address]
makeAddresses mysql = do
    customersAndAddresses <- mysqlQuery mysql
        $ "SELECT a.address_book_id, a.entry_firstname, a.entry_lastname,"
        <> "      a.entry_company, a.entry_street_address, a.entry_suburb,"
        <> "      a.entry_postcode, a.entry_city, a.entry_state,"
        <> "      z.zone_name, co.countries_iso_code_2, c.customers_id,"
        <> "      c.customers_default_address_id "
        <> "FROM address_book AS a "
        <> "RIGHT JOIN customers AS c "
        <> "    ON c.customers_id=a.customers_id "
        <> "LEFT JOIN zones AS z "
        <> "    ON a.entry_zone_id=z.zone_id "
        <> "RIGHT JOIN countries as co "
        <> "    ON entry_country_id=co.countries_id "
        <> "WHERE a.address_book_id IS NOT NULL"
    mapM makeAddress customersAndAddresses
    where
    makeAddress
         [ MySQLInt32 addressId, MySQLText firstName, MySQLText lastName
         , MySQLText companyName, MySQLText street, nullableAddressTwo
         , MySQLText zipCode, MySQLText city, MySQLText state
         , nullableZoneName, MySQLText rawCountryCode, MySQLInt32 customerId
         , MySQLInt32 defaultAddress
         ] =
        let
            addressTwo_ =
                fromNullableText "" nullableAddressTwo
            addressTwo =
                if addressTwo_ == city then "" else addressTwo_
            zone =
                fromNullableText state nullableZoneName
            country =
                makeCountry zone rawCountryCode
            region =
                makeRegion zone country
        in
            return Address
                { addressFirstName = firstName
                , addressLastName = lastName
                , addressCompanyName = companyName
                , addressAddressOne = street
                , addressAddressTwo = addressTwo
                , addressCity = city
                , addressState = region
                , addressZipCode = zipCode
                , addressCountry = country
                , addressIsDefault = defaultAddress == addressId
                , addressType = Shipping
                , addressCustomerId = toSqlKey $ fromIntegral customerId
                , addressIsActive = True
                }
    makeAddress _ = error "Invalid arguments to makeAddress."


makeCustomerCarts :: MySQLConn -> IO (OldIdMap [(Int, Natural)])
makeCustomerCarts mysql = do
    cartItems <- mysqlQuery mysql $
        "SELECT customers_id, products_id, customers_basket_quantity " <>
        "FROM customers_basket ORDER BY customers_id"
    return $ foldl updateCartMap IntMap.empty cartItems
    where
        parseProductId productId =
            case T.split (== ':') productId of
                [] ->
                    error "makeCustomerCarts: T.split returned an empty list!"
                integerPart : _ ->
                    read $ T.unpack integerPart
        updateCartMap m [MySQLInt32 customerId, MySQLText productsId, MySQLFloat quantity] =
            IntMap.insertWith (++) (fromIntegral customerId)
                [(parseProductId productsId, round quantity)] m
        updateCartMap _ _ = error "Invalid arguments to updateCartMap."


makeCoupons :: MySQLConn -> IO [Coupon]
makeCoupons mysql = do
    coupons <- mysqlQuery mysql $
        "SELECT c.coupon_id, coupon_type, coupon_code, " <>
        "       coupon_amount, coupon_minimum_order, coupon_expire_date, " <>
        "       uses_per_coupon, uses_per_user, coupon_active, " <>
        "       date_created, coupon_name, coupon_description " <>
        "FROM coupons AS c " <>
        "RIGHT JOIN coupons_description AS cd ON cd.coupon_id=c.coupon_id " <>
        "WHERE coupon_type <> \"G\""
    return $ map makeCoupon coupons
    where
    toUTC =
            localTimeToUTC $ hoursToTimeZone (-5)
    makeCoupon
         [ _, MySQLText type_, MySQLText code
         , MySQLDecimal amount, MySQLDecimal minOrder, MySQLDateTime expirationDate
         , MySQLInt32 usesPerCoupon, MySQLInt32 usesPerCustomer, MySQLText isActive
         , MySQLDateTime createdDate, MySQLText name, MySQLText description
         ] =
            Coupon
                { couponCode =
                    code
                , couponName =
                    name
                , couponDescription =
                    description
                , couponIsActive =
                    isActive == "Y"
                , couponDiscount =
                    case type_ of
                        "S" ->
                            FreeShipping
                        "P" ->
                            PercentageDiscount $ floor amount
                        "F" ->
                            FlatDiscount . Cents . floor $ amount * 100
                        _ ->
                            error $ "makeCoupons encountered unexpected coupon type: "
                                ++ T.unpack type_
                , couponMinimumOrder =
                    Cents . floor $ 100 * minOrder
                , couponExpirationDate =
                    toUTC expirationDate
                , couponTotalUses =
                    fromIntegral usesPerCoupon
                , couponUsesPerCustomer =
                    fromIntegral usesPerCustomer
                , couponCreatedAt =
                    toUTC createdDate
                }
    makeCoupon _ = error "Invalid arguments to makeCoupon."


makeOrders :: MySQLConn -> IO [(Order, [OrderProduct], [OrderLineItem], Address, Maybe Address, T.Text, Customer)]
makeOrders mysql = do
    orders <- mysqlQuery mysql $
        "SELECT o.orders_id, customers_id, customers_email_address, " <>
        -- Customer Address
        "       customers_name, customers_company, " <>
        "       customers_street_address, customers_suburb, customers_city, " <>
        "       customers_postcode, customers_state, customers_country, " <>
        "       cc.countries_iso_code_2 AS customers_country_code, " <>
        -- Shipping Address
        "       delivery_name, delivery_company, " <>
        "       delivery_street_address, delivery_suburb, delivery_city, " <>
        "       delivery_postcode, delivery_state, delivery_country, " <>
        "       dc.countries_iso_code_2 AS delivery_country_code, " <>
        -- Billing Address
        "       billing_name, billing_company, billing_street_address, " <>
        "       billing_suburb, billing_city, billing_postcode, " <>
        "       billing_state, billing_country, " <>
        "       bc.countries_iso_code_2 AS billing_country_code, " <>
        "       coupon_code, date_purchased, orders_status, " <>
        "       order_total, order_tax, osh.comments AS comment " <>
        "FROM orders AS o " <>
        "LEFT JOIN countries AS cc on cc.countries_name=customers_country " <>
        "LEFT JOIN countries AS dc on dc.countries_name=delivery_country " <>
        "LEFT JOIN countries AS bc on bc.countries_name=billing_country " <>
        "LEFT JOIN (SELECT * FROM orders_status_history WHERE customer_notified != '-1') " <>
        "   AS osh ON osh.orders_id=o.orders_id"
    mapM makeOrder orders
  where
    makeOrder :: [MySQLValue] -> IO (Order, [OrderProduct], [OrderLineItem], Address, Maybe Address, T.Text, Customer)
    makeOrder
        [ MySQLInt32 orderId, MySQLInt32 customerId, MySQLText customerEmail
        -- Customer Address
        , MySQLText customerName, nullableCustomerCompany
        , MySQLText customerStreet, nullableCustomerStreetTwo, MySQLText customerCity
        , MySQLText customerZip, nullableCustomerState, MySQLText customerCountry
        , nullableCustomerCountryCode
        -- Shipping Address
        , MySQLText shippingName, nullableShippingCompany
        , MySQLText shippingStreet, nullableShippingStreetTwo, MySQLText shippingCity
        , MySQLText shippingZip, nullableShippingState, MySQLText shippingCountry
        , nullableShippingCountryCode
        -- Billing Address
        , MySQLText billingName, nullableBillingCompany, MySQLText billingStreet
        , nullableBillingStreetTwo, MySQLText billingCity, MySQLText billingZip
        , nullableBillingState, MySQLText billingCountry
        , nullableBillingCountryCode
        , MySQLText couponCode, MySQLDateTime purchaseDate, MySQLInt32 rawOrderStatus
        , MySQLDecimal orderTotal, MySQLDecimal orderTax, nullableCustomerComment
        ] = do
            createdAt <- convertLocalTimeToUTC purchaseDate
            shippingAddress <-
                if isAddressEmpty shippingName shippingStreet &&
                    isAddressEmpty customerName customerStreet then
                        -- A single order(#15567) has no address
                        -- details at all, so use the customer's
                        -- address_book entry.
                        makeAddressFromCustomer customerId
                else if isAddressEmpty shippingName shippingStreet then
                    return $ makeOrderAddress customerName
                        nullableCustomerCompany customerStreet
                        nullableCustomerStreetTwo customerCity customerZip
                        nullableCustomerState customerCountry nullableCustomerCountryCode
                        customerId Shipping
                else
                    return $ makeOrderAddress shippingName
                        nullableShippingCompany shippingStreet
                        nullableShippingStreetTwo shippingCity shippingZip
                        nullableShippingState shippingCountry nullableShippingCountryCode
                        customerId Shipping
            billingAddress <-
                if isAddressEmpty billingName billingStreet then
                    return Nothing
                else do
                    eitherAddr <- tryAny . evaluate
                        $ makeOrderAddress billingName
                            nullableBillingCompany billingStreet
                            nullableBillingStreetTwo billingCity billingZip
                            nullableBillingState billingCountry nullableBillingCountryCode
                            customerId Billing
                    let handleError e = print e >> return (Just shippingAddress)
                    either handleError (return . Just) eitherAddr
            ((taxDescription, taxLineTotal), lineItems) <- makeLineItems orderId
            let order = Order
                    { orderCustomerId = toSqlKey $ fromIntegral customerId
                    , orderStatus = getOrderStatus rawOrderStatus
                    , orderBillingAddressId = Nothing
                    , orderShippingAddressId = toSqlKey 0
                    , orderTaxDescription = taxDescription
                    , orderCustomerComment = fromNullableText "" nullableCustomerComment
                    , orderCouponId = Nothing
                    , orderStripeChargeId = Nothing
                    , orderCreatedAt = createdAt
                    }
            (orderProducts, orderItems) <- adjustTotal orderId orderTotal lineItems
                .   adjustTax (dollarsToCents orderTax)
                <$> makeOrderProducts orderId
            validateTaxAmounts orderId orderTax taxLineTotal orderProducts
            validateOrderTotal orderId orderTotal orderItems orderProducts
            token <- generateToken
            let customer = Customer
                    { customerEmail = customerEmail
                    , customerStoreCredit = 0
                    , customerMemberNumber = ""
                    , customerEncryptedPassword = ""
                    , customerAuthToken = token
                    , customerStripeId = Nothing
                    , customerIsAdmin = False
                    }
            return (order, orderProducts, orderItems, shippingAddress, billingAddress, couponCode, customer)
    makeOrder args = error
        $ "Invalid arguments to makeOrder:\n\t"
            <> concatMap (\a -> show a <> "\n\t") args
    getOrderStatus :: Int32 -> OrderStatus
    getOrderStatus status =
        case status of
            1 -> OrderReceived
            2 -> Processing
            3 -> Delivered
            4 -> Processing
            _ -> error $ "Unexpected Order Status: " <> show status
    isAddressEmpty :: T.Text -> T.Text -> Bool
    isAddressEmpty name street =
        all (T.null . T.strip) [name, street]
    makeOrderAddress name nullableCompany street nullableStreetTwo city zipCode nullableState country nullableCountryCode customerId type_ =
        let company = fromNullableText "" nullableCompany
            addressTwo = fromNullableText "" nullableStreetTwo
            state = T.strip $ fromNullableText "" nullableState
            (firstName, lastName) = splitName name
            parsedCountry = case nullableCountryCode of
                MySQLText rawCode ->
                    makeCountry state rawCode
                _ ->
                    case country of
                        "United States Minor Outlying Isl" ->
                            if isJust (StateCodes.fromMText $ T.toUpper state)
                                || isJust (StateCodes.fromMName $ T.toTitle state) then
                                Country CountryCodes.US
                            else
                                error $ "makeOrderAddress: Null Country Code for "
                                    <> intercalate "\n\t" (map T.unpack
                                        [ name
                                        , company
                                        , street
                                        , addressTwo
                                        , city
                                        , zipCode
                                        , state
                                        , country
                                        , T.pack $ show customerId
                                        , T.pack $ show type_
                                        ])

                        _ ->
                            error $ "makeOrderAddress: Null Country Code for "
                                <> intercalate "\n\t" (map T.unpack
                                    [ name
                                    , company
                                    , street
                                    , addressTwo
                                    , city
                                    , zipCode
                                    , state
                                    , country
                                    , T.pack $ show customerId
                                    , T.pack $ show type_
                                    ])
            region = makeRegion state parsedCountry
        in  Address
                { addressFirstName = firstName
                , addressLastName = lastName
                , addressCompanyName = if company /= name then company else ""
                , addressAddressOne = street
                , addressAddressTwo = addressTwo
                , addressCity = city
                , addressState = region
                , addressZipCode = zipCode
                , addressCountry = parsedCountry
                , addressIsDefault = True
                , addressType = type_
                , addressCustomerId = toSqlKey $ fromIntegral customerId
                , addressIsActive = True
                }
    splitName :: T.Text -> (T.Text, T.Text)
    splitName fullName =
        let split = T.words fullName
            wordCount = length split
        in if wordCount == 1 then
            (fullName, "")
           else
            ( T.unwords $ take (wordCount - 1) split
            , T.unwords $ drop (wordCount - 1) split
            )
    makeAddressFromCustomer :: Int32 -> IO Address
    makeAddressFromCustomer customerId = do
        query <- prepareStmt mysql . Query $
            "SELECT a.entry_firstname, a.entry_lastname,"
            <> "      a.entry_company, a.entry_street_address, a.entry_suburb,"
            <> "      a.entry_postcode, a.entry_city, a.entry_state,"
            <> "      z.zone_name, co.countries_iso_code_2 "
            <> "FROM address_book AS a "
            <> "LEFT JOIN zones AS z "
            <> "    ON a.entry_zone_id=z.zone_id "
            <> "RIGHT JOIN countries as co "
            <> "    ON entry_country_id=co.countries_id "
            <> "WHERE a.customers_id=?"
        maybeAddress <- fmap listToMaybe $ queryStmt mysql query [MySQLInt32 customerId]
            >>= Streams.toList . snd
        case maybeAddress of
            Nothing ->
                error $ "makeAddressFromCustomer: No address_book entries for customer " <> show customerId
            Just [ MySQLText firstName, MySQLText lastName
                 , MySQLText companyName, MySQLText street, nullableAddressTwo
                 , MySQLText zipCode, MySQLText city, MySQLText state
                 , nullableZoneName, MySQLText rawCountryCode
                 ] ->
                let addressTwo = fromNullableText "" nullableAddressTwo
                    zone = fromNullableText state nullableZoneName
                    country = makeCountry zone rawCountryCode
                in  return Address
                        { addressFirstName = firstName
                        , addressLastName = lastName
                        , addressCompanyName = companyName
                        , addressAddressOne = street
                        , addressAddressTwo = if addressTwo == city then "" else addressTwo
                        , addressCity = city
                        , addressState = makeRegion zone country
                        , addressZipCode = zipCode
                        , addressCountry = country
                        , addressIsDefault = True
                        , addressType = Shipping
                        , addressCustomerId = toSqlKey $ fromIntegral customerId
                        , addressIsActive = True
                        }
            _ ->
                error "makeAddressFromCustomer: Unexpected arguments from query."
    makeLineItems :: Int32 -> IO ((T.Text, Maybe Scientific), [OrderLineItem])
    makeLineItems orderId = do
        lineQuery <- prepareStmt mysql $ Query
            "SELECT title, value, class FROM orders_total WHERE orders_id=?"
        rawLineItems <- queryStmt mysql lineQuery [MySQLInt32 orderId]
            >>= Streams.toList . snd
        closeStmt mysql lineQuery
        return $ foldl makeLineItem (("", Nothing), []) rawLineItems
    makeLineItem
        :: ((T.Text, Maybe Scientific), [OrderLineItem])
        -> [MySQLValue]
        -> ((T.Text, Maybe Scientific), [OrderLineItem])
    makeLineItem (taxInfo, items) [MySQLText title, MySQLDecimal dollars, MySQLText lineType] =
        let cents = dollarsToCents dollars
            make type_ = (taxInfo, OrderLineItem (toSqlKey 0) type_ title cents : items)
            discard = (taxInfo, items)
        in  case lineType of
            "ot_total" ->
                discard -- TODO: check against order table's value?
            "ot_subtotal" ->
                discard
            "ot_shipping" ->
                make ShippingLine
            "ot_tax" ->
                ((title, Just dollars), items)
            "ot_coupon" ->
                ( taxInfo
                , OrderLineItem (toSqlKey 0) CouponDiscountLine
                    (getCouponCode title) cents : items
                )
            "ot_fallspringfee" ->
                make SurchargeLine
            "ot_gv" ->
                make StoreCreditLine
            "ot_intlsur" ->
                make SurchargeLine
            "ot_member_number" ->
                make MemberDiscountLine
            "ot_priority_handling" ->
                make PriorityShippingLine
            "ot_sc" ->
                make StoreCreditLine
            "ot_sweetpotatofee" ->
                make SurchargeLine
            _ -> error $
                "Unexpected order_total class: " <> T.unpack lineType
    makeLineItem _ _ = error "Invalid arguments to makeLineItem."
    getCouponCode :: T.Text -> T.Text
    getCouponCode couponLineTitle =
        case T.split (== ':') couponLineTitle of
            [_, code, _] -> code
            _ -> error $ "getCouponCode: Unexpected coupon line title: "
                    <> T.unpack couponLineTitle
    makeOrderProducts :: Int32 -> IO [OrderProduct]
    makeOrderProducts orderId = do
        query <- prepareStmt mysql . Query $
            "SELECT products_id, final_price, products_tax, products_quantity " <>
            "FROM orders_products WHERE orders_id=?"
        rawProducts <- queryStmt mysql query [MySQLInt32 orderId]
            >>= Streams.toList . snd
        closeStmt mysql query
        return $
            nubByWith (\p1 p2 -> orderProductProductVariantId p1 == orderProductProductVariantId p2)
                (\p1 p2 ->
                    if orderProductPrice p1 /= orderProductPrice p2 then
                        error $ "Duplicate products with different prices:\n\t"
                            <> show p1 <> "\n\t" <> show p2
                    else
                        p1
                            { orderProductQuantity =
                                orderProductQuantity p1 + orderProductQuantity p2
                            , orderProductTax =
                                orderProductTax p1 + orderProductTax p2
                            }
                )
            $ map makeOrderProduct rawProducts
    makeOrderProduct :: [MySQLValue] -> OrderProduct
    makeOrderProduct [ MySQLInt32 productId, MySQLDecimal pricePerProduct
                     , MySQLDecimal taxPercent, MySQLFloat quantity
                     ] =
        let taxRateRational = round (taxPercent * 100) % 10000
            priceRational = round (100 * pricePerProduct) % 1
            taxAmount =
                Cents . round $ taxRateRational * priceRational * (floor quantity % 1 :: Rational)
        in  OrderProduct
            (toSqlKey 0)
            (toSqlKey $ fromIntegral productId)
            (round quantity)
            (dollarsToCents pricePerProduct)
            taxAmount
    makeOrderProduct _ = error "Invalid arguments to makeOrderProduct."
    -- These adjustments are necessary because ZenCart rounds at the end of
    -- order summation, while we round at each step.
    adjustTax :: Cents -> [OrderProduct] -> [OrderProduct]
    adjustTax orderTax products =
        let productTax = getOrderTax products
            taxDifference = integerCents orderTax - integerCents productTax
        in
            if taxDifference == 0 then
                products
            else
                adjustProductTax taxDifference products
    adjustProductTax :: Integer -> [OrderProduct] -> [OrderProduct]
    adjustProductTax adjustment products =
        if adjustment == 0 then products else case products of
            p : ps ->
                if (adjustment < 0 && integerCents (orderProductTax p) > abs adjustment) || adjustment > 0 then
                    p { orderProductTax = Cents . fromInteger
                            $ integerCents (orderProductTax p) + adjustment
                      } : ps
                else
                    p { orderProductTax = 0 }
                        : adjustProductTax (adjustment + integerCents (orderProductTax p)) ps
            _ ->
                error "adjustTax: Tried adjusting product tax with no products"
    adjustTotal :: Int32 -> Scientific -> [OrderLineItem] -> [OrderProduct] -> ([OrderProduct], [OrderLineItem])
    adjustTotal orderId orderTotal lineItems products =
        let calculatedTotal =
                getOrderTotal lineItems products
            intOrderTotal =
                integerCents $ dollarsToCents orderTotal
            adjustment =
                intOrderTotal - integerCents calculatedTotal
            makeSurcharge adj =
                OrderLineItem (toSqlKey 0) SurchargeLine "Order Adjustment"
                    (Cents $ fromInteger adj)
            makeCredit adj =
                OrderLineItem (toSqlKey 0) StoreCreditLine "Order Adjustment"
                    (Cents $ fromInteger $ abs adj)
        in
        if adjustment == 0 then
            (products, lineItems)
        else if adjustment > 0 then
            (products, makeSurcharge adjustment : lineItems)
        else
            let adjustedProducts = adjustProductPrice orderId adjustment products
                remainingAdjustment = intOrderTotal - integerCents (getOrderTotal lineItems adjustedProducts)
            in
            if remainingAdjustment == 0 then
                (adjustedProducts, lineItems)
            else if remainingAdjustment > 0 then
                (adjustedProducts, makeSurcharge remainingAdjustment : lineItems)
            else
                case partition (\i -> orderLineItemType i == StoreCreditLine) lineItems of
                    ([], _) ->
                        (adjustedProducts, makeCredit remainingAdjustment : lineItems)
                    (sc:_, rest) ->
                        ( adjustedProducts
                        , sc {
                                orderLineItemAmount =
                                    orderLineItemAmount sc
                                        + Cents (fromInteger $ abs remainingAdjustment)
                            } : rest
                        )
    adjustProductPrice :: Int32 -> Integer -> [OrderProduct] -> [OrderProduct]
    adjustProductPrice orderId adjustment products =
        if adjustment == 0 then products else case products of
            p : ps ->
                let fractionalAdjustment = round $ adjustment % toInteger (orderProductQuantity p)
                    totalFractionalAdjustment = fractionalAdjustment * fromIntegral (orderProductQuantity p)
                in
                if (adjustment < 0 && intProductTotal p >= abs adjustment) || adjustment > 0 then
                    if orderProductQuantity p == 1 then
                        p { orderProductPrice = addIntCents adjustment (orderProductPrice p) } : ps
                    else
                        p { orderProductPrice = addIntCents fractionalAdjustment (orderProductPrice p) }
                            : adjustProductPrice orderId (adjustment - totalFractionalAdjustment)
                                ps
                else
                    p { orderProductPrice = 0 } : adjustProductPrice orderId (adjustment + intProductTotal p) ps
            [] ->
                products

    addIntCents :: Integer -> Cents -> Cents
    addIntCents i c = Cents . fromIntegral $ i + integerCents c

    intProductTotal :: OrderProduct -> Integer
    intProductTotal p = integerCents (orderProductPrice p) * toInteger (orderProductQuantity p)

    integerCents :: Cents -> Integer
    integerCents = toInteger . fromCents

    validateTaxAmounts :: Int32 -> Scientific -> Maybe Scientific -> [OrderProduct] -> IO ()
    validateTaxAmounts orderId orderTax maybeTaxLine products =
        if maybe True ((== dollarsToCents orderTax) . dollarsToCents) maybeTaxLine then
            let productTax = sum (map orderProductTax products) in
            if productTax == dollarsToCents orderTax then
                return ()
            else
                error $ "validateTaxAmounts: Mismatch between order & generated product's tax: "
                    <> show orderId <> "\n\tExpected: " <> show (dollarsToCents orderTax)
                    <> ", Got: " <> show productTax
                    <> "\n\t\t" <> intercalate "\n\t\t" (map show products)
        else
            putStrLn $ "validateTaxAmounts: Mismatch between order & orders_total tax: "
                <> show orderId
    validateOrderTotal :: Int32 -> Scientific -> [OrderLineItem] -> [OrderProduct] -> IO ()
    validateOrderTotal orderId orderTotal lineItems products =
        let calculatedTotal = getOrderTotal lineItems products
            intOrderTotal = dollarsToCents orderTotal
        in if calculatedTotal == intOrderTotal then
                return ()
            else
                error $ "validateOrderTotal: Mismatch between order & generated order: "
                    <> show orderId <> "\n\tExpected: " <> show (dollarsToCents orderTotal)
                    <> ", Got: " <> show calculatedTotal
                    <> "\n\t\t" <> intercalate "\n\t\t" (map show lineItems)
                    <> "\n\t\t" <> intercalate "\n\t\t" (map show products)




-- Persistent Model Saving Functions

insertCategories :: [(Int, Int, Category)] -> SqlWriteT IO (OldIdMap CategoryId)
insertCategories =
    foldM insertCategory IntMap.empty
    where insertCategory intMap (mysqlId, mysqlParentId, category) = do
            let maybeParentId =
                    IntMap.lookup mysqlParentId intMap
                category' =
                    category { categoryParentId = maybeParentId }
            categoryId <- insert category'
            return $ IntMap.insert mysqlId categoryId intMap


insertCategorySales :: [([Int], CategorySale)] -> OldIdMap CategoryId -> SqlWriteT IO ()
insertCategorySales sales categoryIdMap =
    mapM_ insertSale sales
    where
        insertSale (categoryIds, sale) =
            insert $ sale
                { categorySaleCategoryIds = fixIds categoryIds
                }
        fixIds ids =
            case ids of
                [] ->
                    []
                oldId : rest ->
                    case IntMap.lookup oldId categoryIdMap of
                        Nothing ->
                            error $ "Could not find old category ID: " <> show oldId
                        Just newId ->
                            newId : fixIds rest



insertProducts :: [(Int, Product)] -> OldIdMap CategoryId -> SqlWriteT IO ()
insertProducts products categoryIdMap =
    mapM_ insertProduct products
    where insertProduct (mysqlCategoryId, prod) = do
            let categoryIds =
                    maybeToList $ IntMap.lookup mysqlCategoryId categoryIdMap
                product' = prod { productCategoryIds = categoryIds }
            insert product'


insertVariants :: [(Int, T.Text, ProductVariant)] -> SqlWriteT IO (OldIdMap ProductVariantId)
insertVariants variantData = do
    existing <- foldM insertVariant IntMap.empty variantData
    withRecreated <- foldM insertRecreatedVariant existing recreatedProducts
    foldM insertDeletedVariants withRecreated deletedProducts
    where
        insertVariant intMap (oldProductId, baseSku, variant) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    lift (putStrLn $ "No product for: " ++ show variant)
                        >> return intMap
                Just (Entity prodId _) -> do
                    maybeExistingVariant <- getBy $ UniqueSku prodId $ productVariantSkuSuffix variant
                    let variantWithProduct = variant { productVariantProductId = prodId }
                    maybe
                        (insertIntoIdMap intMap oldProductId <$> insert variantWithProduct)
                        (handleExistingVariant intMap oldProductId variantWithProduct)
                        maybeExistingVariant
        handleExistingVariant intMap oldProductId variant (Entity variantId2 variant2)
            | not (productVariantIsActive variant) =
                let variant_ = variant { productVariantSkuSuffix = "X" } in
                insertIntoIdMap intMap oldProductId <$> insert variant_
            | productVariantIsActive variant && not (productVariantIsActive variant2) = do
                update variantId2 [ProductVariantSkuSuffix =. "X"]
                insertIntoIdMap intMap oldProductId <$> insert variant
            | otherwise =
                error $
                    "Two active variants with same SKU:\n\t"
                        <> show variant <> "\n\t" <> show variant2
        insertRecreatedVariant intMap (oldProductId, baseSku, skuSuffix) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    lift (putStrLn $ "No product for recreated product: "
                            <> show oldProductId)
                        >> return intMap
                Just (Entity prodId _) -> do
                    maybeVariant <- getBy $ UniqueSku prodId skuSuffix
                    case maybeVariant of
                        Nothing ->
                            lift (putStrLn $ "No variant for recreated product: "
                                    <> show oldProductId)
                                >> return intMap
                        Just (Entity varId _) ->
                            return $ insertIntoIdMap intMap oldProductId varId
        insertDeletedVariants intMap (oldProductId, newProduct, newVariant) = do
            productId <- insert newProduct
            let variant_ = newVariant { productVariantProductId = productId }
            insertIntoIdMap intMap oldProductId <$> insert variant_



insertAttributes :: [(T.Text, SeedAttribute)] -> SqlWriteT IO ()
insertAttributes =
    mapM_ insertAttribute
    where insertAttribute (baseSku, attribute) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    lift . putStrLn $ "No product for: " ++ show attribute
                Just (Entity prodId _) ->
                    void . insert $ attribute { seedAttributeProductId = prodId }


insertProductSales :: [(Int, ProductSale)] -> OldIdMap ProductVariantId -> SqlWriteT IO ()
insertProductSales sales variantIdMap =
    mapM_ insertSale sales
    where
        insertSale (oldVariantId, sale) =
            case IntMap.lookup oldVariantId variantIdMap of
                Nothing ->
                    lift . putStrLn $ "Could not find old variant ID: " <> show oldVariantId
                Just variantId ->
                    insert_ $ sale { productSaleProductVariantId = variantId }


insertPages :: [Page] -> SqlWriteT IO ()
insertPages = insertMany_


insertCustomers :: [([Int], Customer)] -> SqlWriteT IO (OldIdMap CustomerId)
insertCustomers =
    foldM insertCustomer IntMap.empty
    where insertCustomer intMap (oldCustomerIds, customer) = do
            newId <- insert customer
            return $ foldl (\newMap oldId -> insertIntoIdMap newMap oldId newId)
                intMap oldCustomerIds


-- | Replace the CustomerIds & insert the Addresses.
insertAddresses :: OldIdMap CustomerId -> [Address] -> SqlWriteT IO ()
insertAddresses customerMap =
    mapM_ insertAddress
    where insertAddress address@Address {..} =
            let
                oldCustomerId =
                    fromIntegral $ fromSqlKey addressCustomerId
            in
                case IntMap.lookup oldCustomerId customerMap of
                    Nothing ->
                        error $ "insertAddress: Could Not Find Customer "
                                ++ show oldCustomerId
                    Just customerId ->
                        insertUniqueAddress address { addressCustomerId = customerId }


insertCharges :: SqlWriteT IO ()
insertCharges = do
    void . insert $
        TaxRate "VA Sales Tax (5.3%)" 53 (Country CountryCodes.US)
            (Just $ USState StateCodes.VA) [] True
    getBy (UniqueCategorySlug "potatoes") >>=
        maybe (return ()) (\(Entity catId _) -> void . insert $
            Surcharge "Potato Fee" (Cents 200) (Cents 400) [catId] True)
    getBy (UniqueCategorySlug "sweet-potatoes") >>=
        maybe (return ()) (\(Entity catId _) -> void . insert $
            Surcharge "Sweet Potato Fee" (Cents 200) (Cents 400) [catId] True)
    fallCategoryIds <- selectKeysList
        [ CategorySlug <-.
            [ "garlic", "asiatic-turban", "elephant-garlic", "garlic-samplers"
            , "softneck-braidable", "perennial-onions", "ginseng-goldenseal"
            ]
        ] []
    void . insert $
        Surcharge "Fall Item Fee" (Cents 200) (Cents 400) fallCategoryIds True
    let priorityRate = PriorityShippingFee (Cents 500) 5
    priorityExcludedCategories <- do
        let categorySlugs =
                [ "potatoes"
                , "sweet-potatoes"
                , "garlic"
                , "perennial-onions"
                , "mushrooms"
                , "ginseng-goldenseal"
                ]
        map entityKey <$> selectList [CategorySlug <-. categorySlugs] []
    void . insert $
        ShippingMethod "Shipping to USA" [Country CountryCodes.US]
            [ Flat (Cents 0) (Cents 350)
            , Flat (Cents 3000) (Cents 450)
            , Flat (Cents 5000) (Cents 550)
            , Flat (Cents 12000) (Cents 650)
            , Percentage (Cents 50000000) 5
            ]
            priorityRate
            []
            priorityExcludedCategories
            True
            2
    void . insert $
        ShippingMethod "International Shipping"
            [Country CountryCodes.CA, Country CountryCodes.MX]
            [ Flat (Cents 0) (Cents 550)
            , Flat (Cents 3000) (Cents 750)
            , Flat (Cents 5000) (Cents 950)
            , Percentage (Cents 12000) 8
            , Percentage (Cents 50000000) 10
            ]
            priorityRate
            []
            priorityExcludedCategories
            True
            2
    getBy (UniqueCategorySlug "request-a-catalog") >>=
        maybe (return ()) (\(Entity catId _) -> void . insert $
            ShippingMethod "Free Shipping" [Country CountryCodes.US]
                [Flat (Cents 0) (Cents 0)]
                priorityRate
                [catId]
                priorityExcludedCategories
                True
                1
            )


insertCustomerCarts :: OldIdMap ProductVariantId
                    -> OldIdMap CustomerId
                    -> OldIdMap [(Int, Natural)]
                    -> SqlWriteT IO ()
insertCustomerCarts variantMap customerMap =
    IntMap.foldlWithKey (\acc k c -> acc >> newCart k c) (return ())
    where newCart oldCustomerId variantsAndQuantities =
            let
                maybeCustomerId = IntMap.lookup oldCustomerId customerMap
            in
                case maybeCustomerId of
                    Nothing ->
                        lift . putStrLn
                            $ "newCart: Could not find customer with ID "
                                <> show oldCustomerId
                    Just customerId -> do
                        cartId <- insertCart customerId
                        mapM_ (insertCartItem cartId) variantsAndQuantities
          insertCart customerId =
            entityKey <$> upsertBy (UniqueCustomerCart $ Just customerId)
                (Cart
                    { cartCustomerId = Just customerId
                    , cartSessionToken = Nothing
                    , cartExpirationTime = Nothing
                    })
                []
          insertCartItem cartId (oldVariantId, quantity) =
            let
                maybeVariantId = IntMap.lookup oldVariantId variantMap
            in
                case maybeVariantId of
                    Nothing ->
                        -- Product #1639 was deleted in ZenCart
                        unless (oldVariantId == 1639) $ lift . putStrLn
                            $ "insertCartItem: Could not find variant with ID "
                                <> show oldVariantId
                    Just variantId ->
                        void $ upsert
                            CartItem
                                { cartItemCartId = cartId
                                , cartItemProductVariantId = variantId
                                , cartItemQuantity = quantity
                                }
                            [ CartItemQuantity +=. quantity ]

deleteInactiveCartItems :: SqlWriteT IO ()
deleteInactiveCartItems = do
    inactiveVariants <- do
        inactiveProducts <- selectKeysList [ProductIsActive ==. False] []
        (<>)
            <$> selectKeysList [ProductVariantProductId <-. inactiveProducts] []
            <*> selectKeysList [ProductVariantIsActive ==. False] []
    deleteWhere [CartItemProductVariantId <-. inactiveVariants]



insertCoupons :: [Coupon] -> SqlWriteT IO ()
insertCoupons = insertMany_


insertOrders :: OldIdMap CustomerId -> OldIdMap ProductVariantId
    -> [(Order, [OrderProduct], [OrderLineItem], Address, Maybe Address, T.Text, Customer)]
    -> SqlWriteT IO ()
insertOrders customerMap variantMap =
    mapM_ insertOrder
  where
    insertOrder (order, products, lines, shippingAddr, maybeBillingAddr, couponCode, backupCustomer) = do
        maybeCoupon <- fmap entityKey <$> selectFirst [ CouponCode ==. couponCode ] []
        let oldCustomerId = fromIntegral . fromSqlKey $ orderCustomerId order
        customerId <- case IntMap.lookup oldCustomerId customerMap of
            Nothing -> do
                maybeCustomer <- getBy $ UniqueEmail $ customerEmail backupCustomer
                case maybeCustomer of
                    Just (Entity cId _) ->
                        return cId
                    Nothing -> do
                        lift . putStrLn
                            $ "Inserting Missing Customer using Orders Table data"
                        insert backupCustomer
            Just customerId ->
                return customerId
        let insertCustomerAddress addr =
                insertUniqueAddress addr { addressCustomerId = customerId }
        shippingId <- insertCustomerAddress shippingAddr
        maybeBillingId <- sequence $ insertCustomerAddress <$> maybeBillingAddr
        newOrderId <- insert order
            { orderCouponId = maybeCoupon
            , orderCustomerId = customerId
            , orderShippingAddressId = shippingId
            , orderBillingAddressId = maybeBillingId
            }
        mapM_ (insertLineItem newOrderId) lines
        mapM_ (insertOrderProduct newOrderId) products
    insertLineItem orderId item =
        insert_ item { orderLineItemOrderId = orderId }
    insertOrderProduct orderId product =
        let oldVariantId = fromIntegral . fromSqlKey $ orderProductProductVariantId product
        in  case IntMap.lookup oldVariantId variantMap of
                Nothing ->
                    lift . putStrLn
                        $ "insertOrderProduct: Could not find variant with ID "
                            <> show oldVariantId
                Just variantId ->
                    void $ upsertBy (UniqueOrderProduct orderId variantId)
                        product
                            { orderProductOrderId = orderId
                            , orderProductProductVariantId = variantId
                            }
                        [ OrderProductQuantity +=. orderProductQuantity product
                        , OrderProductTax +=. orderProductTax product
                        ]


-- Utils

mysqlQuery :: MySQLConn -> ByteString -> IO [[MySQLValue]]
mysqlQuery conn queryString =
    query_ conn (Query queryString) >>= Streams.toList . snd

insertIntoIdMap :: OldIdMap a -> IntMap.Key -> a -> OldIdMap a
insertIntoIdMap intMap key value =
    IntMap.insert key value intMap


dayToUTC :: Day -> IO UTCTime
dayToUTC day =
    convertLocalTimeToUTC $ LocalTime day midnight

fromNullableText :: T.Text -> MySQLValue -> T.Text
fromNullableText def val =
    case val of
        MySQLText text -> text
        _ -> def

convertLocalTimeToUTC :: LocalTime -> IO UTCTime
convertLocalTimeToUTC time = do
    timezone <- getCurrentTimeZone
    return $ localTimeToUTC timezone time


generateToken :: IO T.Text
generateToken =
    UUID.toText <$> UUID4.nextRandom

-- | Convert a Scientific dollar amount to Cents.
--
-- Note that if the thousandths digit of the dollar amount is `5`, we have
-- to force rounding up since the `round` function will round towards the
-- even integer.
dollarsToCents :: Scientific -> Cents
dollarsToCents dollars =
    let tenthCents = dollars * 1000 in
    if floor tenthCents `mod` 10 == (5 :: Integer) then
        Cents . ceiling $ dollars * 100
    else
        Cents . round $ dollars * 100

makeCountry :: T.Text -> T.Text -> Country
makeCountry state rawCountryCode =
    case state of
        "Federated States Of Micronesia" ->
            Country CountryCodes.FM
        "Marshall Islands" ->
            Country CountryCodes.MH
        _ ->
            case readMaybe (T.unpack rawCountryCode) of
                Just countryCode ->
                    Country countryCode
                Nothing ->
                    case rawCountryCode of
                        "AN" ->
                            Country CountryCodes.BQ
                        _ ->
                            error $ "Invalid Country Code: " ++ T.unpack rawCountryCode

makeRegion :: T.Text -> Country -> Region
makeRegion state country =
    case fromCountry country of
        CountryCodes.US ->
            let maybeCode =
                    StateCodes.fromMName (T.toTitle state)
                        <|> StateCodes.fromMName state
                        <|> StateCodes.fromMText (T.toUpper state)
                        <|> StateCodes.fromMText state
            in case maybeCode of
                Just stateCode ->
                    USState stateCode
                Nothing ->
                    case state of
                        "Armed Forces Africa" ->
                            USArmedForces AE
                        "Armed Forces Canada" ->
                            USArmedForces AE
                        "Armed Forces Europe" ->
                            USArmedForces AE
                        "Armed Forces Middle East" ->
                            USArmedForces AE
                        "Armed Forces Pacific" ->
                            USArmedForces AP
                        "Armed Forces Americas" ->
                            USArmedForces AA
                        "Virgin Islands" ->
                            USState StateCodes.VI
                        _ ->
                            error $ "Invalid State Code: " ++ T.unpack state
        CountryCodes.CA ->
            case CACodes.fromName state of
                Just provinceCode ->
                    CAProvince provinceCode
                Nothing ->
                    case state of
                        "Yukon Territory" ->
                            CAProvince CACodes.YT
                        "Newfoundland" ->
                            CAProvince CACodes.NL
                        _ ->
                            error $ "Invalid Canadian Province: " ++ T.unpack state
        _ ->
            CustomRegion state

-- TODO: This runs pretty slow, see if we can speed it up by reducing
-- queries. Maybe keep a map of Address -> ID as well as Customer->(Bool,Bool)
-- for defaults. Then we can just query the map for existence instead of
-- the database.
insertUniqueAddress :: Address -> SqlWriteT IO AddressId
insertUniqueAddress address@Address {..} = do
    existingAddress <- selectFirst
        [ AddressFirstName ==. addressFirstName
        , AddressLastName ==. addressLastName
        , AddressCompanyName ==. addressCompanyName
        , AddressAddressOne ==. addressAddressOne
        , AddressAddressTwo ==. addressAddressTwo
        , AddressCity ==. addressCity
        , AddressState ==. addressState
        , AddressZipCode ==. addressZipCode
        , AddressCountry ==. addressCountry
        , AddressType ==. addressType
        , AddressCustomerId ==. addressCustomerId
        ]
        []
    case existingAddress of
        Just addr ->
            return $ entityKey addr
        Nothing -> do
            alreadyDefaultForType <- isJust <$> selectFirst
                [ AddressType ==. addressType
                , AddressCustomerId ==. addressCustomerId
                , AddressIsDefault ==. True
                ]
                []
            insert $ address
                { addressIsDefault =
                    not alreadyDefaultForType && addressIsDefault
                }

-- | Reduce duplicates in a list with a custom equality function
-- & a function to merge duplicate items.
nubByWith :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a]
nubByWith eq merge xs = case xs of
    [] ->
        []
    x : rest ->
        let (matched, unmatched) = partition (eq x) rest
        in foldl merge x matched : nubByWith eq merge unmatched
