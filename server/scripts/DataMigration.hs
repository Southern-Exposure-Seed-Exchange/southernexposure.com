{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (foldM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Lazy (ByteString)
import Data.Char (isAlpha)
import Data.Int (Int32)
import Data.List (nubBy, partition)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Monoid ((<>))
import Data.Pool (destroyAllResources)
import Data.Scientific (Scientific)
import Data.Time
    ( LocalTime(..), Day, UTCTime, hoursToTimeZone, localTimeToUTC
    , getCurrentTimeZone, midnight
    )
import Database.MySQL.Base
    ( MySQLConn, Query(..), query_, close, MySQLValue(..), prepareStmt, queryStmt )
import Database.Persist
    ( (<-.), (+=.), (=.), Entity(..), Filter, getBy, insert, insertMany_, upsert
    , deleteWhere, selectKeysList, insert_, selectList, update
    )
import Database.Persist.Postgresql
    ( ConnectionPool, SqlWriteT, createPostgresqlPool, toSqlKey, fromSqlKey
    , runSqlPool
    )
import Numeric.Natural (Natural)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

import Models
import Models.Fields
import Utils

import qualified Data.CAProvinceCodes as CACodes
import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.IntMap as IntMap
import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified System.IO.Streams as Streams


main :: IO ()
main = do
    mysqlConn <- connectToMysql
    psqlConn <- connectToPostgres
    mysqlProducts <- makeProducts mysqlConn
    categories <- makeCategories mysqlConn
    categorySales <- makeCategorySales mysqlConn
    let products = mergeProducts
            $ map (\(_, catId, _, _, _, _, _, p) -> (catId, p)) mysqlProducts
        variants = makeVariants mysqlProducts
    productSales <- makeProductSales mysqlConn
    attributes <- makeSeedAttributes mysqlConn
    pages <- makePages mysqlConn
    customers <- makeCustomers mysqlConn
    addresses <- makeAddresses mysqlConn
    carts <- makeCustomerCarts mysqlConn
    coupons <- makeCoupons mysqlConn
    flip runSqlPool psqlConn $
        dropNewDatabaseRows >>
        insertCategories categories >>= \categoryMap ->
        insertCategorySales categorySales categoryMap >>
        insertProducts products categoryMap >>
        insertVariants variants >>= \variantMap ->
        insertAttributes attributes >>
        insertProductSales productSales variantMap >>
        insertPages pages >>
        insertCustomers customers >>= \customerMap ->
        insertAddresses customerMap addresses >>
        insertCharges >>
        insertCustomerCarts variantMap customerMap carts >>
        insertCoupons coupons
    close mysqlConn
    destroyAllResources psqlConn
  where
    mergeProducts :: [(Int, Product)] -> [(Int, Product)]
    mergeProducts xs = case xs of
        [] ->
            []
        p : rest ->
            let (matchingProducts, unmatched) =
                    partition (\(_, p2) -> productBaseSku (snd p) == productBaseSku p2) rest
            in foldl merge p matchingProducts : mergeProducts unmatched
      where
        merge (cat, prod) (_, nextProd) =
            ( cat
            , prod
                { productIsActive =
                    productIsActive prod || productIsActive nextProd
                }
            )




type OldIdMap a = IntMap.IntMap a


-- DB Utility Functions

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1


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
            let
                imgUrl =
                    case nullableImageUrl of
                        MySQLText str ->
                            str
                        _ ->
                            ""
            in
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
        <> "    products_image, products_status "
        <> "FROM products"
    mapM makeProduct products
    where
        makeProduct
         [ MySQLInt32 prodId, MySQLInt32 catId, MySQLDecimal prodPrice
         , MySQLFloat prodQty, MySQLFloat prodWeight, MySQLText prodSKU
         , MySQLText prodImg, MySQLInt8 prodStatus] = do
             -- TODO: Just use join query?
            queryString <- prepareStmt mysql . Query $
                "SELECT products_id, products_name, products_description "
                <> "FROM products_description WHERE products_id=?"
            (_, descriptionStream) <- queryStmt mysql queryString [MySQLInt32 prodId]
            [_, MySQLText dbName, MySQLText description] <- head <$> Streams.toList descriptionStream
            let name = if dbName == ""
                    then "Inactive Product - " <> T.pack (show prodId)
                    else dbName
            let (baseSku, skuSuffix) = splitSku prodSKU
                isActive = prodStatus == 1
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
                        }
                   )
        makeProduct _ = error "Invalid arguments to makeProduct."


makeVariants :: [(Int32, Int, T.Text, Scientific, Float, Float, Bool, Product)] -> [(Int, T.Text, ProductVariant)]
makeVariants =
    map makeVariant
    where makeVariant (productId, _, suffix, price, qty, weight, isActive, prod) =
            (fromIntegral productId, productBaseSku prod,) $
                ProductVariant
                    (toSqlKey 0)
                    (T.toUpper suffix)
                    (Cents . round $ 100 * price)
                    (floor qty)
                    (Milligrams . round $ 1000 * weight)
                    isActive


makeSeedAttributes :: MySQLConn -> IO [(T.Text, SeedAttribute)]
makeSeedAttributes mysql = do
    attributes <- mysqlQuery mysql $
        "SELECT p.products_id, products_model, is_eco,"
        <> "    is_organic, is_heirloom, is_southern "
        <> "FROM sese_products_icons as i "
        <> "RIGHT JOIN products AS p "
        <> "ON p.products_id=i.products_id "
        <> "WHERE p.products_status=1"
    nubBy (\a1 a2 -> fst a1 == fst a2) <$> mapM toData attributes
    where toData [ MySQLInt32 _, MySQLText prodSku, MySQLInt8 isEco
                 , MySQLInt8 isOrg, MySQLInt8 isHeir, MySQLInt8 isRegion
                 ] =
            return . (fst $ splitSku prodSku,) $
                SeedAttribute (toSqlKey 0) (toBool isOrg) (toBool isHeir)
                    (toBool isEco) (toBool isRegion)
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
                , ProductSale (Cents . round $ 100 * salePrice) (toSqlKey 0)
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


makeCustomers :: MySQLConn -> IO [(Int, Customer)]
makeCustomers mysql = do
    storeCreditMap <- getStoreCreditMap mysql
    customers <- mysqlQuery mysql
        $ "SELECT c.customers_id, c.customers_email_address "
        <> "FROM customers AS c "
        <> "WHERE c.COWOA_account=0"
    mapM (makeCustomer storeCreditMap) customers
    where
        generateToken = UUID.toText <$> UUID4.nextRandom
        makeCustomer creditMap [MySQLInt32 customerId, MySQLText email ] = do
            let storeCredit = fromMaybe 0
                    $ IntMap.lookup (fromIntegral customerId) creditMap
            token <- generateToken
            return . (fromIntegral customerId,) $ Customer
                { customerEmail = email
                , customerStoreCredit = storeCredit
                -- TODO: Get an export of the latest member numbers from stonedge
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
            IntMap.insert (fromIntegral customerId) (Cents . round $ 100 * amount) m
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
        <> "FROM customers AS c "
        <> "RIGHT JOIN address_book AS a "
        <> "    ON c.customers_default_address_id=a.address_book_id "
        <> "LEFT JOIN zones AS z "
        <> "    ON a.entry_zone_id=z.zone_id "
        <> "RIGHT JOIN countries as co "
        <> "    ON entry_country_id=co.countries_id "
        <> "WHERE c.COWOA_account=0"
    mapM makeAddress customersAndAddresses
    where
    makeAddress
         [ MySQLInt32 addressId, MySQLText firstName, MySQLText lastName
         , MySQLText companyName, MySQLText street, MySQLText addressTwo
         , MySQLText zipCode, MySQLText city, MySQLText state
         , nullableZoneName, MySQLText rawCountryCode, MySQLInt32 customerId
         , MySQLInt32 defaultAddress
         ] =
        let
            zone =
                case nullableZoneName of
                    MySQLText text ->
                        text
                    _ ->
                        state
            country =
                case zone of
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
            region =
                case fromCountry country of
                    CountryCodes.US ->
                        case StateCodes.fromMName zone of
                            Just stateCode ->
                                USState stateCode
                            Nothing ->
                                case zone of
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
                                        error $ "Invalid State Code: " ++ T.unpack zone
                    CountryCodes.CA ->
                        case CACodes.fromName zone of
                            Just provinceCode ->
                                CAProvince provinceCode
                            Nothing ->
                                case zone of
                                    "Yukon Territory" ->
                                        CAProvince CACodes.YT
                                    "Newfoundland" ->
                                        CAProvince CACodes.NL
                                    _ ->
                                        error $ "Invalid Canadian Province: " ++ T.unpack zone
                    _ ->
                        CustomRegion zone
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
                , couponCreatedDate =
                    toUTC createdDate
                }
    makeCoupon _ = error "Invalid arguments to makeCoupon."


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
insertVariants =
    foldM insertVariant IntMap.empty
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


insertCustomers :: [(Int, Customer)] -> SqlWriteT IO (OldIdMap CustomerId)
insertCustomers =
    foldM insertCustomer IntMap.empty
    where insertCustomer intMap (oldCustomerId, customer) =
            insertIntoIdMap intMap oldCustomerId <$> insert customer


-- | Replace the CustomerIds & insert the Addresses.
insertAddresses :: OldIdMap CustomerId -> [Address] -> SqlWriteT IO ()
insertAddresses customerMap =
    mapM_ insertAddress
    where insertAddress address =
            let
                oldCustomerId =
                    fromIntegral . fromSqlKey $ addressCustomerId address
            in
                case IntMap.lookup oldCustomerId customerMap of
                    Nothing ->
                        error $ "insertAddress: Could Not Find Customer "
                                ++ show oldCustomerId
                    Just customerId ->
                        insert_ $ address { addressCustomerId = customerId }


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
                        return ()
                    Just customerId -> do
                        cartId <- insertCart customerId
                        mapM_ (insertCartItem cartId) variantsAndQuantities
          insertCart customerId =
            insert Cart
                { cartCustomerId = Just customerId
                , cartSessionToken = Nothing
                , cartExpirationTime = Nothing
                }
          insertCartItem cartId (oldVariantId, quantity) =
            let
                maybeVariantId = IntMap.lookup oldVariantId variantMap
            in
                case maybeVariantId of
                    Nothing ->
                        return ()
                    Just variantId ->
                        void $ upsert
                            CartItem
                                { cartItemCartId = cartId
                                , cartItemProductVariantId = variantId
                                , cartItemQuantity = quantity
                                }
                            [ CartItemQuantity +=. quantity ]


insertCoupons :: [Coupon] -> SqlWriteT IO ()
insertCoupons = insertMany_


-- Utils

mysqlQuery :: MySQLConn -> ByteString -> IO [[MySQLValue]]
mysqlQuery conn queryString =
    query_ conn (Query queryString) >>= Streams.toList . snd

insertIntoIdMap :: OldIdMap a -> IntMap.Key -> a -> OldIdMap a
insertIntoIdMap intMap key value =
    IntMap.insert key value intMap


dayToUTC :: Day -> IO UTCTime
dayToUTC day = do
    timezone <- getCurrentTimeZone
    return . localTimeToUTC timezone $ LocalTime day midnight
