{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad (forM, foldM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (runNoLoggingT)
import Data.Char (isAlpha)
import Data.Int (Int32)
import Data.List (nubBy)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Pool (destroyAllResources)
import Data.Scientific (Scientific)
import Database.MySQL.Base
    ( MySQLConn, Query(..), query_, close, MySQLValue(..), prepareStmt, queryStmt )
import Database.Persist
    (Entity(..), Filter, getBy, insert, insertMany_, deleteWhere)
import Database.Persist.Postgresql
    (ConnectionPool, SqlWriteT, createPostgresqlPool, toSqlKey, runSqlPool)
import System.FilePath (takeFileName)

import Models
import Models.Fields
import Utils

import qualified Data.IntMap as IntMap
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
    let products = nubBy (\(_, p1) (_, p2) -> productBaseSku p1 == productBaseSku p2)
            $ map (\(_, catId, _, _, _, _, p) -> (catId, p)) mysqlProducts
        variants = makeVariants mysqlProducts
    attributes <- makeSeedAttributes mysqlConn
    pages <- makePages mysqlConn
    customers <- makeCustomers mysqlConn
    flip runSqlPool psqlConn
        $ dropNewDatabaseRows
        >> insertCategories categories
        >>= insertProducts products
        >> insertVariants variants
        >> insertAttributes attributes
        >> insertPages pages
        >> insertCustomers customers
    close mysqlConn
    destroyAllResources psqlConn


-- DB Utility Functions

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 1


dropNewDatabaseRows :: SqlWriteT IO ()
dropNewDatabaseRows =
        deleteWhere ([] :: [Filter SeedAttribute])
        >> deleteWhere ([] :: [Filter ProductVariant])
        >> deleteWhere ([] :: [Filter Product])
        >> deleteWhere ([] :: [Filter Category])
        >> deleteWhere ([] :: [Filter Page])
        >> deleteWhere ([] :: [Filter Customer])


-- MySQL -> Persistent Functions

makeCategories :: MySQLConn -> IO [(Int, Int, Category)]
makeCategories mysql = do
    (_, categoryStream) <- query_ mysql . Query $
        "SELECT c.categories_id, categories_image, parent_id, sort_order,"
        <> "    categories_name, categories_description "
        <> "FROM categories as c "
        <> "LEFT JOIN categories_description as cd ON c.categories_id=cd.categories_id "
        <> "WHERE categories_status=1 "
        <> "ORDER BY parent_id ASC"
    cs <- Streams.toList categoryStream
    mapM toData cs
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


makeProducts :: MySQLConn -> IO [(Int32, Int, T.Text, Scientific, Float, Float, Product)]
makeProducts mysql = do
    (_, productStream) <- query_ mysql
        "SELECT products_id, master_categories_id, products_price, products_quantity, products_weight, products_model, products_image, products_status FROM products WHERE products_status=1"
    ps <- Streams.toList productStream
    forM ps $
        \[MySQLInt32 prodId, MySQLInt32 catId, MySQLDecimal prodPrice, MySQLFloat prodQty, MySQLFloat prodWeight, MySQLText prodSKU, MySQLText prodImg, _] -> do
            queryString <- prepareStmt mysql
                "SELECT products_id, products_name, products_description FROM products_description WHERE products_id=?"
            (_, descriptionStream) <- queryStmt mysql queryString [MySQLInt32 prodId]
            [_, MySQLText name, MySQLText description] <- head <$> Streams.toList descriptionStream
            _ <- return prodId
            _ <- return prodQty
            let (baseSku, skuSuffix) = splitSku prodSKU
            return (prodId, fromIntegral catId, skuSuffix, prodPrice, prodQty, prodWeight, Product name (slugify name) [] baseSku "" description (T.pack . takeFileName $ T.unpack prodImg))


makeVariants :: [(Int32, Int, T.Text, Scientific, Float, Float, Product)] -> [(T.Text, ProductVariant)]
makeVariants =
    map makeVariant
    where makeVariant (_, _, suffix, price, qty, weight, prod) =
            (,) (productBaseSku prod) $
            ProductVariant
                (toSqlKey 0)
                suffix
                (Cents . round $ 100 * price)
                (floor qty)
                (Milligrams . round $ 1000 * weight)
                True


makeSeedAttributes :: MySQLConn -> IO [(T.Text, SeedAttribute)]
makeSeedAttributes mysql = do
    (_, attributeStream) <- query_ mysql
        "SELECT p.products_id, products_model, is_eco, is_organic, is_heirloom, is_southern FROM sese_products_icons as i RIGHT JOIN products AS p ON p.products_id=i.products_id WHERE p.products_status=1"
    attrs <- Streams.toList attributeStream
    return . nubBy (\a1 a2 -> fst a1 == fst a2) . flip map attrs $
        \[MySQLInt32 _, MySQLText prodSku, MySQLInt8 isEco, MySQLInt8 isOrg, MySQLInt8 isHeir, MySQLInt8 isRegion] ->
            let
                (baseSku, _) = splitSku prodSku
            in
                (,) baseSku $
                    SeedAttribute (toSqlKey 0) (toBool isOrg) (toBool isHeir)
                        (toBool isEco) (toBool isRegion)
            where toBool = (==) 1



splitSku :: T.Text -> (T.Text, T.Text)
splitSku fullSku =
    case T.split isAlpha fullSku of
         [baseSku, ""] ->
            case T.stripPrefix baseSku fullSku of
                Just skuSuffix ->
                    (baseSku, skuSuffix)
                Nothing ->
                    (fullSku, "")
         _ ->
            (fullSku, "")


makePages :: MySQLConn -> IO [Page]
makePages mysql = do
    pages <- Streams.toList . snd
        =<< (query_ mysql . Query
            $ "SELECT pages_title, pages_html_text"
            <> "    FROM ezpages WHERE pages_html_text <> \"\"")
    return $ flip map pages $ \[MySQLText name, MySQLText content] ->
        Page name (slugify name) content

makeCustomers :: MySQLConn -> IO [Customer]
makeCustomers mysql = do
    customers <- Streams.toList . snd
        =<< (query_ mysql . Query
                $ "SELECT c.customers_firstname, c.customers_lastname, c.customers_email_address,"
                <> "      c.customers_telephone, c.customers_password, a.entry_street_address,"
                <> "      a.entry_suburb, a.entry_postcode, a.entry_city,"
                <> "      a.entry_state, z.zone_name, co.countries_name "
                <> "FROM customers AS c "
                <> "RIGHT JOIN address_book AS a "
                <> "    ON c.customers_default_address_id=a.address_book_id "
                <> "LEFT JOIN zones AS z "
                <> "    ON a.entry_zone_id=z.zone_id "
                <> "RIGHT JOIN countries as co "
                <> "    ON entry_country_id=co.countries_id "
                <> "WHERE c.COWOA_account=0"
            )
    forM customers $
        \[ MySQLText firstName, MySQLText lastName, MySQLText email
         , MySQLText telephone, MySQLText _, MySQLText address
         , MySQLText addressTwo , MySQLText zipCode, MySQLText city
         , MySQLText state , nullableZoneName, MySQLText country
         ] ->
        let
            zone =
                case nullableZoneName of
                    MySQLText text ->
                        text
                    _ ->
                        state
        in do
            token <- generateToken
            return Customer
                { customerFirstName = firstName
                , customerLastName = lastName
                , customerAddressOne = address
                , customerAddressTwo = addressTwo
                , customerCity = city
                , customerState = zone
                , customerZipCode = zipCode
                , customerCountry = country
                , customerTelephone = telephone
                , customerEmail = email
                , customerEncryptedPassword = ""
                , customerAuthToken = token
                , customerIsAdmin = email == "gardens@southernexposure.com"
                }
    where generateToken = UUID.toText <$> UUID4.nextRandom


-- Persistent Model Saving Functions
insertCategories :: [(Int, Int, Category)] -> SqlWriteT IO (IntMap.IntMap (Key Category))
insertCategories =
    foldM insertCategory IntMap.empty
    where insertCategory intMap (mysqlId, mysqlParentId, category) = do
            let maybeParentId =
                    IntMap.lookup mysqlParentId intMap
                category' =
                    category { categoryParentId = maybeParentId }
            categoryId <- insert category'
            return $ IntMap.insert mysqlId categoryId intMap


insertProducts :: [(Int, Product)] -> IntMap.IntMap (Key Category) -> SqlWriteT IO ()
insertProducts products categoryIdMap =
    mapM_ insertProduct products
    where insertProduct (mysqlCategoryId, prod) = do
            let categoryIds =
                    maybeToList $ IntMap.lookup mysqlCategoryId categoryIdMap
                product' = prod { productCategoryIds = categoryIds }
            insert product'

insertVariants :: [(T.Text, ProductVariant)] -> SqlWriteT IO ()
insertVariants =
    mapM_ insertVariant
    where insertVariant (baseSku, variant) = do
            maybeProduct <- getBy $ UniqueBaseSku baseSku
            case maybeProduct of
                Nothing ->
                    lift . putStrLn $ "No product for: " ++ show variant
                Just (Entity prodId _) ->
                    void . insert $ variant { productVariantProductId = prodId }

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

insertPages :: [Page] -> SqlWriteT IO ()
insertPages = insertMany_

insertCustomers :: [Customer] -> SqlWriteT IO ()
insertCustomers = insertMany_
