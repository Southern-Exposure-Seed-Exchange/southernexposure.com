{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-| Merge customers with case-insensitive duplicate emails for various
domains, & create a CSV export of emails for all other domains.
-}
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Logger (runNoLoggingT)
import Data.Conduit ((.|), runConduitRes)
import Data.Csv (encode)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, SqlBackend, createPostgresqlPool, runSqlPool
    )

import Models

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- | The domains whose emails should get merged instead of exported.
mergeDomains :: [T.Text]
mergeDomains =
    [ "gmail.com"
    , "yahoo.com"
    , "aol.com"
    , "hotmail.com"
    , "comcast.net"
    , "bellsouth.net"
    , "msn.com"
    , "verizon.net"
    , "att.net"
    , "me.com"
    , "live.com"
    , "outlook.com"
    , "cox.net"
    , "sbcglobal.net"
    , "icloud.com"
    , "earthlink.net"
    , "mac.com"
    , "ymail.com"
    , "juno.com"
    , "charter.net"
    , "windstream.net"
    , "embarqmail.com"
    ]


main :: IO ()
main = do
    psql <- connectToPostgres
    duplicateCustomers <- runSqlPool getDuplicateCustomers psql
    let (customersToMerge, customersToExport) = splitCustomers duplicateCustomers
    putStrLn $ show (length customersToMerge) ++ " Customers to Merge"
    mapM_ (flip runSqlPool psql . mergeCustomer) customersToMerge
    exportCustomers customersToExport

connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ createPostgresqlPool "dbname=sese-website" 8


-- | Build the list of Customers with equivalent case-insensitive emails.
getDuplicateCustomers :: SqlPersistT IO [(T.Text, [Entity Customer])]
getDuplicateCustomers =
    fmap (M.toList . M.filter duplicatesOnly) $ runConduitRes $
        selectSource [] []
            .| CL.fold insertCustomer M.empty
  where
    duplicatesOnly :: [Entity Customer] -> Bool
    duplicatesOnly cs =
        length cs > 1
    insertCustomer
        :: M.Map T.Text [Entity Customer]
        -> Entity Customer
        -> M.Map T.Text [Entity Customer]
    insertCustomer dupeMap e@(Entity _ customer) =
        M.alter
            (\case
                Nothing ->
                    Just [e]
                Just cs ->
                    Just $ e : cs
            )
            (T.toLower $ customerEmail customer)
            dupeMap

-- | Split customers into a list of those to merge and those to export.
splitCustomers :: [(T.Text, [Entity Customer])] -> ([[Entity Customer]], [[Entity Customer]])
splitCustomers =
    foldr
        (\(email, cs) (merges, exports) ->
            if any (`T.isSuffixOf` email) mergeDomains then
                (cs : merges, exports)
            else
                (merges, cs : exports)
        )
        ([], [])

-- | Write each list of duplicate customers as a single CSV row
exportCustomers :: [[Entity Customer]] -> IO ()
exportCustomers cs =
    LBS.writeFile "duplicate-customers.csv" $ encode $ map (map $ customerEmail . entityVal) cs

-- | Merge the Customer's Addresses, Orders, PasswordResets, & Carts,
-- deleting the extraneous Customers afterwards.
mergeCustomer :: [Entity Customer] -> SqlPersistT IO ()
mergeCustomer cs = case getBaseCustomer of
    Nothing ->
        return ()
    Just (base, rest) -> do
        let mergedIds = map entityKey rest
            baseId = entityKey base
            mergeOn
                :: (PersistEntityBackend e ~ SqlBackend, PersistEntity e)
                => EntityField e CustomerId -> SqlPersistT IO ()
            mergeOn field =
                updateWhere [field <-. mergedIds] [field =. baseId]
        mergeAddresses baseId mergedIds
        mergeOn OrderCustomerId
        mergeOn PasswordResetCustomerId
        mergeOn ReviewCustomerId
        baseCart <- getBy (UniqueCustomerCart $ Just baseId) >>= \case
            Just c ->
                return c
            Nothing ->
                insertEntity Cart
                    { cartCustomerId = Just baseId
                    , cartSessionToken = Nothing
                    , cartExpirationTime = Nothing
                    }
        mergedCarts <- selectList [CartCustomerId <-. map Just mergedIds] []
        mapM_ (mergeCarts baseCart) mergedCarts
        deleteWhere [CustomerId <-. mergedIds]
  where
    -- Merge all addresses for the customers into the base customer,
    -- removing any duplicates.
    mergeAddresses :: CustomerId -> [CustomerId] -> SqlPersistT IO ()
    mergeAddresses baseId = \case
        [] ->
            return ()
        next : rest -> do
            customerAddresses <- selectList [AddressCustomerId ==. baseId] []
            nextAddresses <- selectList [AddressCustomerId ==. next] []
            let (newAddresses, dupeAddresses) =
                    splitDuplicateAddresses customerAddresses nextAddresses
            updateWhere [AddressId <-. newAddresses] [AddressCustomerId =. baseId]
            forM_ dupeAddresses $ \(mergeIntoId, duplicateId) -> do
                updateWhere
                    [OrderBillingAddressId ==. Just duplicateId]
                    [OrderBillingAddressId =. Just mergeIntoId]
                updateWhere
                    [OrderShippingAddressId ==. duplicateId]
                    [OrderShippingAddressId =. mergeIntoId]
                delete duplicateId
            mergeAddresses baseId rest
    -- Split the addresses into unique addresses and duplicate addresses
    splitDuplicateAddresses
        :: [Entity Address] -> [Entity Address]
        -> ([AddressId], [(AddressId, AddressId)])
    splitDuplicateAddresses baseAddresses =
        foldr
            (\(Entity mergeId mergeAddr) (new, toMerge) ->
                case L.find (isSameAddress mergeAddr) baseAddresses of
                    Nothing ->
                        (mergeId : new, toMerge)
                    Just (Entity matchingBaseId _) ->
                        (new, (matchingBaseId, mergeId) : toMerge)
            )
        ([], [])
    -- Case insensitive equality of address fields
    isSameAddress :: Address -> Entity Address -> Bool
    isSameAddress addr1 (Entity _ addr2) =
        let checkAddressField selector =
                T.toLower (selector addr1) == T.toLower (selector addr2)
        in
        checkAddressField addressFirstName &&
        checkAddressField addressLastName &&
        checkAddressField addressCompanyName &&
        checkAddressField addressAddressOne &&
        checkAddressField addressAddressTwo &&
        checkAddressField addressCity &&
        (addressState addr1 == addressState addr2) &&
        checkAddressField addressZipCode &&
        (addressCountry addr1 == addressCountry addr2) &&
        checkAddressField addressPhoneNumber &&
        (addressType addr1 == addressType addr2)

    -- Prefer new password format, otherwise use the largest ID number.
    getBaseCustomer :: Maybe (Entity Customer, [Entity Customer])
    getBaseCustomer =
        getNewPasswordFormat <|> getLargestId
    getNewPasswordFormat :: Maybe (Entity Customer, [Entity Customer])
    getNewPasswordFormat =
        let maybeCustomer =
                L.find
                (\(Entity _ customer) -> not (T.isInfixOf ":" $ customerEmail customer))
                cs
        in
        (\e@(Entity cId _) -> (e, filter ((/= cId) . entityKey) cs))
            <$> maybeCustomer
    getLargestId :: Maybe (Entity Customer, [Entity Customer])
    getLargestId =
        let maybeId = safeMax $ map entityKey cs
        in
        maybeId >>= \cId ->
            (,)
                <$> L.find ((== cId) . entityKey) cs
                <*> pure (filter ((/= cId) . entityKey) cs)
    safeMax :: Ord a => [a] -> Maybe a
    safeMax = \case
        [] ->
            Nothing
        xs ->
            Just $ maximum xs
