{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    ( DefaultOrdered(..), FromNamedRecord(..), ToNamedRecord(..)
    , decodeByName, encodeDefaultOrderedByName, header, namedRecord, (.:), (.=)
    )
import Database.Persist
import Database.Persist.Sql (fromSqlKey)
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, runSqlPool
    )
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Environment (getArgs)

import Models
import Utils (makeSqlPool)

data InputCustomerData = InputCustomerData
    { inputFullName      :: T.Text
    , inputNamePrefix    :: T.Text
    , inputFirstName     :: T.Text
    , inputMiddleName    :: T.Text
    , inputLastName      :: T.Text
    , inputNameSuffix    :: T.Text
    , inputCompany       :: T.Text
    , inputAddress1      :: T.Text
    , inputAddress2      :: T.Text
    , inputCity          :: T.Text
    , inputState         :: T.Text
    , inputZipcode       :: T.Text
    , inputCountry       :: T.Text
    , inputEmail         :: T.Text
    , inputFax           :: T.Text
    , inputPhone         :: T.Text
    , inputId            :: Int
    , inputOriginalSource :: T.Text
    } deriving (Show, Generic)

instance FromNamedRecord InputCustomerData where
    parseNamedRecord r = InputCustomerData
        <$> r .: "Full Name"
        <*> r .: "Name Prefix"
        <*> r .: "First Name"
        <*> r .: "Middle Name"
        <*> r .: "Last Name"
        <*> r .: "Name Suffix"
        <*> r .: "Company"
        <*> r .: "Address1"
        <*> r .: "Address2"
        <*> r .: "City"
        <*> r .: "State"
        <*> r .: "Zipcode"
        <*> r .: "Country"
        <*> r .: "Email"
        <*> r .: "Fax"
        <*> r .: "Phone"
        <*> r .: "Customer ID"
        <*> r .: "Original Source"

data OutputCustomerData = OutputCustomerData
    { outputFullName      :: T.Text
    , outputNamePrefix    :: T.Text
    , outputFirstName     :: T.Text
    , outputMiddleName    :: T.Text
    , outputLastName      :: T.Text
    , outputNameSuffix    :: T.Text
    , outputCompany       :: T.Text
    , outputAddress1      :: T.Text
    , outputAddress2      :: T.Text
    , outputCity          :: T.Text
    , outputState         :: T.Text
    , outputZipcode       :: T.Text
    , outputCountry       :: T.Text
    , outputEmail         :: T.Text
    , outputFax           :: T.Text
    , outputPhone         :: T.Text
    , outputId            :: Int
    , outputOriginalSource :: T.Text
    , outputWebId         :: Maybe Int
    } deriving (Show, Generic)

instance ToNamedRecord OutputCustomerData where
    toNamedRecord (OutputCustomerData fullName namePrefix firstName middleName lastName nameSuffix company address1 address2 city state zipcode country email fax phone cid originalSource webId) =
        namedRecord
            [ "Full Name"        .= fullName
            , "Name Prefix"      .= namePrefix
            , "First Name"       .= firstName
            , "Middle Name"      .= middleName
            , "Last Name"        .= lastName
            , "Name Suffix"      .= nameSuffix
            , "Company"          .= company
            , "Address1"         .= address1
            , "Address2"         .= address2
            , "City"             .= city
            , "State"            .= state
            , "Zipcode"          .= zipcode
            , "Country"          .= country
            , "Email"            .= email
            , "Fax"              .= fax
            , "Phone"            .= phone
            , "Customer ID"      .= cid
            , "Original Source"  .= originalSource
            , "Web ID"           .= webId
            ]

instance DefaultOrdered OutputCustomerData where
    headerOrder _ = header
        [ "Full Name", "Name Prefix", "First Name", "Middle Name", "Last Name", "Name Suffix"
        , "Company", "Address1", "Address2", "City", "State", "Zipcode", "Country"
        , "Email", "Fax", "Phone", "Customer ID", "Original Source", "Web ID"
        ]

readCustomerCSV :: FilePath -> IO (Either String [InputCustomerData])
readCustomerCSV filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of
        Left err -> return $ Left err
        Right (_, records) -> return $ Right $ V.toList records

main :: IO ()
main = do
    args <- getArgs
    case args of
        [csvFile] -> do
            inputCustomersData <- readCustomerCSV csvFile
            case inputCustomersData of
                Left err -> putStrLn $ "Error reading CSV: " ++ err
                Right customers -> do
                    pool <- connectToPostgres
                    outputCustomers <- runSqlPool (mapStoneEdgeCustomers customers) pool
                    BL.writeFile "output_customers.csv" $ encodeDefaultOrderedByName outputCustomers
        _ -> putStrLn "Expected exactly 1 argument: CUSTOMERS_CSV_FILE"


connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ makeSqlPool 1

mapStoneEdgeCustomers :: [InputCustomerData] -> SqlPersistT IO [OutputCustomerData]
mapStoneEdgeCustomers inputCustomers = do
    forM inputCustomers $ \cust -> do
        let email = inputEmail cust
        mCustomer <- getBy $ UniqueEmail email
        let webId = fmap (fromIntegral . fromSqlKey . entityKey) mCustomer
        return $ OutputCustomerData
            (inputFullName cust)
            (inputNamePrefix cust)
            (inputFirstName cust)
            (inputMiddleName cust)
            (inputLastName cust)
            (inputNameSuffix cust)
            (inputCompany cust)
            (inputAddress1 cust)
            (inputAddress2 cust)
            (inputCity cust)
            (inputState cust)
            (inputZipcode cust)
            (inputCountry cust)
            (inputEmail cust)
            (inputFax cust)
            (inputPhone cust)
            (inputId cust)
            (inputOriginalSource cust)
            webId
