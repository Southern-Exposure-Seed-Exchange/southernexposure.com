{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module implements our integration with the Avalara Sales Tax
Calculation API. It is a minimal implementation that supports the features
we use, which is essentially Transaction Creation & Refunding.

TODO: RefundTransaction API
TODO: src/Config.hs - add env, id, key values - parse/set in app/Main.hs
TODO: src/Server.hs - add runner function: (ReaderT Config m SpecificResponseType -> App SpecificResponseType)

-}
module Avalara
    ( -- * Configuration
      AccountId(..)
    , LicenseKey(..)
    , ServiceEnvironment(..)
    , Config(..)
      -- * Requests
    , ping
    , createTransaction
    , commitTransaction
    , createCustomers
      -- * Types
      -- ** Errors
    , WithError(..)
    , ErrorInfo(..)
    , ErrorDetail(..)
      -- ** Requests
    , CreateTransactionRequest(..)
    , CommitTransactionRequest(..)
    , CreateCustomersRequest(..)
      -- ** Responses
    , PingResponse(..)
    , Transaction(..)
      -- ** Transactions
    , LineItem(..)
    , DocumentType(..)
    , TransactionStatus(..)
    , TaxCode(..)
    , shippingAndHandlingTaxCode
    , TransactionCode(..)
      -- ** Customer
    , Customer(..)
    , CustomerCode(..)
      -- ** Address
    , Address(..)
    , AddressInfo(..)
      -- ** Miscellaneous
    , CompanyId(..)
    , CompanyCode(..)
    , AuthenticationType(..)
    ) where

import Control.Monad.Reader (MonadIO, ReaderT, asks, ask, liftIO)
import Data.Aeson
    ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), Value(..), object, withObject
    , withText, withScientific, toJSONList
    )
import Data.Default (def)
import Data.Foldable (asum)
import Data.Monoid ((<>))
import Data.Scientific (Scientific)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Network.HTTP.Req
    ( (/:), Option, Scheme(Https), Url, runReq, req, header, https, GET(..)
    , POST(..), responseBody, jsonResponse, NoReqBody(..), ReqBodyJson(..)
    , HttpMethod, HttpBody, HttpBodyAllowed, AllowsBody, ProvidesBody
    )
import Network.HostName (getHostName)

import Paths_sese_website (version)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T


-- CONFIGURATION


-- | Your Avalara Account's ID Number for authentication.
newtype AccountId =
    AccountId
        { fromAccountId :: T.Text
        } deriving (Show, Read, Eq)


-- | Your Avalara Account's License Number for authentication.
newtype LicenseKey =
    LicenseKey
        { fromLicenseKey :: T.Text
        } deriving (Show, Read, Eq)


-- | The Avalara Service that requests will hit.
data ServiceEnvironment
    = ProductionEnvironment
    | SandboxEnvironment
    deriving (Show, Read, Eq)

environmentHostname :: ServiceEnvironment -> T.Text
environmentHostname = \case
    ProductionEnvironment ->
        "rest.avatax.com"
    SandboxEnvironment ->
        "sandbox-rest.avatax.com"


-- | Required configuration data for making authenticated requests.
data Config =
    Config
        { cAccountId :: AccountId
        , cLicenseKey :: LicenseKey
        , cServiceEnvironment :: ServiceEnvironment
        , cAppName :: T.Text
        -- ^ The name of your Application to report to Avalara
        , cAppVerson :: T.Text
        -- ^ The version of your Appliation to report to Avalara
        } deriving (Show, Read)

-- | Get the URL for the ServiceEnvironment in the Config.
getBaseUrl :: Monad m => ReaderT Config m T.Text
getBaseUrl =
    asks $ environmentHostname . cServiceEnvironment

-- | Build the @Authorization@ header using the Config's 'AccountId'
-- & 'LicenseKey'.
generateAuthorizationHeader :: Monad m => ReaderT Config m (Option 'Https)
generateAuthorizationHeader = do
    Config { cAccountId, cLicenseKey } <- ask
    let encodedDetails =
            Base64.encode . encodeUtf8 $
                fromAccountId cAccountId <> ":" <> fromLicenseKey cLicenseKey
    return $ header "Authorization" $ "Basic " <> encodedDetails

-- | Build the @X-Avalara-Client@ header using the Config's 'cAppName'
-- & 'cAppVersion' along with this library's version and the current
-- machine's hostname(determined by 'getHostName'.
generateClientHeader :: MonadIO m => ReaderT Config m (Option 'Https)
generateClientHeader = do
    Config { cAppName, cAppVerson } <- ask
    hostname <- T.pack <$> liftIO getHostName
    let libraryVersion =
            T.pack $ showVersion version
        clientHeader =
            encodeUtf8 $ T.intercalate "; "
                [ cAppName
                , cAppVerson
                , "AvaTax-Haskell-SESE"
                , libraryVersion
                , hostname
                ]
    return $ header "X-Avalara-Client" clientHeader



-- REQUESTS


-- | A ping request lets you check your connectivity with the AvaTax API
-- server & verify your authenticaton credentials.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/methods/Utilities/Ping/
ping :: MonadIO m => ReaderT Config m (WithError PingResponse)
ping =
    makeGetRequest Ping

-- | A createTransaction request lets you get a sales tax quote for an
-- order or record a completed order.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/methods/Transactions/CreateTransaction/
createTransaction :: MonadIO m => CreateTransactionRequest -> ReaderT Config m (WithError Transaction)
createTransaction =
    makePostRequest CreateTransaction

-- | A commitTransaction request lets you mark an existing Transaction
-- as committed.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/methods/Transactions/CommitTransaction/
commitTransaction :: MonadIO m => CompanyCode -> TransactionCode -> CommitTransactionRequest
    -> ReaderT Config m (WithError Transaction)
commitTransaction companyCode transactionCode =
    makePostRequest $ CommitTransaction companyCode transactionCode


-- | A createCustomers request lets you create new Customers for a Company.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/methods/Customers/CreateCustomers/
createCustomers :: MonadIO m => CompanyId -> CreateCustomersRequest -> ReaderT Config m (WithError [Customer])
createCustomers companyId =
    makePostRequest $ CreateCustomers companyId


-- TYPES


-- API

data Endpoint
    = Ping
    | CreateTransaction
    | CreateCustomers CompanyId
    | CommitTransaction CompanyCode TransactionCode
    deriving (Show, Read, Eq)

endpointPath :: Monad m => Endpoint -> ReaderT Config m (Url 'Https)
endpointPath endpoint = do
    baseUrl <- getBaseUrl
    return . joinPaths (https baseUrl /: "api" /: "v2") $ case endpoint of
        Ping ->
            ["utilities", "ping"]
        CreateTransaction ->
            ["transactions", "create"]
        CreateCustomers (CompanyId companyId) ->
            ["companies", T.pack (show companyId), "customers"]
        CommitTransaction (CompanyCode companyCode) (TransactionCode transCode) ->
            ["companies", companyCode, "transactions", transCode, "commit"]
  where
    joinPaths :: Url 'Https -> [T.Text] -> Url 'Https
    joinPaths =
        foldl (/:)


-- ERRORS

-- | Information about errors received during processing.
data ErrorInfo =
    ErrorInfo
        { eiCode :: T.Text
        , eiMessage :: T.Text
        , eiTarget :: T.Text
        , eiDetails :: [ErrorDetail]
        } deriving (Show, Read, Eq)

-- | Parse the ErrorInfo nested in an @"error"@ key.
instance FromJSON ErrorInfo where
    parseJSON = withObject "ErrorInfo" $ \o -> do
        v <- o .: "error"
        eiCode <- v .: "code"
        eiMessage <- v .: "message"
        eiTarget <- v .: "target"
        eiDetails <- v .: "details"
        return ErrorInfo {..}


-- | In-depth information of an Error response.
data ErrorDetail =
    ErrorDetail
        { edCode :: T.Text
        , edNumber :: Integer
        , edMessage :: T.Text
        , edDescription :: T.Text
        , edFaultCode :: T.Text
        , edHelpLink :: T.Text
        , edSeverity :: T.Text
        } deriving (Show, Read, Eq)

instance FromJSON ErrorDetail where
    parseJSON = withObject "ErrorDetail" $ \o -> do
        edCode <- o .: "code"
        edNumber <- o .: "number"
        edMessage <- o .: "message"
        edDescription <- o .: "description"
        edFaultCode <- o .: "faultCode"
        edHelpLink <- o .: "helpLink"
        edSeverity <- o .: "severity"
        return ErrorDetail {..}


-- | Wraps a Response type for handling API responses containing an
-- 'ErrorInfo'.
data WithError a
    = SuccessfulResponse a
    | ErrorResponse ErrorInfo
    deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (WithError a) where
    parseJSON v =
        asum
            [ ErrorResponse <$> parseJSON v
            , SuccessfulResponse <$> parseJSON v
            ]


-- REQUESTS

-- | Request data for the @CreateTransacton@ endpoint.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/CreateTransactionModel/
data CreateTransactionRequest =
    CreateTransactionRequest
        { ctrCode :: Maybe TransactionCode
        -- ^ Your internal reference for the Transaction. AvaTax will
        -- generate GUID if left blank.
        , ctrLines :: [LineItem]
        -- ^ Line items for the Transaction
        , ctrType :: Maybe DocumentType
        -- ^ The type of Transction to create. Defaults to 'SalesOrder' if
        -- not present.
        , ctrCompanyCode :: Maybe CompanyCode
        -- ^ The company creating the transaction. 'Nothing' will cause
        -- AvaTax to use the account's default company.
        , ctrDate :: UTCTime
        -- ^ The date of the invoice or purchase.
        , ctrCustomerCode :: CustomerCode
        -- ^ A unique identifier for the Customer in your application.
        , ctrDiscount :: Maybe Scientific
        -- ^ A discount amount to apply to all 'LineItem' with
        -- 'liDiscounted' set to 'True'.
        , ctrAddresses :: Maybe Address
        -- ^ Default addresses for all lines in the document.
        , ctrCommit :: Maybe Bool
        -- ^ Commit the transaction after creation. Only applicable to
        -- Invoice document types, not Orders.
        } deriving (Show, Read, Eq)

instance ToJSON CreateTransactionRequest where
    toJSON CreateTransactionRequest {..} =
        object
            [ "code" .= ctrCode
            , "lines" .= ctrLines
            , "type" .= ctrType
            , "companyCode" .= ctrCompanyCode
            , "discount" .= ctrDiscount
            , "date" .= formatTime defaultTimeLocale "%FT%T+00:00" ctrDate
            , "customerCode" .= ctrCustomerCode
            , "addresses" .= ctrAddresses
            , "commit" .= ctrCommit
            ]


-- | The list of Customers to create for the @CreateCustomers@ endpoint.
newtype CreateCustomersRequest =
    CreateCustomersRequest { ccrCustomers :: [Customer] }
    deriving (Read, Show, Eq)

instance ToJSON CreateCustomersRequest where
    toJSON CreateCustomersRequest {..} =
        toJSONList ccrCustomers


newtype CommitTransactionRequest =
    CommitTransactionRequest
        { ctsrCommit :: Bool
        } deriving (Show, Read, Eq)

instance ToJSON CommitTransactionRequest where
    toJSON CommitTransactionRequest {..} =
        object
            [ "commit" .= ctsrCommit
            ]


-- RESPONSES

-- | Response body of the 'Ping' endpoint.
data PingResponse =
    PingResponse
        { prVersion :: T.Text
        -- ^ AvaTax API Version Number
        , prAuthenticated :: Bool
        -- ^ Was authentication provided in the request?
        , prAuthenticationType :: AuthenticationType
        -- ^ The type of authentication provided
        , prUserName :: Maybe T.Text
        -- ^ Username of the authenticated user.
        , prUserId :: Maybe Integer
        -- ^ ID number of the authenticated user.
        , prAccountId :: Maybe Integer
        -- ^ ID number of the authenticated user's account.
        , prCrmId :: Maybe T.Text
        -- ^ The connected Salesforce account.
        } deriving (Show, Read, Eq)

instance FromJSON PingResponse where
    parseJSON = withObject "PingResponse" $ \o -> do
        prVersion <- o .: "version"
        prAuthenticated <- o .: "authenticated"
        prAuthenticationType <- o .: "authenticationType"
        prUserName <- o .:? "authenticatedUserName"
        prUserId <- o .:? "authenticatedUserId"
        prAccountId <- o .:? "authenticatedAccountId"
        prCrmId <- o .:? "crmid"
        return PingResponse {..}


-- | Response body of the @CreateTransaction@ & @RefundTransaction@ endpoints.
--
-- There are many more fields available, but we only pull a useful subset.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/TransactionModel/
data Transaction =
    Transaction
        { tId :: Maybe Integer
        , tCode :: Maybe TransactionCode
        , tCompanyId :: Maybe CompanyId
        , tDate :: Maybe UTCTime
        , tStatus :: Maybe TransactionStatus
        , tType :: Maybe DocumentType
        , tCustomerCode :: Maybe CustomerCode
        , tReconciled :: Maybe Bool
        , tTotalAmount :: Maybe Scientific
        , tTotalExempt :: Maybe Scientific
        , tTotalTax :: Maybe Scientific
        , tTotalTaxable :: Maybe Scientific
        , tTotalTaxCalculated :: Maybe Scientific
        , tLocked :: Maybe Bool
        , tRegion :: Maybe T.Text
        , tCountry :: Maybe T.Text
        }

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \o -> do
        tId <- o .: "id"
        tCode <- o .: "code"
        tCompanyId <- o .: "companyId"
        tDate <- o .: "date"
        tStatus <- o .: "status"
        tType <- o .: "type"
        tCustomerCode <- o .: "customerCode"
        tReconciled <- o .: "reconciled"
        tTotalAmount <- o .: "totalAmount"
        tTotalExempt <- o .: "totalExempt"
        tTotalTax <- o .: "totalTax"
        tTotalTaxable <- o .: "totalTaxable"
        tTotalTaxCalculated <- o .: "tTotalTaxCalculated"
        tLocked <- o .: "locked"
        tRegion <- o .: "region"
        tCountry <- o .: "country"
        return Transaction {..}


-- MISCELLANEOUS

-- | The type of authentication given to the 'Ping' endpoint.
data AuthenticationType
    = NoAuthentication
    -- ^ API call was not authenticated.
    | UsernamePassword
    -- ^ Authenticated with a Username & Password.
    | AccountIdLicenseKey
    -- ^ Authenticated with an 'AccountId' & 'LicenseKey'.
    | OpenIdBearerToken
    -- ^ Authenticated with an OpenID Bearer Token.
    deriving (Show, Read, Eq)

instance FromJSON AuthenticationType where
    parseJSON = withText "AuthenticationType" $ \case
        "None" ->
            return NoAuthentication
        "UsernamePassword" ->
            return UsernamePassword
        "AccountIdLicenseKey" ->
            return AccountIdLicenseKey
        "OpenIdBearerToken" ->
            return OpenIdBearerToken
        str ->
            fail $ "Unexpected AuthenticationType Model: " <> T.unpack str


-- | A single line in a Transaction.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/LineItemModel/
data LineItem =
    LineItem
        { liNumber :: Maybe T.Text
        -- ^ The line number within the document. Can be any useful text.
        , liQuantity :: Maybe Scientific
        -- ^ Should always be positive. 'Nothing' defaults to @1@.
        , liTotalAmount :: Maybe Scientific
        -- ^ The total amount for the line. Must be positive for sale
        -- transactions & negative for refund & return transactions.
        , liAddresses :: Maybe Address
        -- ^ Address overrides for this specific line. 'Nothing' will
        -- default to the @addreses@ field from the Document.
        , liTaxCode :: Maybe TaxCode
        -- ^ The tax code for the line. Maybe be a standard AvaTax code or
        -- a custom tax code.
        , liItemCode :: Maybe T.Text
        -- ^ Your SKU for the line.
        , liDiscounted :: Maybe Bool
        -- ^ Apply the Document's 'ctrDiscount' to this line.
        , liDescription :: Maybe T.Text
        -- ^ A description of the line item. Required for SST customers
        -- with an unmapped 'liItemCode'.
        }
    deriving (Show, Read, Eq)

instance ToJSON LineItem where
    toJSON LineItem {..} =
        object
            [ "number" .= liNumber
            , "quantity" .= liQuantity
            , "amount" .= liTotalAmount
            , "addresses" .= liAddresses
            , "taxCode" .= liTaxCode
            , "itemCode" .= liItemCode
            , "discounted" .= liDiscounted
            , "description" .= liDescription
            ]


-- | A custom Tax Code or a standard AvaTax Tax Code for classifying
-- the tax class of a 'LineItem'.
newtype TaxCode =
    TaxCode { fromTaxCode :: T.Text }
    deriving (Show, Read, Eq)

instance ToJSON TaxCode where
    toJSON = String . fromTaxCode

-- | Standard Avalara Tax Code for combined Shipping & Handling charges.
shippingAndHandlingTaxCode :: TaxCode
shippingAndHandlingTaxCode =
    TaxCode "FR030000"


-- | A subset of the Transction Document Types - only the ones that we
-- require.
data DocumentType
    = SalesOrder
    -- ^ An estimate of the tax to be paid.
    | SalesInvoice
    -- ^ A sale that has been finalized. Records the transction and
    -- calculates the final tax amount.
    | ReturnOrder
    -- ^ An estimate of the tax to be refunded.
    | ReturnInvoice
    -- ^ A return that has been finalized. Records the transaction and
    -- calculates the final amount of tax to be refunded.
    deriving (Show, Read, Eq)

instance ToJSON DocumentType where
    toJSON = String . \case
        SalesOrder ->
            "SalesOrder"
        SalesInvoice ->
            "SalesInvoice"
        ReturnOrder ->
            "ReturnOrder"
        ReturnInvoice ->
            "ReturnInvoice"

instance FromJSON DocumentType where
    parseJSON = withText "DocumentType" $ \case
        "SalesOrder" ->
            return SalesOrder
        "SalesInvoice" ->
            return SalesInvoice
        "ReturnOrder" ->
            return ReturnOrder
        "ReturnInvoice" ->
            return ReturnInvoice
        str ->
            fail $ "Unexpected DocumentType: " <> T.unpack str


newtype TransactionCode =
    TransactionCode { fromTransctionCode :: T.Text }
    deriving (Show, Read, Eq)

instance ToJSON TransactionCode where
    toJSON = String . fromTransctionCode

instance FromJSON TransactionCode where
    parseJSON = withText "TransactionCode" (return . TransactionCode)


newtype CustomerCode =
    CustomerCode { fromCustomerCode :: T.Text }
    deriving (Show, Read, Eq)

instance ToJSON CustomerCode where
    toJSON = String . fromCustomerCode

instance FromJSON CustomerCode where
    parseJSON = withText "CustomerCode" (return . CustomerCode)


newtype CompanyId =
    CompanyId { fromCompanyId :: Integer }
    deriving (Show, Read, Eq)

instance ToJSON CompanyId where
    toJSON = toJSON . fromCompanyId

instance FromJSON CompanyId where
    parseJSON = withScientific "CompanyId" (return . CompanyId . floor)


newtype CompanyCode =
    CompanyCode { fromCompanyCode :: T.Text }
    deriving (Show, Read, Eq)

instance ToJSON CompanyCode where
    toJSON = toJSON . fromCompanyCode

instance FromJSON CompanyCode where
    parseJSON = withText "CompanyCode" (return . CompanyCode)


data TransactionStatus
    = Temporary
    | Saved
    | Posted
    | Committed
    | Cancelled
    | Adjusted
    | PendingApproval
    deriving (Show, Read, Eq)

instance FromJSON TransactionStatus where
    parseJSON = withText "TransactionStatus" $ \case
        "Temporary" ->
            return Temporary
        "Saved" ->
            return Saved
        "Posted" ->
            return Posted
        "Committed" ->
            return Committed
        "Cancelled" ->
            return Cancelled
        "Adjusted" ->
            return Adjusted
        "PendingApproval" ->
            return PendingApproval
        str ->
            fail $ "Unexpected value for TransactionStatus: " <> T.unpack str


-- | Information on all Addresses involved in a Transaction.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/AddressesModel/
data Address
    = Address
        { addrSingleLocation :: Maybe AddressInfo
        , addrShipFrom :: Maybe AddressInfo
        , addrShipTo :: Maybe AddressInfo
        , addrPointOfOrderOrigin :: Maybe AddressInfo
        , addrPointOfOrderAcceptance :: Maybe AddressInfo
        } deriving (Show, Read, Eq)

instance ToJSON Address where
    toJSON Address {..} =
        object
            [ "singleLocation" .= addrSingleLocation
            , "shipFrom" .= addrShipFrom
            , "shipTo" .= addrShipTo
            , "pointOfOrderOrigin" .= addrPointOfOrderOrigin
            , "pointOfOrderAcceptance" .= addrPointOfOrderAcceptance
            ]


-- | Represents a single address to resolve.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/AddressLocationInfo/
data AddressInfo =
    AddressInfo
        { aiLocationCode :: Maybe T.Text
        -- ^ Use a pre-existing Location with the given code.
        , aiLineOne :: Maybe T.Text
        , aiLineTwo :: Maybe T.Text
        , aiLineThree :: Maybe T.Text
        , aiCity :: Maybe T.Text
        , aiRegion :: Maybe T.Text
        , aiCountry :: Maybe T.Text
        , aiPostalCode :: Maybe T.Text
        , aiLatitude :: Maybe Scientific
        , aiLongitude :: Maybe Scientific
        } deriving (Show, Read, Eq)

instance ToJSON AddressInfo where
    toJSON AddressInfo {..} =
        object
            [ "locationCode" .= aiLocationCode
            , "line1" .= aiLineOne
            , "line2" .= aiLineTwo
            , "line3" .= aiLineThree
            , "city" .= aiCity
            , "region" .= aiRegion
            , "country" .= aiCountry
            , "postalCode" .= aiPostalCode
            , "latitude" .= aiLatitude
            , "longitude" .= aiLongitude
            ]


-- | A Customer for the Request & Response data of the @CreateCustomers@
-- endpoint.
--
-- API Docs:
-- https://developer.avalara.com/api-reference/avatax/rest/v2/models/CustomerModel/
data Customer =
    Customer
        { cId :: Maybe Integer
        -- ^ The Customer ID. This is read-only so do not specify it for
        -- the 'createCustomers' request.
        , cCompanyId :: CompanyId
        -- ^ The Company the customer belongs to.
        , cCustomerCode :: CustomerCode
        -- ^ The unique code identifying the Customer in other API calls.
        , cAlternateId :: Maybe T.Text
        -- ^ A configurable alternate ID for interfacing with other systems
        -- that want to reference the Customer.
        , cName :: T.Text
        -- ^ A friendly name that identifies the Customer.
        , cLineOne :: T.Text
        , cLineTwo :: Maybe T.Text
        , cCity :: T.Text
        , cPostalCode :: T.Text
        , cRegion :: T.Text
        , cCountry :: T.Text
        , cPhoneNumber :: Maybe T.Text
        , cEmailAddress :: Maybe T.Text
        } deriving (Show, Read, Eq)

instance ToJSON Customer where
    toJSON Customer {..} =
        object
            [ "companyId" .= cCompanyId
            , "customerCode" .= cCustomerCode
            , "alternateId" .= cAlternateId
            , "name" .= cName
            , "line1" .= cLineOne
            , "line2" .= cLineTwo
            , "city" .= cCity
            , "postalCode" .= cPostalCode
            , "region" .= cRegion
            , "country" .= cCountry
            , "phoneNumber" .= cPhoneNumber
            , "emailAddress" .= cEmailAddress
            ]

instance FromJSON Customer where
    parseJSON = withObject "Customer" $ \o -> do
        cId <- o .: "id"
        cCompanyId <- o .: "companyId"
        cCustomerCode <- o .: "customerCode"
        cAlternateId <- o .: "alternateId"
        cName <- o .: "name"
        cLineOne <- o .: "line1"
        cLineTwo <- o .: "line2"
        cCity <- o .: "city"
        cPostalCode <- o .: "postalCode"
        cRegion <- o .: "region"
        cCountry <- o .: "country"
        cPhoneNumber <- o .: "phoneNumber"
        cEmailAddress <- o .: "emailAddress"
        return Customer {..}


-- HELPERS

makeGetRequest :: (FromJSON a, MonadIO m) => Endpoint -> ReaderT Config m (WithError a)
makeGetRequest endpoint =
    makeRequest endpoint GET NoReqBody

makePostRequest :: (ToJSON a, FromJSON b, MonadIO m) => Endpoint -> a -> ReaderT Config m (WithError b)
makePostRequest endpoint body =
    makeRequest endpoint POST $ ReqBodyJson body

makeRequest
    :: ( FromJSON a, HttpMethod method, HttpBody body, MonadIO m
       , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
       )
    => Endpoint -> method -> body
    -> ReaderT Config m (WithError a)
makeRequest endpoint method reqBody = do
    path <- endpointPath endpoint
    authHeader <- generateAuthorizationHeader
    clientHeader <- generateClientHeader
    let headers = authHeader <> clientHeader
    runReq def $ responseBody <$> req method path reqBody jsonResponse headers
