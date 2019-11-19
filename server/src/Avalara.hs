{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module implements our integration with the Avalara Sales Tax
Calculation API. It is a minimal implementation that supports the features
we use, which is essentially Transaction Creation & Refunding.

TODO: CreateTransaction API
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
      -- * Types
      -- ** Errors
    , WithError(..)
    , ErrorInfo(..)
    , ErrorDetail(..)
      -- ** Responses
    , PingResponse(..)
      -- ** Miscellaneous
    , AuthenticationType(..)
    ) where

import Control.Monad.Reader (MonadIO, ReaderT, asks, ask, liftIO)
import Data.Aeson ((.:), (.:?), FromJSON(..), withObject, withText)
import Data.Default (def)
import Data.Foldable (asum)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Version (showVersion)
import Network.HTTP.Req
    ( (/:), Option, Scheme(Https), Url, runReq, req, header, https, GET(..)
    , responseBody, jsonResponse, NoReqBody(..)
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
ping :: MonadIO m => ReaderT Config m (WithError PingResponse)
ping =
    makeGetRequest Ping



-- TYPES


-- API

data Endpoint
    = Ping
    deriving (Show, Read, Eq)

endpointPath :: Monad m => Endpoint -> ReaderT Config m (Url 'Https)
endpointPath endpoint = do
    baseUrl <- getBaseUrl
    return . joinPaths (https baseUrl /: "api" /: "v2") $ case endpoint of
        Ping ->
            ["utilities", "ping"]
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



-- HELPERS

makeGetRequest :: (FromJSON a, MonadIO m) => Endpoint -> ReaderT Config m (WithError a)
makeGetRequest endpoint = do
    path <- endpointPath endpoint
    authHeader <- generateAuthorizationHeader
    clientHeader <- generateClientHeader
    runReq def $ responseBody
        <$> req GET path NoReqBody jsonResponse (authHeader <> clientHeader)
