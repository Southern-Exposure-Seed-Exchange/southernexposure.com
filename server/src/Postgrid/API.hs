{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Postgrid.API
    ( API
    , ApiKey(..)
    , AuthHeader
    , BoolFlag(..)
    , PostgridAddressVerificationAPI
    , PostgridError(..)

    , api
    , baseUrl
    , runPostgridClient
    , verifyStructuredAddress
    ) where

import Control.Exception (Exception)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.Client (ClientM, client, BaseUrl(..), Scheme(..), ClientError(..), mkClientEnv, runClientM)

import Postgrid.API.Types

newtype ApiKey = ApiKey { unApiKey :: Text }
    deriving (Show, Eq)
    deriving newtype ToHttpApiData

type AuthHeader = Header "x-api-key" ApiKey

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.postgrid.com" 443 "/v1"

newtype BoolFlag = BoolFlag { unBoolFlag :: Bool }
    deriving (Show, Eq)

instance ToHttpApiData BoolFlag where
    -- Postgrid expects lowercase "true"/"false" strings for boolean query parameters
    toQueryParam (BoolFlag True)  = "true"
    toQueryParam (BoolFlag False) = "false"

type PostgridAddressVerificationAPI =
    "addver" :> "verifications" :> AuthHeader :>
    QueryParam "includeDetails" BoolFlag :> QueryParam "properCase" BoolFlag :> QueryParam "geocode" BoolFlag :>
    ReqBody '[JSON] VerifyStructuredAddressRequest :> Post '[JSON] VerifyStructuredAddressResponse

type API = PostgridAddressVerificationAPI

api :: Proxy API
api = Proxy

verifyStructuredAddress
    :: Maybe ApiKey -> Maybe BoolFlag -> Maybe BoolFlag -> Maybe BoolFlag
    -> VerifyStructuredAddressRequest
    -> ClientM VerifyStructuredAddressResponse
verifyStructuredAddress = client api

data ApiError = ApiError
    { aeStatus :: Text
    , aeMessage :: Text
    } deriving (Show, Eq)

data PostgridError
    = PostgridApiError ApiError
    | PostgridClientError ClientError
    deriving (Show, Eq)

instance Exception PostgridError

runPostgridClient :: PostgridResponse a => ClientM a -> IO (Either PostgridError a)
runPostgridClient clientM = do
    manager <- newManager tlsManagerSettings
    let env = mkClientEnv manager baseUrl
    res <- runClientM clientM env
    case res of
        Left err -> return $ Left (PostgridClientError err)
        Right val -> case getStatus val of
            "success" -> return $ Right val
            _         -> return $ Left (PostgridApiError (ApiError (getStatus val) (getMessage val)))
