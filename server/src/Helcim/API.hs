{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Helcim.API where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:))
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.Client (ClientM, client, BaseUrl(..), Scheme(..), ClientError(..), mkClientEnv, runClientM, ResponseF (Response))

import Helcim.API.Types.Checkout
import Helcim.API.Types.Customer
import Helcim.API.Types.Payment

newtype ApiToken = ApiToken { unAuthToken :: Text }
    deriving (Show, Eq)
    deriving newtype ToHttpApiData

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.helcim.com" 443 "/v2"

newtype IdempotencyKey = IdempotencyKey { unIdempotencyKey :: UUID }
    deriving (Show, Eq)
    deriving newtype ToHttpApiData

type AuthHeader = Header "api-token" ApiToken

type IdempotencyKeyHeader = Header "idempotency-key" IdempotencyKey

type HeclimPayAPI =
    "helcim-pay" :> "checkout"
        :> AuthHeader
        :> ReqBody '[JSON] InitializeRequest
        :> Post '[JSON] CheckoutCreateResponse

type PurchaseAPI = "payment" :> (
        "purchase" :> AuthHeader :> IdempotencyKeyHeader :> ReqBody '[JSON] PurchaseRequest :> Post '[JSON] PaymentResponse
        :<|> "refund"   :> AuthHeader :> IdempotencyKeyHeader :> ReqBody '[JSON] RefundRequest  :> Post '[JSON] PaymentResponse
    )

type CustomerAPI = "customers" :> (
        AuthHeader :> Capture "customer_id" CustomerId :> Get '[JSON] CustomerResponse
        :<|> AuthHeader :> ReqBody '[JSON] CreateCustomerRequest :> Post '[JSON] CustomerResponse
    )

type API = HeclimPayAPI :<|> PurchaseAPI :<|> CustomerAPI

api :: Proxy API
api = Proxy

createCheckout :: Maybe ApiToken -> InitializeRequest -> ClientM CheckoutCreateResponse
purchase       :: Maybe ApiToken -> Maybe IdempotencyKey -> PurchaseRequest -> ClientM PaymentResponse
refund         :: Maybe ApiToken -> Maybe IdempotencyKey -> RefundRequest  -> ClientM PaymentResponse
getCustomer    :: Maybe ApiToken -> CustomerId -> ClientM CustomerResponse
createCustomer :: Maybe ApiToken -> CreateCustomerRequest -> ClientM CustomerResponse
createCheckout :<|> (purchase :<|> refund) :<|> (getCustomer :<|> createCustomer) = client api

newtype ApiError = ApiError { errors :: [Text] }
    deriving (Show, Eq)

instance FromJSON ApiError where
    parseJSON = withObject "ApiError" $ \v -> ApiError <$> v .: "errors"

data HelcimError
    = HelcimApiError ApiError
    | HelcimClientError ClientError
    deriving (Show, Eq)

instance Exception HelcimError

tryClientErrorToApiError :: ClientError -> Maybe ApiError
tryClientErrorToApiError err = case err of
    FailureResponse _ (Response _ _ _ body) ->
        case eitherDecode body of
            Left _        -> Nothing
            Right apiError -> Just apiError
    _ -> Nothing

runHelcimClient :: ClientM a -> IO (Either HelcimError a)
runHelcimClient clientAction = do
    manager <- newManager tlsManagerSettings
    result  <- runClientM clientAction (mkClientEnv manager baseUrl)
    case result of
        Left err ->
            case tryClientErrorToApiError err of
                Just apiError -> return $ Left (HelcimApiError apiError)
                Nothing       -> return $ Left (HelcimClientError err)
        Right val -> return $ Right val
