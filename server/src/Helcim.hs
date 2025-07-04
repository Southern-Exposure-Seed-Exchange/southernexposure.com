{-# LANGUAGE OverloadedStrings #-}
module Helcim
    ( createCustomer
    , createVerifyCheckout
    , getCardBatch
    , getCustomer
    , getCustomers
    , getTransaction
    , purchase
    , refund
    , Helcim.reverse
    , updateCustomer
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.UUID.V4 (nextRandom)
import Servant.Client (ClientM)

import Helcim.API (ApiToken, IdempotencyKey(..), HelcimError, runHelcimClient)
import qualified Helcim.API as API
    ( createCheckout
    , createCustomer
    , getCardBatch
    , getCustomer
    , getCustomers
    , getTransaction
    , purchase
    , refund
    , reverse
    , updateCustomer
    )
import Helcim.API.Types.Checkout as Checkout
import Helcim.API.Types.Customer
import Helcim.API.Types.Common

import Config (Config(..))
import Server (App)
import Helcim.API.Types.CardBatch (CardBatchId, CardBatchResponse)
import Helcim.API.Types.Payment (PurchaseRequest, TransactionResponse, RefundRequest, ReverseRequest, TransactionId)

-- Abstracted helper for Helcim actions with idempotency
runHelcimWithIdempotency
    :: (Maybe ApiToken -> Maybe IdempotencyKey -> req -> ClientM res)
    -> req
    -> App (Either HelcimError res)
runHelcimWithIdempotency apiFunc request = do
    authToken <- asks getHelcimAuthKey
    idempotencyKey <- IdempotencyKey <$> liftIO nextRandom
    let clientAction = apiFunc (Just authToken) (Just idempotencyKey) request
    liftIO $ runHelcimClient clientAction

-- Abstracted helper for Helcim actions without idempotency
runHelcim
    :: (Maybe ApiToken -> req -> ClientM res)
    -> req
    -> App (Either HelcimError res)
runHelcim apiFunc request = do
    authToken <- asks getHelcimAuthKey
    let clientAction = apiFunc (Just authToken) request
    liftIO $ runHelcimClient clientAction

createVerifyCheckout :: Maybe CustomerCode -> App (Either HelcimError CheckoutCreateResponse)
createVerifyCheckout customerCode = do
    let request = InitializeRequest
            { ccrPaymentType = Verify
            , ccrAmount = 0
            , ccrCurrency = "USD"
            , Checkout.ccrCustomerCode = customerCode
            , ccrInvoiceNumber = Nothing
            , ccrPaymentMethod = Nothing
            , ccrAllowPartial = Nothing
            , ccrHasConvenienceFee = Nothing
            , ccrTaxAmount = Nothing
            , ccrHideExistingPaymentDetails = Just 1
            , ccrSetAsDefaultPaymentMethod = Just 0
            , ccrTerminalId = Nothing
            , ccrConfirmationScreen = Just True
            , ccrDigitalWallet = Nothing
            , ccrDisplayContactFields = Nothing
            , ccrCustomStyling = Just $ CustomStyling
                { csAppearance = Just "light"
                , csBrandColor = Just "158312"
                , csCornerRadius = Just "rounded"
                , csCtaButtonText = Just "pay"
                }
            , ccrCustomerRequest = Nothing
            , ccrInvoiceRequest = Nothing
            }
    runHelcim API.createCheckout request

purchase :: PurchaseRequest -> App (Either HelcimError TransactionResponse)
purchase = runHelcimWithIdempotency API.purchase

createCustomer :: CreateCustomerRequest -> App (Either HelcimError CustomerResponse)
createCustomer = runHelcim API.createCustomer

getCustomer :: CustomerId -> App (Either HelcimError CustomerResponse)
getCustomer = runHelcim API.getCustomer

getCustomers :: GetCustomersRequest -> App (Either HelcimError [CustomerResponse])
getCustomers = runHelcim API.getCustomers

getTransaction :: TransactionId -> App (Either HelcimError TransactionResponse)
getTransaction = runHelcim API.getTransaction

getCardBatch :: CardBatchId -> App (Either HelcimError CardBatchResponse)
getCardBatch = runHelcim API.getCardBatch

updateCustomer :: CustomerId -> CreateCustomerRequest -> App (Either HelcimError CustomerResponse)
updateCustomer customerId request = do
    runHelcim (`API.updateCustomer` customerId) request

refund :: RefundRequest -> App (Either HelcimError TransactionResponse)
refund = runHelcimWithIdempotency API.refund

reverse :: ReverseRequest -> App (Either HelcimError TransactionResponse)
reverse = runHelcimWithIdempotency API.reverse
