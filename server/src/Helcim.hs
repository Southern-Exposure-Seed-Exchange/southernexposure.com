{-# LANGUAGE OverloadedStrings #-}
module Helcim where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.UUID.V4 (nextRandom)

import Helcim.API as API
    ( IdempotencyKey(..)
    , HelcimError
    , runHelcimClient
    , createCheckout
    , createCustomer
    , getCustomer
    , purchase
    , refund
    )
import Helcim.API.Types.Checkout as Checkout
import Helcim.API.Types.Customer
import Helcim.API.Types.Common

import Config (Config(..))
import Server (App)
import Helcim.API.Types.Payment (PurchaseRequest, PaymentResponse, RefundRequest)

createVerifyCheckout :: App (Either HelcimError CheckoutCreateResponse)
createVerifyCheckout = do
    authToken <- asks getHelcimAuthKey
    let request = InitializeRequest
            { ccrPaymentType = Verify
            , ccrAmount = 0
            , ccrCurrency = "USD"
            , Checkout.ccrCustomerCode = Nothing
            , ccrInvoiceNumber = Nothing
            , ccrPaymentMethod = Nothing
            , ccrAllowPartial = Nothing
            , ccrHasConvenienceFee = Nothing
            , ccrTaxAmount = Nothing
            , ccrHideExistingPaymentDetails = Nothing
            , ccrSetAsDefaultPaymentMethod = Nothing
            , ccrTerminalId = Nothing
            , ccrConfirmationScreen = Just True
            , ccrDigitalWallet = Nothing
            , ccrDisplayContactFields = Nothing
            , ccrCustomStyling = Just $ CustomStyling
                { csAppearance = Just "light"
                , csBrandColor = Just "#158312"
                , csCornerRadius = Just "rounded"
                , csCtaButtonText = Nothing
                }
            , ccrCustomerRequest = Nothing
            , ccrInvoiceRequest = Nothing
            }
        clientAction = API.createCheckout (Just authToken) request
    liftIO $ runHelcimClient clientAction

purchase :: PurchaseRequest -> App (Either HelcimError PaymentResponse)
purchase request = do
    authToken <- asks getHelcimAuthKey
    idempotencyKey <- IdempotencyKey <$> liftIO nextRandom
    let clientAction = API.purchase (Just authToken) (Just idempotencyKey) request
    liftIO $ runHelcimClient clientAction

createCustomer :: CreateCustomerRequest -> App (Either HelcimError CustomerResponse)
createCustomer request = do
    authToken <- asks getHelcimAuthKey
    let clientAction = API.createCustomer (Just authToken) request
    liftIO $ runHelcimClient clientAction

getCustomer :: CustomerId -> App (Either HelcimError CustomerResponse)
getCustomer customerId = do
    authToken <- asks getHelcimAuthKey
    let clientAction = API.getCustomer (Just authToken) customerId
    liftIO $ runHelcimClient clientAction

refund :: RefundRequest -> App (Either HelcimError PaymentResponse)
refund request = do
    authToken <- asks getHelcimAuthKey
    idempotencyKey <- IdempotencyKey <$> liftIO nextRandom
    let clientAction = API.refund (Just authToken) (Just idempotencyKey) request
    liftIO $ runHelcimClient clientAction
