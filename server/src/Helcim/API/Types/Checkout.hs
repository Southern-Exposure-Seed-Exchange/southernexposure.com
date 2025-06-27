{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Helcim.API.Types.Checkout where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Scientific (Scientific)
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)

import Helcim.API.Types.Common
import Helcim.API.Types.Customer
import Helcim.API.Types.Payment (PaymentResponse)
import Helcim.API.Types.Utils (helcimAesonOptions)

data PaymentMethod
    = CreditCard
    | ACH
    | CreditCardACH
    deriving (Show, Eq)

instance ToJSON PaymentMethod where
    toJSON CreditCard    = Aeson.String "cc"
    toJSON ACH           = Aeson.String "ach"
    toJSON CreditCardACH = Aeson.String "cc-ach"

newtype DigitalWallet = DigitalWallet
    { unDigitalWallet :: Map Text Int
    }
    deriving (Show, Generic)
    deriving newtype ToJSON

-- | Datatype that represents the input for the https://api.helcim.com/v2/helcim-pay/initialize API endpoint.
--
--  It is used to obtain a checkout token for initiating a payment process.
--  This token is later used in the frontend to render the payment form.
data InitializeRequest = InitializeRequest
    { ccrPaymentType :: PaymentType
    , ccrAmount :: Scientific
    , ccrCurrency :: Text
    , ccrCustomerCode :: Maybe CustomerCode
    , ccrInvoiceNumber :: Maybe Text
    , ccrPaymentMethod :: Maybe PaymentMethod
    , ccrAllowPartial :: Maybe Int
    , ccrHasConvenienceFee :: Maybe Int
    , ccrTaxAmount :: Maybe Scientific
    , ccrHideExistingPaymentDetails :: Maybe Int
    , ccrSetAsDefaultPaymentMethod :: Maybe Int
    , ccrTerminalId :: Maybe Int
    , ccrConfirmationScreen :: Maybe Bool
    , ccrDigitalWallet :: Maybe DigitalWallet -- e.g., ["google-pay"]
    , ccrDisplayContactFields :: Maybe Int
    , ccrCustomStyling :: Maybe CustomStyling
    , ccrCustomerRequest :: Maybe CustomerRequest
    , ccrInvoiceRequest :: Maybe Invoice
    } deriving (Show, Generic)

data CustomStyling = CustomStyling
    { csAppearance     :: Maybe Text  -- "light" | "dark" | "system"
    , csBrandColor     :: Maybe Text  -- Hex: "#815AF0"
    , csCornerRadius   :: Maybe Text  -- "pill" | "rounded" | "rectangular"
    , csCtaButtonText  :: Maybe Text  -- "pay" | "checkout" | etc.
    } deriving (Show, Generic)

data CustomerRequest = CustomerRequest
    { crCustomerCode :: Maybe CustomerCode
    , crContactName  :: Text
    , crBusinessName :: Maybe Text
    , crCellPhone    :: Maybe Text
    , crBillingAddress :: Maybe Address
    , crShippingAddress :: Maybe Address
    } deriving (Show, Generic)

newtype CheckoutToken = CheckoutToken
    { unCheckoutToken :: Text }
    deriving (Show, Generic)
    deriving newtype FromJSON
    deriving newtype ToJSON

newtype SecretToken = SecretToken
    { unSecretToken :: Text }
    deriving (Show, Generic)
    deriving newtype FromJSON

-- | Datatype that represents the response from the https://api.helcim.com/v2/helcim-pay/initialize API endpoint.
data CheckoutCreateResponse = CheckoutCreateResponse
    { ccrCheckoutToken :: CheckoutToken
    , ccrSecretToken :: SecretToken
    } deriving (Show, Generic)

-- | Datatype that represents the event data obtained from HelcimPay.js checkout
-- plus the checkout token to do the verification on the server side prior
-- to processing the payment.
-- see https://devdocs.helcim.com/docs/validate-helcimpayjs
data CheckoutData = CheckoutData
    { cdData :: PaymentResponse
    , cdHash :: Text
    , cdCheckoutToken :: CheckoutToken
    }

deriveToJSON helcimAesonOptions ''InitializeRequest
deriveToJSON helcimAesonOptions ''CustomStyling
deriveToJSON helcimAesonOptions ''CustomerRequest

deriveFromJSON helcimAesonOptions ''CheckoutCreateResponse
deriveFromJSON helcimAesonOptions ''CheckoutData
