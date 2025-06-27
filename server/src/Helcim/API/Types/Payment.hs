{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module Helcim.API.Types.Payment where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import GHC.Generics (Generic)

import Helcim.API.Types.Common
import Helcim.API.Types.Utils (helcimAesonOptions)
import Helcim.API.Types.Customer (CustomerCode)
import Database.Esqueleto.Experimental (PersistField, PersistFieldSql)

newtype CardData = CardData
    { cdCardToken :: CardToken
    } deriving (Show, Generic)

-- | Datatype that represents the input for the https://api.helcim.com/v2/payment/purchase API endpoint.
data PurchaseRequest = PurchaseRequest
    { prqIpAddress       :: Text
    , prqEcommerce       :: Maybe Bool
    , prqTerminalId      :: Maybe Int
    , prqCurrency        :: Text
    , prqAmount          :: Scientific
    , prqCustomerCode    :: Maybe CustomerCode
    , prqInvoiceNumber   :: Maybe Text
    , prqBillingAddress  :: Maybe Address
    , prqInvoice         :: Maybe Invoice
    , prqCardData        :: CardData
    } deriving (Show, Generic)

data Shipping = Shipping
    { sPickup         :: Maybe Bool
    , sFreightAmount  :: Maybe Scientific
    } deriving (Show, Generic)

data Status
    = Approved
    | Declined
    deriving (Show, Eq)

instance FromJSON Status where
    parseJSON (Aeson.String "APPROVED") = pure Approved
    parseJSON (Aeson.String "DECLINED") = pure Declined
    parseJSON _ = fail "Invalid Status"

instance ToJSON Status where
    toJSON Approved = Aeson.String "APPROVED"
    toJSON Declined = Aeson.String "DECLINED"

newtype TransactionId = TransactionId { getTransactionId :: Int }
    deriving (Show, Generic)
    deriving newtype (FromJSON, ToJSON, PersistField, PersistFieldSql)

data RefundRequest = RefundRequest
    { rrOriginalTransactionId :: TransactionId
    , rrAmount                :: Scientific
    , rrIpAddress             :: Text
    , rrEcommerce             :: Maybe Bool
    } deriving (Show, Generic)

-- | Datatype that represents the response from the https://api.helcim.com/v2/payment/{purchase, refund} API endpoint.
data PaymentResponse = PaymentResponse
    { preTransactionId  :: TransactionId
    , preCardBatchId    :: Int
    , preDateCreated    :: Text
    , preStatus         :: Status
    , preUser           :: Text
    , preType           :: PaymentType
    , preAmount         :: Scientific
    , preAvsResponse    :: Text
    , preCvvResponse    :: Text
    , preCardType       :: Text
    , preApprovalCode   :: Text
    , preCardToken      :: CardToken
    , preCardNumber     :: Text
    , preCardHolderName :: Text
    , preInvoiceNumber  :: Text
    , preWarning        :: Text
    } deriving (Show, Generic)

deriveToJSON helcimAesonOptions ''CardData
deriveToJSON helcimAesonOptions ''PurchaseRequest
deriveToJSON helcimAesonOptions ''RefundRequest
deriveToJSON helcimAesonOptions ''PaymentResponse

deriveFromJSON helcimAesonOptions ''PaymentResponse
