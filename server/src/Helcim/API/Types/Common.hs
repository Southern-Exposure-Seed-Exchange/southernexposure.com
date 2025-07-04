{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Helcim.API.Types.Common
    ( Address(..)
    , CardToken(..)
    , Discount(..)
    , Invoice(..)
    , LineItem(..)
    , PaymentType(..)
    , Tax(..)
    ) where

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import GHC.Generics (Generic)

import Helcim.API.Types.Utils (helcimAesonOptions)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)

data PaymentType
    = Purchase
    | PreAuth
    | Verify
    | Refund
    | Reverse
    deriving (Show, Eq)

instance ToJSON PaymentType where
    toJSON Purchase = Aeson.String "purchase"
    toJSON PreAuth  = Aeson.String "preauth"
    toJSON Verify   = Aeson.String "verify"
    toJSON Refund   = Aeson.String "refund"
    toJSON Reverse  = Aeson.String "reverse"

instance FromJSON PaymentType where
    parseJSON (Aeson.String "purchase") = pure Purchase
    parseJSON (Aeson.String "preauth")  = pure PreAuth
    parseJSON (Aeson.String "verify")   = pure Verify
    parseJSON (Aeson.String "refund")   = pure Refund
    parseJSON (Aeson.String "reverse")  = pure Reverse
    parseJSON _ = fail "Invalid PaymentType"

data Address = Address
    { aName       :: Text
    , aStreet1    :: Text
    , aStreet2    :: Maybe Text
    , aCity       :: Maybe Text
    , aProvince   :: Maybe Text
    , aCountry    :: Maybe Text
    , aPostalCode :: Text
    , aPhone      :: Maybe Text
    , aEmail      :: Maybe Text
    } deriving (Show, Generic)

data Invoice = Invoice
    { iInvoiceNumber :: Maybe Text
    , iNotes         :: Maybe Text
    , iTipAmount     :: Maybe Scientific
    , iTax           :: Maybe Tax
    , iDiscount      :: Maybe Discount
    , iLineItems     :: Maybe [LineItem]
    } deriving (Show, Generic)

data Tax = Tax
    { tAmount  :: Scientific
    , tDetails :: Text
    } deriving (Show, Generic)

data Discount = Discount
    { dAmount  :: Scientific
    , dDetails :: Text
    } deriving (Show, Generic)

data LineItem = LineItem
    { liSku            :: Maybe Text
    , liDescription    :: Maybe Text
    , liQuantity       :: Int
    , liPrice          :: Scientific
    , liTotal          :: Int
    , liTaxAmount      :: Maybe Scientific
    , liDiscountAmount :: Maybe Scientific
    } deriving (Show, Generic)

newtype CardToken = CardToken { unCardToken :: Text }
    deriving (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

deriveToJSON helcimAesonOptions ''Address
deriveToJSON helcimAesonOptions ''Invoice
deriveToJSON helcimAesonOptions ''Tax
deriveToJSON helcimAesonOptions ''Discount
deriveToJSON helcimAesonOptions ''LineItem

deriveFromJSON helcimAesonOptions ''Address
