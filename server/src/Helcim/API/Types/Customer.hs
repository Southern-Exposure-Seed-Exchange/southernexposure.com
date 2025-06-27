{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Helcim.API.Types.Customer where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (ToHttpApiData)

import Helcim.API.Types.Common
import Helcim.API.Types.Utils (helcimAesonOptions)
import Database.Esqueleto.Experimental (PersistField, PersistFieldSql)

newtype CustomerId = CustomerId { unCustomerId :: Int }
    deriving (Show, Eq)
    deriving newtype (FromJSON, ToJSON, ToHttpApiData, PersistField, PersistFieldSql)

newtype CustomerCode = CustomerCode { unCustomerCode :: Text }
    deriving (Show, Eq)
    deriving newtype (FromJSON, ToJSON)

-- | Datatype that represents the input for the https://api.helcim.com/v2/customers API endpoint.
data CreateCustomerRequest = CreateCustomerRequest
    { ccrCustomerCode    :: Maybe CustomerCode
    , ccrContactName     :: Maybe Text
    , ccrBusinessName    :: Maybe Text
    , ccrCellPhone       :: Maybe Text
    , ccrBillingAddress  :: Maybe Address
    , ccrShippingAddress :: Maybe Address
    }
    deriving (Show, Generic)

data CustomerCard = CustomerCard
    { ccCardHolderName :: Text
    , ccCardF6L4       :: Text  -- First 6 and Last 4 digits of the card number
    , ccCardToken      :: CardToken
    }
    deriving (Show, Generic)

-- | Datatype that represents the response from the https://api.helcim.com/v2/customers/{customerId} API endpoint.
data CustomerResponse = CustomerResponse
    { creId              :: CustomerId
    , creCustomerCode    :: CustomerCode
    , creContactName     :: Maybe Text
    , creBusinessName    :: Maybe Text
    , creCellPhone       :: Maybe Text
    , creBillingAddress  :: Maybe Address
    , creShippingAddress :: Maybe Address
    , creCards           :: [CustomerCard]
    }
    deriving (Show, Generic)

deriveToJSON helcimAesonOptions ''CreateCustomerRequest
deriveFromJSON helcimAesonOptions ''CustomerCard
deriveFromJSON helcimAesonOptions ''CustomerResponse
