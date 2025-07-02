{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Helcim.API.Types.Customer where

import Data.Aeson (FromJSON(..), ToJSON, withObject, (.:), (.:?), Value(..))
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

-- When address (either billing or shipping) is not specified, Helcim returns an empty string.
-- Despite the fact that the API documentation says it has 'object' type.
-- Hence we need custom FromJSON instance to handle this case.
instance FromJSON CustomerResponse where
    parseJSON = withObject "CustomerResponse" $ \o -> do
        creId              <- o .: "id"
        creCustomerCode    <- o .: "customerCode"
        creContactName     <- o .:? "contactName"
        creBusinessName    <- o .:? "businessName"
        creCellPhone       <- o .:? "cellPhone"

        billingAddress  <- o .: "billingAddress"
        creBillingAddress <- case billingAddress of
            String "" -> pure Nothing
            v -> Just <$> parseJSON v
        shippingAddress <- o .: "shippingAddress"
        creShippingAddress <- case shippingAddress of
            String "" -> pure Nothing
            v -> Just <$> parseJSON v
        creCards           <- o .: "cards"
        return CustomerResponse {..}

deriveToJSON helcimAesonOptions ''CreateCustomerRequest
deriveFromJSON helcimAesonOptions ''CustomerCard
