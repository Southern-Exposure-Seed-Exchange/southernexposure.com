{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Helcim.API.Types.Customer
    ( CreateCustomerRequest(..)
    , CustomerCard(..)
    , CustomerCode(..)
    , CustomerId(..)
    , CustomerResponse(..)
    , GetCustomersRequest(..)
    ) where

import Data.Aeson (FromJSON(..), Object, ToJSON, withObject, (.:), (.:?), Value(..))
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Data.Aeson.Types (Parser)
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

data GetCustomersRequest = GetCustomersRequest
    { gcrSearch       :: Maybe Text
    , gcrCustomerCode :: Maybe CustomerCode
    , gcrLimit        :: Maybe Int
    , gcrPage         :: Maybe Int
    , gcrIncludeCards :: Maybe Bool
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

instance FromJSON CustomerResponse where
    parseJSON = withObject "CustomerResponse" $ \o -> do
        creId              <- o .: "id"
        creCustomerCode    <- o .: "customerCode"
        creContactName     <- o .:? "contactName"
        creBusinessName    <- o .:? "businessName"
        creCellPhone       <- o .:? "cellPhone"

        creBillingAddress <- o `at` "billingAddress"
        creShippingAddress <- o `at` "shippingAddress"
        creCards           <- o .: "cards"
        return CustomerResponse {..}
        where
            -- When address (either billing or shipping) is not specified, Helcim returns an empty string.
            -- Despite the fact that the API documentation says it has 'object' type.
            -- Hence we need custom FromJSON instance to handle this case.
            at :: FromJSON a => Object -> Text -> Parser (Maybe a)
            at o fld = do
                res <- o .: fld
                case res of
                    String "" -> pure Nothing
                    v -> Just <$> parseJSON v

deriveToJSON helcimAesonOptions ''CustomerCard
deriveToJSON helcimAesonOptions ''CustomerResponse

deriveToJSON helcimAesonOptions ''CreateCustomerRequest
deriveToJSON helcimAesonOptions ''GetCustomersRequest
deriveFromJSON helcimAesonOptions ''CustomerCard
