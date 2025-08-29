{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Postgrid.API.Types
    ( AddressVerificationStatus(..)
    , CountryCodeLowercase(..)
    , PostgridResponse(..)
    , VerifyStructuredAddressData(..)
    , VerifyStructuredAddressErrors(..)
    , VerifyStructuredAddressRequest(..)
    , VerifyStructuredAddressRequestData(..)
    , VerifyStructuredAddressResponse(..)
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.Map.Strict (Map)
import Data.StateCodes (StateCode)
import Data.Text (Text, pack, toLower, toUpper, unpack)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Postgrid.API.Utils (aesonOptions)

-- postgrid uses lowercase country codes
newtype CountryCodeLowercase = CountryCodeLowercase { unCountryCodeLowercase :: CountryCode }
    deriving (Show, Eq)

instance ToJSON CountryCodeLowercase where
    toJSON (CountryCodeLowercase code) = String $ toLower (pack $ show code)

instance FromJSON CountryCodeLowercase where
    parseJSON = withText "Country" $ \t ->
        case readMaybe (unpack $ toUpper t) of
            Just countryCode ->
                return $ CountryCodeLowercase countryCode
            Nothing ->
                fail $ "Invalid Country Code: " ++ unpack t


-- https://postgrid.readme.io/reference/verify-a-structured-address-1

newtype VerifyStructuredAddressRequest = VerifyStructuredAddressRequest
    { vsarqAddress :: VerifyStructuredAddressRequestData
    } deriving (Show, Generic)

data VerifyStructuredAddressRequestData = VerifyStructuredAddressRequestData
    { vsardRecipient       :: Maybe Text
    , vsardLine1           :: Text
    , vsardLine2           :: Maybe Text
    , vsardCity            :: Text
    , vsardProvinceOrState :: Text
    , vsardPostalOrZip     :: Text
    , vsardCountry         :: Text
    } deriving (Show, Generic)

-- Data type for the "errors" field
newtype VerifyStructuredAddressErrors = VerifyStructuredAddressErrors
    { vsaeErrors :: Map Text [Text]
    } deriving (Show, Eq)

instance FromJSON VerifyStructuredAddressErrors where
    parseJSON v = VerifyStructuredAddressErrors <$> parseJSON v

instance ToJSON VerifyStructuredAddressErrors where
    toJSON (VerifyStructuredAddressErrors errs) = toJSON errs

data AddressVerificationStatus
    = AVVerified
    | AVFailed
    | AVCorrected
    deriving (Show, Eq)

instance FromJSON AddressVerificationStatus where
    parseJSON (String "verified") = pure AVVerified
    parseJSON (String "failed") = pure AVFailed
    parseJSON (String "corrected") = pure AVCorrected
    parseJSON _ = fail "Invalid AddressVerificationStatus"

instance ToJSON AddressVerificationStatus where
    toJSON AVVerified  = String "verified"
    toJSON AVFailed   = String "failed"
    toJSON AVCorrected = String "corrected"

-- Data type for the "data" field
data VerifyStructuredAddressData = VerifyStructuredAddressData
    { vsadCity            :: Text
    , vsadCountry         :: Maybe CountryCodeLowercase
    , vsadCountryName     :: Maybe Text
    , vsadErrors          :: VerifyStructuredAddressErrors
    , vsadLine1           :: Text
    , vsadPostalOrZip     :: Text
    , vsadProvinceOrState :: StateCode
    , vsadStatus          :: AddressVerificationStatus
    } deriving (Show, Generic)

class PostgridResponse a where
    getStatus :: a -> Text
    getMessage :: a -> Text

data VerifyStructuredAddressResponse = VerifyStructuredAddressResponse
    { vsarStatus  :: Text
    , vsarMessage :: Text
    , vsarData    :: VerifyStructuredAddressData
    } deriving (Show, Generic)

instance PostgridResponse VerifyStructuredAddressResponse where
    getStatus = vsarStatus
    getMessage = vsarMessage

deriveJSON aesonOptions ''VerifyStructuredAddressData
deriveJSON aesonOptions ''VerifyStructuredAddressResponse
deriveToJSON aesonOptions ''VerifyStructuredAddressRequestData
deriveToJSON aesonOptions ''VerifyStructuredAddressRequest
