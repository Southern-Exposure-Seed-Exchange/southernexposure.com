{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Postgrid.Utils
    ( addressDataToVerifyStructuredAddressRequest
    , correctAddressData
    ) where

import Data.ISO3166_CountryCodes (CountryCode(CA, US))
import Data.Text as T (null, pack)

import Postgrid.API.Types
    ( CountryCodeLowercase(..), VerifyStructuredAddressData(..)
    , VerifyStructuredAddressRequest(..), VerifyStructuredAddressRequestData(..))
import Models.Fields (regionCode, Country(..), Region(..))
import Routes.CommonData (AddressData(..))

addressDataToVerifyStructuredAddressRequest :: AddressData -> VerifyStructuredAddressRequest
addressDataToVerifyStructuredAddressRequest AddressData {..} = VerifyStructuredAddressRequest $
    VerifyStructuredAddressRequestData
        { vsardRecipient = if T.null adCompanyName then Nothing else Just adCompanyName
        , vsardLine1 = adAddressOne
        , vsardLine2 = if T.null adAddressTwo then Nothing else Just adAddressTwo
        , vsardCity = adCity
        , vsardProvinceOrState = let country = fromCountry adCountry in
            if country == US || country == CA then regionCode adState else T.pack $ show adState
        , vsardPostalOrZip = adZipCode
        , vsardCountry = T.pack $ show adCountry
        }

correctAddressData :: AddressData -> VerifyStructuredAddressData -> AddressData
correctAddressData addrData vsaData = addrData
    { adAddressOne = vsadLine1 vsaData
    , adAddressTwo = ""
    , adCity = vsadCity vsaData
    , adState = USState $ vsadProvinceOrState vsaData
    , adZipCode = vsadPostalOrZip vsaData
    , adCountry = maybe (adCountry addrData) (Country . unCountryCodeLowercase) (vsadCountry vsaData)
    }
