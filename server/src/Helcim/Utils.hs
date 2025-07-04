{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helcim.Utils
    ( addressToHelcimAddress
    ) where

import Data.ISO3166_CountryCodes (CountryCode(CA, US))
import Data.Text as T (null)

import qualified Helcim.API.Types.Common as Helcim (Address(..))
import Models.DB (Address(..))
import Models.Fields (regionCode, Country(fromCountry))

addressToHelcimAddress :: Address -> Helcim.Address
addressToHelcimAddress Address {..} = Helcim.Address
    { aName = if T.null addressCompanyName then addressFirstName <> " " <> addressLastName else addressCompanyName
    , aStreet1 = addressAddressOne
    , aStreet2 = Just addressAddressTwo
    , aCity = Just addressCity
    , aProvince = let country = fromCountry addressCountry in
        if country == US || country == CA then Just $ regionCode addressState else Nothing
    , aCountry = case fromCountry addressCountry of
        US -> Just "USA"
        CA -> Just "CAN"
        _ -> Nothing
    , aPostalCode = addressZipCode
    -- Helcim API expected the phone to be well-formated, however, the input data may not always meet this requirement.
    -- so we set it to Nothing
    , aPhone = Nothing
    -- Address entity in the backend does not have an email field, so we set it to Nothing
    , aEmail = Nothing
    }
