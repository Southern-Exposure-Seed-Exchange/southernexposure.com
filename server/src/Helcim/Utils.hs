
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helcim.Utils where
    
import Data.ISO3166_CountryCodes (CountryCode(CA, US))
import Data.Text as T (Text, null)

import qualified Helcim.API.Types.Common as Helcim (Address(..))
import Models.DB (Address(..))
import Models.Fields (regionName, Country(fromCountry))

addressToHelcimAddress :: Address -> Text -> Helcim.Address
addressToHelcimAddress Address {..} email = Helcim.Address
  { aName = if T.null addressCompanyName then addressFirstName <> " " <> addressLastName else addressCompanyName
  , aStreet1 = addressAddressOne
  , aStreet2 = Just addressAddressTwo
  , aCity = Just addressCity
  , aProvince = Just $ regionName addressState
  , aCountry = Just $ case fromCountry addressCountry of
        US -> "USA"
        CA -> "CAN"
        _ -> error "unsupported"
  , aPostalCode = addressZipCode
  , aPhone = Just addressPhoneNumber
  , aEmail = Just email
  }
