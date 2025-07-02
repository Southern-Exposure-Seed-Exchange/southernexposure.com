
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helcim.Utils where

import Data.ISO3166_CountryCodes (CountryCode(CA, US))
import Data.Text as T (Text, null)

import qualified Helcim.API.Types.Common as Helcim (Address(..))
import Models.DB (Address(..))
import Models.Fields (regionCode, Country(fromCountry))

addressToHelcimAddress :: Address -> Text -> Helcim.Address
addressToHelcimAddress Address {..} email = Helcim.Address
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
  , aPhone = Nothing
  , aEmail = Just email
  }
