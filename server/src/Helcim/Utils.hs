{-# LANGUAGE RecordWildCards #-}
module Helcim.Utils where

import Data.Text (Text)

import qualified Helcim.API.Types.Common as Helcim (Address(..))
import Models.DB (Address(..))
import Models.Fields (AddressType(..))

-- addressToHelcimAddress :: Address -> Text -> Helcim.Address
-- addressToHelcimAddress Address {..} email = Helcim.Address
--   { aName = if null companyName then firstName <> " " <> lastName else companyName
--   , aStreet1 = addressOne
--   , aStreet2 = Just addressTwo
--   , aCity = city
--   , aProvince = Just state
--   , aCountry = country
--   , aPostalCode = zipCode
--   , aPhone = Just phoneNumber
--   , aEmail = Just email
--   }
