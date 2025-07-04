module Helcim.API.Types.Utils
    ( helcimAesonOptions
    ) where

import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.Types (Options(..))

-- | Custom Aeson options for Helcim types.
--
-- These options remove the prefix from field names and omit fields with 'Nothing' values.
helcimAesonOptions :: Options
helcimAesonOptions = (aesonPrefix camelCase) { omitNothingFields = True }
