module Postgrid.API.Utils (aesonOptions) where

import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.Types (Options(..))


aesonOptions :: Options
aesonOptions = (aesonPrefix camelCase) { omitNothingFields = True }
