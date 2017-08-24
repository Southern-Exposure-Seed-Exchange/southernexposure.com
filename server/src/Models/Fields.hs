{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Models.Fields where

import Data.Aeson (ToJSON, FromJSON)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Numeric.Natural


-- | Cents are used to do any currency-related arithmetic & are represented
-- as arbitrary-percision numbers.
newtype Cents =
    Cents { fromCents :: Natural }
    deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql)

-- | Milligrams are used to do any weight-related arithmetic & are
-- represented as arbitrary-percision numbers.
newtype Milligrams =
    Milligrams { fromMilligrams :: Natural }
    deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql)
