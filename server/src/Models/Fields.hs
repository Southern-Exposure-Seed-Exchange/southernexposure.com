{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Models.Fields where

import Control.Monad (fail, join)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?), object, withObject, withText)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.StateCodes (StateCode)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(SqlString))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Models.ProvinceCodes (ProvinceCode)

import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Models.ProvinceCodes as ProvinceCodes


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


-- LOCATIONS


data ArmedForcesRegionCode
    = AA
    | AE
    | AP
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance ToJSON ArmedForcesRegionCode
instance FromJSON ArmedForcesRegionCode

armedForcesRegion :: ArmedForcesRegionCode -> T.Text
armedForcesRegion = \case
    AA ->
        "Armed Forces Americas"
    AE ->
        "Armed Forces Europe, Middle East, Africa, & Canada"
    AP ->
        "Armed Forces Pacific"


data Region
    = USState StateCode
    | USArmedForces ArmedForcesRegionCode
    | CAProvince ProvinceCode
    | CustomRegion T.Text
    deriving (Show, Read, Eq, Generic)

derivePersistField "Region"

instance ToJSON Region where
    toJSON (USState code) =
        object [ "state" .= toJSON code ]
    toJSON (USArmedForces code)  =
        object ["armedForces" .= toJSON code ]
    toJSON (CAProvince province)=
        object [ "province" .= toJSON province ]
    toJSON (CustomRegion region) =
        object [ "custom" .= toJSON region ]

instance FromJSON Region where
    -- TODO: Cleaner way to do this.. Use Either type instead of Maybe?
    parseJSON = withObject "Region" $ \v -> do
        maybeState <- v .:? "state"
        case maybeState of
            Just state ->
                maybe (fail "Invalid US State Code") (return . USState)
                    $ StateCodes.fromMText state
            Nothing -> do
                maybeAF <- v .:? "armedForces"
                case maybeAF of
                    Just afCode ->
                        maybe (fail $ "Invalid US Armed Forces Code: " ++ afCode)
                            (return . USArmedForces)
                            $ readMaybe afCode
                    Nothing -> do
                        maybeProvince <- join . fmap ProvinceCodes.fromMName
                                            <$> (v .:? "province")
                        case maybeProvince of
                            Just province ->
                                return $ CAProvince province
                            Nothing -> do
                                maybeCustom <- v .:? "custom"
                                case maybeCustom of
                                    Just custom ->
                                        return $ CustomRegion custom
                                    Nothing ->
                                        fail "Invalid Region Format, No Matching Key"


newtype Country =
    Country { fromCountry :: CountryCode }
    deriving (Show, Read, Eq, Ord)

instance PersistField Country where
    toPersistValue = toPersistValue . show . fromCountry
    fromPersistValue val = do
        value <- fromPersistValue val
        maybe (Left "Couldn't parse Country Code") (Right . Country)
            $ readMaybe value

instance PersistFieldSql Country where
    sqlType _ = SqlString

instance ToJSON Country where
    toJSON =
        toJSON . show . fromCountry

instance FromJSON Country where
    parseJSON = withText "Country" $ \t ->
        case readMaybe (T.unpack t) of
            Just countryCode ->
                return $ Country countryCode
            Nothing ->
                fail $ "Invalid Country Code: " ++ T.unpack t
