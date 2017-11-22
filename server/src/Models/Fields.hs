{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Models.Fields where

import Control.Applicative ((<|>))
import Control.Monad (fail)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject, withText)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.Ratio ((%), numerator, denominator)
import Data.StateCodes (StateCode)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(SqlString))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Read (readMaybe, readPrec)

import Models.ProvinceCodes (ProvinceCode)

import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Models.ProvinceCodes as ProvinceCodes
import qualified Web.Stripe.Types as Stripe


-- | Cents are used to do any currency-related arithmetic & are represented
-- as arbitrary-percision numbers.
newtype Cents =
    Cents { fromCents :: Natural }
    deriving (Show, Read, Eq, Ord, Num, ToJSON, FromJSON, PersistField, PersistFieldSql)

formatCents :: Cents -> T.Text
formatCents (Cents c) =
    T.pack . ("$" ++) . formatRational 2 $ fromIntegral c % 100

formatRational :: Int -> Rational -> String
formatRational len rat =
    (if num < 0 then "-" else "") ++ shows d ("." ++ decimalPart)
    where (d, next) = abs num `quotRem` den
          num = numerator rat
          den = denominator rat
          dec = take len $ go next
          decimalPart =
              if length dec < len then
                dec ++ replicate (len - length dec) '0'
              else
                dec
          go 0 = ""
          go x = let (d_, next_) = (10 * x) `quotRem` den
              in shows d_ (go next_)

-- | Convert `Cents` into a Stripe `Amount`.
toStripeAmount :: Cents -> Stripe.Amount
toStripeAmount = Stripe.Amount . fromIntegral . fromCents


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
    parseJSON u =
        parseState u
        <|> parseArmedForces u
        <|> parseProvince u
        <|> parseCustom u
        where parseState =
                withObject "State" $ \v ->
                    v .: "state"
                    >>= maybeM "Invalid US State Code" USState
                        . StateCodes.fromMText
              parseArmedForces =
                withObject "USArmedForces" $ \v ->
                    v .: "armedForces"
                    >>= maybeM "Invalid US Armed Forces Code" USArmedForces
                        . readMaybe
              parseProvince =
                  withObject "CAProvince" $ \v ->
                    v .: "province"
                    >>= maybeM "Invalid CA Province Code" CAProvince
                        . readMaybe
              parseCustom =
                  withObject "CustomRegion" $ \v ->
                    CustomRegion <$> v .: "custom"
              maybeM errorStr constructor =
                  maybe (fail errorStr) (return . constructor)

regionName :: Region -> T.Text
regionName = \case
    USState code ->
        StateCodes.toName code
    USArmedForces code ->
        armedForcesRegion code
    CAProvince code ->
        ProvinceCodes.toName code
    CustomRegion region ->
        region


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


-- SHIPPING RATES


type Threshold = Cents
type Percent = Natural

data ShippingRate
    = Flat Threshold Cents
    | Percentage Threshold Percent
    deriving (Show, Read, Generic)

derivePersistField "ShippingRate"


-- ADDRESSES

data AddressType
    = Billing
    | Shipping
    deriving (Show, Read, Eq, Generic)

derivePersistField "AddressType"


-- STRIPE

newtype StripeCustomerId =
    StripeCustomerId { fromStripeCustomerId :: Stripe.CustomerId }

instance PersistField StripeCustomerId where
    toPersistValue (StripeCustomerId (Stripe.CustomerId customerId)) =
        toPersistValue customerId
    fromPersistValue =
        fmap (StripeCustomerId . Stripe.CustomerId) <$> fromPersistValue

instance PersistFieldSql StripeCustomerId where
    sqlType _ = SqlString


newtype StripeChargeId =
    StripeChargeId { fromStripeChargeId :: Stripe.ChargeId }

instance Read StripeChargeId where
    readPrec =
        StripeChargeId . Stripe.ChargeId <$> readPrec

instance Show StripeChargeId where
    show =
        show . (\(Stripe.ChargeId c) -> c) . fromStripeChargeId

instance PersistField StripeChargeId where
    toPersistValue (StripeChargeId (Stripe.ChargeId chargeId)) =
        toPersistValue chargeId
    fromPersistValue =
        fmap (StripeChargeId . Stripe.ChargeId) <$> fromPersistValue

instance PersistFieldSql StripeChargeId where
    sqlType _ = SqlString

instance ToJSON StripeChargeId where
    toJSON =
        toJSON . (\(Stripe.ChargeId c) -> c) . fromStripeChargeId
instance FromJSON StripeChargeId where
    parseJSON j =
        StripeChargeId . Stripe.ChargeId <$> parseJSON j


-- ORDERS


data OrderStatus
    = OrderReceived
    | PaymentReceived
    | PaymentFailed
    | Processing
    | Refunded
    | Delivered
    deriving (Show, Read, Generic)

instance ToJSON OrderStatus
instance FromJSON OrderStatus

derivePersistField "OrderStatus"


data LineItemType
    = ShippingLine
    | SurchargeLine
    deriving (Show, Read, Generic)

instance ToJSON LineItemType
instance FromJSON LineItemType

derivePersistField "LineItemType"
