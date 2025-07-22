{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Models.Fields where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject, withText)
import Data.ISO3166_CountryCodes (CountryCode)
import Data.Ratio ((%), numerator, denominator)
import Data.Scientific (Scientific, scientific)
import Data.StateCodes (StateCode)
import Data.Time (UTCTime)
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(SqlString))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Text.Read (readMaybe, readPrec)
import Data.Coerce (coerce)

import qualified Avalara
import qualified Data.CAProvinceCodes as CACodes
import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Web.Stripe.Types as Stripe
import Data.Int
import qualified Data.Foldable
import GHC.Stack
import Control.Monad

-- | Cents are used to do any currency-related arithmetic & are represented
-- as arbitrary-percision numbers.
newtype Cents =
    Cents { fromCents :: Int64 }
    deriving newtype (Eq, Ord, ToJSON, Show, PersistField, PersistFieldSql)

instance Read Cents where
    readPrec = do
        val <- readPrec
        when (val < 0) $ fail "Negative cents aren't allowed"
        pure (Cents val)

instance FromJSON Cents where
    parseJSON v = do
        val <- parseJSON v
        when (val < 0) $ fail "Negative cents aren't allowed"
        pure (Cents val)

timesQuantity :: HasCallStack => Cents -> Int64 -> Cents
timesQuantity x y
    | y < 0 = error ("Negative quantity isn't allowed: " <> show x <> ", " <> show y)
    | otherwise = coerce ((*) @Int64) x y

plusCents :: Cents -> Cents -> Cents
plusCents = coerce ((+) @Int64)

subtractCents :: HasCallStack => Cents -> Cents -> Cents
subtractCents x y
    | x < y = error ("Negatvie cents aren't allowed: " <> show x <> ", " <> show y)
    | otherwise = coerce ((-) @Int64) x y

infixl 6 `plusCents`     -- the same as +
infixl 6 `subtractCents` -- the same as -
infixl 7 `timesQuantity` -- the same as *

sumPrices :: Foldable f => f Cents -> Cents
sumPrices = Data.Foldable.foldl' plusCents (Cents 0)
{-# SPECIALISE sumPrices :: [Cents] -> Cents #-}

mkCents :: Int64 -> Cents
mkCents = coerce

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

-- | Convert 'Cents' into a Decimal-based Dollar amount.
toDollars :: Cents -> Scientific
toDollars (Cents c) =
    scientific (fromIntegral c) (-2)

-- | Convert a Decimal-based Dollar amount into a whole number of Cents.
-- Truncates any amount past the 2nd decimal place.
fromDollars :: Scientific -> Cents
fromDollars dollars =
    Cents $ floor $ abs $ dollars * 100


-- | Milligrams are used to do any weight-related arithmetic & are
-- represented as arbitrary-percision numbers.
newtype Milligrams =
    Milligrams { fromMilligrams :: Int64 }
    deriving newtype (Eq, Ord, ToJSON, FromJSON, Show, Read, PersistField, PersistFieldSql)

mkMilligrams :: Int64 -> Milligrams
mkMilligrams = coerce

-- | Prettify the milligrams by turning some values into pounds.
-- Note: When changing matches, update the client code in Models.Fields as
-- well.
renderMilligrams :: Milligrams -> T.Text
renderMilligrams (Milligrams mg) =
    case mg of
        84000 ->
            "3 oz"
        114000 ->
            "¼ lb"
        171000 ->
            "6 oz"
        228000 ->
            "½ lb"
        342000 ->
            "¾ lb"
        454000 ->
            "1 lb"
        568000 ->
            "1¼ lbs"
        680000 ->
            "1½ lbs"
        908000 ->
            "2 lbs"
        1135000 ->
            "2½ lbs"
        1140000 ->
            "2½ lbs"
        1816000 ->
            "4 lbs"
        2270000 ->
            "5 lbs"
        _ ->
            let
                (wholePart, fractionalPart) = mg `divMod` 1000
                fractionalString = stripZeroes $ T.pack $ show fractionalPart
                decimalPart =
                    if T.null fractionalString then
                        ""
                    else
                        "." <> fractionalString
            in
                T.pack (show wholePart) <> decimalPart <> " g"
  where
    stripZeroes :: T.Text -> T.Text
    stripZeroes t =
        if T.takeEnd 1 t == "0" then
            stripZeroes $ T.dropEnd 1 t
        else
            t



-- LOT SIZES

-- | The lot size indicates how much of a ProductVariant a customer
-- receives when they order one of an item. E.g., 28.5 grams, 3 slips, etc.
-- Discrete types are available for the most common classifications, but
-- custom types are possible via the 'CustomLotSize' value.
data LotSize
    = Mass Milligrams
    | Bulbs Integer
    | Slips Integer
    | Plugs Integer
    | CustomLotSize T.Text
    deriving (Show, Read, Eq)

derivePersistField "LotSize"

-- | Encode the type & amount into an object. E.g., @{ "type": "mass",
-- "size": 28500 }@.
instance ToJSON LotSize where
    toJSON ls =
        let (type_, size) = case ls of
                Mass s ->
                    ("mass" :: T.Text, toJSON s)
                Bulbs s ->
                    ("bulbs", toJSON s)
                Slips s ->
                    ("slips", toJSON s)
                Plugs s ->
                    ("plugs", toJSON s)
                CustomLotSize s ->
                    ("custom", toJSON s)
        in  object ["type" .= type_, "size" .= size]

-- | Invert the ToJSON instance.
instance FromJSON LotSize where
    parseJSON = withObject "LotSize" $ \v ->
        v .: "type" >>= \case
            "mass" ->
                Mass <$> v .: "size"
            "bulbs" ->
                Bulbs <$> v .: "size"
            "slips" ->
                Slips <$> v .: "size"
            "plugs" ->
                Plugs <$> v .: "size"
            "custom" ->
                CustomLotSize <$> v .: "size"
            unexpected ->
                fail $ "Unexpected LotSize type: " ++ T.unpack unexpected

-- | TODO: Should we just send the final string in the JSON?
renderLotSize :: LotSize -> T.Text
renderLotSize = \case
    Mass mg ->
        renderMilligrams mg
    Bulbs i ->
        T.pack (show i) <> " Bulbs"
    Slips i ->
        T.pack (show i) <> " Slips"
    Plugs i ->
        T.pack (show i) <> " Plugs"
    CustomLotSize t ->
        t


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
    | CAProvince CACodes.Code
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
        CACodes.toName code
    CustomRegion region ->
        region


-- Convert the Region to a 2-letter code textual representation.
regionCode :: Region -> T.Text
regionCode = \case
    USState code ->
        StateCodes.toText code
    USArmedForces code -> case code of
        AA -> "AA"
        AE -> "AE"
        AP -> "AP"
    CAProvince code -> T.pack (show code)
    CustomRegion region ->
        T.toUpper region

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

type Percent = Int64

data ShippingRate
    = Flat Threshold Cents
    | Percentage Threshold Percent -- ^ Percent field is a whole percent(`5 == 5%`).
    deriving (Show, Read, Generic)

derivePersistField "ShippingRate"

-- | The Flat Fee & Percentage Fee for adding Priority S&H to an Order.
data PriorityShippingFee
    = PriorityShippingFee Cents Percent -- ^ Percent field is a whole percent(`1 == 1%`).
    deriving (Show, Read, Generic)

derivePersistField "PriorityShippingFee"

instance ToJSON PriorityShippingFee where
    toJSON (PriorityShippingFee flat percent) =
        object [ "flat" .= flat, "percent" .= percent ]


-- ADDRESSES

data AddressType
    = Billing
    | Shipping
    deriving (Show, Read, Eq, Generic)

derivePersistField "AddressType"


-- STRIPE

newtype StripeCustomerId =
    StripeCustomerId { fromStripeCustomerId :: Stripe.CustomerId }
    deriving (Show)

instance PersistField StripeCustomerId where
    toPersistValue (StripeCustomerId (Stripe.CustomerId customerId)) =
        toPersistValue customerId
    fromPersistValue =
        fmap (StripeCustomerId . Stripe.CustomerId) <$> fromPersistValue

instance PersistFieldSql StripeCustomerId where
    sqlType _ = SqlString

instance ToJSON StripeCustomerId where
    toJSON (StripeCustomerId (Stripe.CustomerId customerId)) =
        toJSON customerId


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


-- AVALARA

newtype AvalaraCustomerCode =
    AvalaraCustomerCode { fromAvalaraCustomerCode :: Avalara.CustomerCode }
    deriving (Show, Read, Eq)

instance ToJSON AvalaraCustomerCode where
    toJSON = toJSON . fromAvalaraCustomerCode

instance PersistField AvalaraCustomerCode where
    toPersistValue (AvalaraCustomerCode (Avalara.CustomerCode customerCode)) =
        toPersistValue customerCode
    fromPersistValue =
        fmap (AvalaraCustomerCode . Avalara.CustomerCode) <$> fromPersistValue

instance PersistFieldSql AvalaraCustomerCode where
    sqlType _ = SqlString


data AvalaraTransactionCode =
    AvalaraTransactionCode
        { avalaraCompanyCode :: Avalara.CompanyCode
        , avalaraTransactionCode :: Avalara.TransactionCode
        } deriving (Show, Read, Eq, Generic)

instance FromJSON AvalaraTransactionCode
instance ToJSON AvalaraTransactionCode

derivePersistField "AvalaraTransactionCode"


-- COUPONS

data CouponType
    = FlatDiscount Cents
    | PercentageDiscount Percent    -- ^ Percent field is whole percentage(`5 == 5`).
    | FreeShipping
    deriving (Show, Read, Eq, Generic)

derivePersistField "CouponType"

instance ToJSON CouponType where
    toJSON = \case
        FlatDiscount amount ->
            object
                [ "type" .= ("flat" :: T.Text)
                , "amount" .= amount
                ]
        PercentageDiscount percent ->
            object
                [ "type" .= ("percentage" :: T.Text)
                , "amount" .= percent
                ]
        FreeShipping ->
            object
                [ "type" .= ("shipping" :: T.Text)
                ]

instance FromJSON CouponType where
    parseJSON = withObject "CouponType" $ \v ->
        v .: "type" >>= \case
            ("flat" :: T.Text) ->
                FlatDiscount <$> v .: "amount"
            "percentage" ->
                PercentageDiscount <$> v .: "amount"
            "shipping" ->
                return FreeShipping
            str ->
                fail $ "Unexpected CouponType: " ++ T.unpack str


-- ORDERS


data OrderStatus
    = OrderReceived
    | PaymentReceived
    | PaymentFailed
    | Processing
    | Refunded
    | Delivered
    deriving (Show, Read, Generic)

showOrderStatus :: OrderStatus -> T.Text
showOrderStatus = \case
    OrderReceived -> "Order Received"
    PaymentReceived -> "Payment Received"
    PaymentFailed -> "Payment Failed"
    Processing -> "Processing"
    Refunded -> "Refunded"
    Delivered -> "Shipped"

instance ToJSON OrderStatus
instance FromJSON OrderStatus

derivePersistField "OrderStatus"


data LineItemType
    = ShippingLine
    | PriorityShippingLine
    | SurchargeLine
    | StoreCreditLine
    | MemberDiscountLine
    | CouponDiscountLine
    | RefundLine
    | TaxLine
    deriving (Show, Eq, Read, Generic, Enum, Bounded)

instance ToJSON LineItemType
instance FromJSON LineItemType

derivePersistField "LineItemType"

-- | An enumeration of `LineItemType`s containing only credits.
creditLineItemTypes :: [LineItemType]
creditLineItemTypes =
    filter isCreditLine [minBound .. maxBound]
 where
    isCreditLine = \case
        StoreCreditLine ->
            True
        MemberDiscountLine ->
            True
        CouponDiscountLine ->
            True
        RefundLine ->
            True
        ShippingLine ->
            False
        PriorityShippingLine ->
            False
        SurchargeLine ->
            False
        TaxLine ->
            False


-- | Admin comments added to an Order, containing the message & creation time.
data AdminOrderComment =
    AdminOrderComment
        { adminCommentContent :: T.Text
        , adminCommentTime :: UTCTime
        } deriving (Show, Read, Generic)

derivePersistField "AdminOrderComment"

instance ToJSON AdminOrderComment where
    toJSON comment =
        object
            [ "content" .= adminCommentContent comment
            , "time" .= adminCommentTime comment
            ]



-- CATEGORY SALES

data SaleType
    = FlatSale Cents
    | PercentSale Percent       -- ^ Percent field is whole percentage(`5 == 5%`)
    deriving (Show, Read, Generic, Eq)

derivePersistField "SaleType"

instance ToJSON SaleType where
    toJSON sale =
        let
            (type_, amount) =
                case sale of
                    FlatSale discount ->
                        ("flat" :: T.Text, fromCents discount)
                    PercentSale percent ->
                        ("percent", percent)
        in
            object ["type" .= type_, "amount" .= amount]

instance FromJSON SaleType where
    parseJSON = withObject "SaleType" $ \v ->
        v .: "type" >>= \case
            "flat" ->
                FlatSale <$> v .: "amount"
            "percent" ->
                PercentSale <$> v .: "amount"
            unexpected ->
                fail $ "Unexpected SaleType type: " ++ T.unpack unexpected
