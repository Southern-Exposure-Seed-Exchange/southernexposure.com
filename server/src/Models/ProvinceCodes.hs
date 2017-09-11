{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.ProvinceCodes
    ( ProvinceCode(..)
    , all
    , toName
    , fromMName
    ) where

-- ISO 3166-2:CA 2-letter codes for Canadian Provinces.

import Prelude hiding (all)

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import qualified Data.Text as T

data ProvinceCode
    = AB
    | BC
    | MB
    | NB
    | NL
    | NS
    | NT
    | NU
    | ON
    | PE
    | QC
    | SK
    | YT
    deriving (Show, Read, Eq, Enum, Bounded, Generic)


instance ToJSON ProvinceCode
instance FromJSON ProvinceCode

all :: [ProvinceCode]
all =
    enumFrom minBound


toName :: ProvinceCode -> T.Text
toName = \case
    AB ->
        "Alberta"
    BC ->
        "British Columbia"
    MB ->
        "Manitoba"
    NB ->
        "New Brunswick"
    NL ->
        "Newfoundland And Labrador"
    NS ->
        "Nova Scotia"
    NT ->
        "Northwest Territories"
    NU ->
        "Nunavut"
    ON ->
        "Ontario"
    PE ->
        "Prince Edward Island"
    QC ->
        "Quebec"
    SK ->
        "Saskatchewan"
    YT ->
        "Yukon"


fromMName :: T.Text -> Maybe ProvinceCode
fromMName = \case
    "Alberta" ->
        Just AB
    "British Columbia" ->
        Just BC
    "Manitoba" ->
        Just MB
    "New Brunswick" ->
        Just NB
    "Newfoundland and Labrador" ->
        Just NL
    "Nova Scotia" ->
        Just NS
    "Northwest Territories" ->
        Just NT
    "Nunavut" ->
        Just NU
    "Ontario" ->
        Just ON
    "Prince Edward Island" ->
        Just PE
    "Quebec" ->
        Just QC
    "Saskatchewan" ->
        Just SK
    "Yukon" ->
        Just YT
    _ ->
        Nothing
