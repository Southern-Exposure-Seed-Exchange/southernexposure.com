{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.ProvinceCodes
    ( ProvinceCode(..)
    , fromMName
    ) where

-- ISO 3166-2:CA 2-letter codes for Canadian Provinces.

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
    deriving (Show, Read, Eq, Generic)


instance ToJSON ProvinceCode
instance FromJSON ProvinceCode


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
