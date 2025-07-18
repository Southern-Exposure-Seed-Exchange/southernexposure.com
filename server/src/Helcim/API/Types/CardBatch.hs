{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Helcim.API.Types.CardBatch
    ( CardBatchId(..)
    , CardBatchResponse(..)
    )where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Servant (ToHttpApiData)

import Helcim.API.Types.Utils (helcimAesonOptions)

newtype CardBatchId = CardBatchId { unCardBatchId :: Int }
    deriving (Show, Generic)
    deriving newtype (FromJSON, ToJSON, ToHttpApiData)

data CardBatchResponse = CardBatchResponse
    { cbrId                  :: CardBatchId
    , cbrDateCreated         :: Text -- We don't use the time part at the moment, so we decode bare Text
    , cbrDateUpdated         :: Text -- We don't use the time part at the moment, so we decode bare Text
    , cbrDateClosed          :: Text -- We don't use the time part at the moment, so we decode bare Text
    , cbrClosed              :: Bool
    , cbrTerminalId          :: Int
    , cbrBatchNumber         :: Int
    , cbrNetSales            :: Maybe Scientific
    , cbrTotalRefunds        :: Maybe Scientific
    , cbrTotalReversed       :: Maybe Scientific
    , cbrTotalRefundReversed :: Maybe Scientific
    , cbrCountApproved       :: Maybe Int -- Expects '1' or '0'
    , cbrCountDeclined       :: Maybe Int -- Expects '1' or '0'
    }

deriveJSON helcimAesonOptions ''CardBatchResponse
