{-# LANGUAGE OverloadedStrings #-}
{- | This module is the implementation of the PersistField instances
for the JSON Value type from a more recent version of the
'persistent-postgresql' package, wrapped in a newtype to prevent orphan
instances.

This module should be removed once we move to a Stack LTS containing
v2.8.2+ of the package.

-}
module Models.PersistJSON
    ( JSONValue(..)
    ) where

import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Proxy (Proxy)
import Database.Persist.Sql

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

newtype JSONValue =
    JSONValue
        { fromJSONValue :: Value
        }

instance Show JSONValue where
    show = show . fromJSONValue

instance Read JSONValue where
    readsPrec i s = first JSONValue <$> readsPrec i s

instance ToJSON JSONValue where
    toJSON = toJSON . fromJSONValue

instance FromJSON JSONValue where
    parseJSON = return . JSONValue


instance PersistField JSONValue where
  toPersistValue = toPersistValueJsonB . fromJSONValue
  fromPersistValue = fmap JSONValue . fromPersistValueJsonB

instance PersistFieldSql JSONValue where
  sqlType = sqlTypeJsonB

toPersistValueJsonB :: ToJSON a => a -> PersistValue
toPersistValueJsonB = PersistLiteralEscaped . LBS.toStrict . encode

fromPersistValueJsonB :: FromJSON a => PersistValue -> Either Text a
fromPersistValueJsonB (PersistText t) =
    case eitherDecodeStrict $ encodeUtf8 t of
      Left str -> Left $ fromPersistValueParseError "FromJSON" t $ T.pack str
      Right v -> Right v
fromPersistValueJsonB (PersistByteString bs) =
    case eitherDecodeStrict bs of
      Left str -> Left $ fromPersistValueParseError "FromJSON" bs $ T.pack str
      Right v -> Right v
fromPersistValueJsonB x = Left $ fromPersistValueError "FromJSON" "string or bytea" x

-- Constraints on the type might not be necessary,
-- but better to leave them in.
sqlTypeJsonB :: Proxy a -> SqlType
sqlTypeJsonB _ = SqlOther "JSONB"

fromPersistValueError :: Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                      -> Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
                      -> PersistValue -- ^ Incorrect value
                      -> Text -- ^ Error message
fromPersistValueError haskellType databaseType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`; expected "
    , databaseType
    , " from database, but received: "
    , T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

fromPersistValueParseError :: (Show a)
                           => Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                           -> a -- ^ Received value
                           -> Text -- ^ Additional error
                           -> Text -- ^ Error message
fromPersistValueParseError haskellType received err = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`, but received "
    , T.pack (show received)
    , " | with error: "
    , err
    ]
