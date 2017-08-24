{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Models.Classes
    ( JSONList(..)
    , JSONObject(..)
    ) where

import Control.Monad (mzero)
import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), Value(..), object)
import Data.Proxy (Proxy(..))
import Database.Persist (Entity)

import Models.DB

import qualified Data.Text as T


-- | The Named typeclass is used to automatically generate the key to nest
-- objects under when returning a JSON response.
class Named a where
    name :: Proxy a -> T.Text

instance Named Product where
    name _ = "products"

-- | The `Entity a` instance of the Named typeclass uses the Named
-- instance for `a`.
instance Named a => Named (Entity a) where
    name _ = name (Proxy :: Proxy a)



-- | The JSONList type represents a list of objects to parse & display as
-- JSON.
newtype JSONList a =
    JSONList [a]

-- | The JSONList type uses the `Named a` typeclass to parse the JSON
-- objects.
instance (Named a, FromJSON a) => FromJSON (JSONList a) where
    parseJSON (Object o) = do
        namedObjects <- o .: name (Proxy :: Proxy a) >>= parseJSON
        return $ JSONList namedObjects
    parseJSON _ =
        mzero

-- | The JSONList type uses the `Named a` typeclass to generate the JSON
-- object.
instance (Named a, ToJSON a) => ToJSON (JSONList a) where
    toJSON (JSONList l) =
        object [ name (Proxy :: Proxy a) .= map toJSON l ]



-- | The JSONObject type represent a single object to parse & display as JSON.
newtype JSONObject a =
    JSONObject a

-- | The JSONObject type uses the `Named a` typeclass to parse the JSON
-- objects.
instance (Named a, FromJSON a) => FromJSON (JSONObject a) where
    parseJSON (Object o) = do
        namedObject <- o .: name (Proxy :: Proxy a) >>= parseJSON
        return $ JSONObject namedObject
    parseJSON _ =
        mzero

-- The JSONObject type uses the `Named a` typeclass to generate the JSON
-- object.
instance (Named a, ToJSON a) => ToJSON (JSONObject a) where
    toJSON (JSONObject o) =
        object [ name (Proxy :: Proxy a) .= toJSON o ]
