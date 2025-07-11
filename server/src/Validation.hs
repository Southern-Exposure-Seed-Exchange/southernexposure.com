{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Validation
    ( Validators
    , Validation(..)
    , singleError
    , singleFieldError
    , validateMap
    , mapCheck
    , prefixFields
    , indexedValidation
    , indexedValidator
    , required
    , doesntExist
    , noMatches
    , uniqueCustomer
    , exists
    , uniqueExists
    , minimumLength
    , zeroOrPositive
    ) where

import Control.Arrow (second)
import Data.Aeson (ToJSON(..), encode, object)
import Data.Maybe (isJust, isNothing)
import Database.Persist
    ( PersistEntityBackend, PersistEntity, Filter, Unique, get, getBy
    , selectFirst, Entity (..)
    )
import Database.Persist.Sql (SqlBackend, Key)
import Servant (err422, errBody)

import Server

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Models (Customer(..))
import Models.Utils (getCustomerByEmail)


-- | Force the server to return a 422 HTTP Validation Error with a JSON body.
validationError :: ToJSON a => a -> App b
validationError body =
    serverError $ err422 { errBody = encode body }


type FieldName = T.Text
type ErrorMessage = T.Text
type Validators = [(FieldName, [(ErrorMessage, Bool)])]

class Validation a where
    -- | Validate an item, returning a 422 error with a body containing the
    -- error messages. The default implementation transforms the list of
    -- validators returned by `validators` into a JSON object.
    validate :: a -> App a
    validate item = do
        errorPairs <- processErrors <$> validators item
        let errorMap = HM.fromList
                . filter (\(_, es) -> not $ null es)
                $ errorPairs
        if HM.null errorMap then
            return item
        else
            validationError $ toJSON errorMap
        where processErrors =
                map $ second concatErrors
              concatErrors =
                concatMap (\(message, hasError) -> [message | hasError])

    -- | Return a list of validators by field name. Each field contains
    -- a list of messages and whether they are invalid. An empty field name
    -- corresponds to a general error.
    -- TODO: Switch bool is mean a field is valid, it's confusing the other
    -- way.
    validators :: a -> App Validators
    validators _ = return []

-- | Return a single general error in the same format as the Validation
-- typeclass.
singleError :: T.Text -> App a
singleError =
    singleFieldError ""

-- | Return a Validation error for a single field in the same format as the
-- Validation typeclass.
singleFieldError :: T.Text -> T.Text -> App a
singleFieldError field text =
    validationError $ object [ ( field, toJSON [ text ] ) ]

-- | Apply a list of validators to a Map, with each key of the Map as
-- a separate field.
validateMap :: Show k => [v -> (T.Text, Bool)] -> M.Map k v -> [(T.Text, [(T.Text, Bool)])]
validateMap funcs =
    M.foldlWithKey
        (\acc key val -> ( T.pack $ show key, map ($ val) funcs) : acc) []

-- | Apply a validation to the inner value of a 'Maybe'.
mapCheck :: Maybe a -> (a -> (FieldName, [(ErrorMessage, Bool)])) -> Maybe (FieldName, [(ErrorMessage, Bool)])
mapCheck = flip fmap

prefixFields :: T.Text -> [(FieldName, [(ErrorMessage, Bool)])] -> [(FieldName, [(ErrorMessage, Bool)])]
prefixFields pfx =
    map (\(fn, errs) -> (pfx <> fn, errs))

-- | Valiate a list of items by prepending a prefix & index to items' field
-- names.
indexedValidation :: T.Text -> (a -> App Validators) -> [a] -> App Validators
indexedValidation prefix validation items =
    fmap concat . flip indexedMapM items $ \index item ->
        prefixFields (prefix <> "-" <> T.pack (show index) <> "-")
            <$> validation item

-- | Validate a field containing a list, suffixing the fieldnames with each
-- item's index.
--
-- Note this differs from the `indexedValidation` function, as that applies
-- to nested structures with multiple sub-fields while this applies to
-- a single repeated field.
indexedValidator :: T.Text -> (a -> App [(ErrorMessage, Bool)]) -> [a] -> App Validators
indexedValidator fieldName_ validation =
    indexedMapM $ \index item ->
        let fieldName = fieldName_ <> "-" <> T.pack (show index)
        in  (fieldName, ) <$> validation item

required :: T.Text -> (T.Text, Bool)
required text =
    ("This field is required.", T.null text)

doesntExist :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
            => Unique r -> App Bool
doesntExist uniqueKey =
    isJust <$> runDB (getBy uniqueKey)

noMatches :: (PersistEntityBackend e ~ SqlBackend, PersistEntity e)
          => [Filter e] -> App Bool
noMatches filters =
    isJust <$> runDB (selectFirst filters [])

-- | Ensure a a Customer with the given email doesn't exist.
uniqueCustomer :: T.Text -> App Bool
uniqueCustomer email = runDB $ do
    mCustomer <- getCustomerByEmail email
    case mCustomer of
        Just (Entity _ c) -> pure $ not $ customerEphemeral c
        _ -> pure False

exists :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
       => Key r -> App Bool
exists entityId =
    isNothing <$> runDB (get entityId)

uniqueExists :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
             => Unique r -> App Bool
uniqueExists uniqueKey =
    isNothing <$> runDB (getBy uniqueKey)

minimumLength :: Int -> T.Text -> (T.Text, Bool)
minimumLength minLength text =
    ("Must be at least " <> T.pack (show minLength) <> " characters.", T.length text < minLength)

zeroOrPositive :: (Num a, Ord a) => a -> (T.Text, Bool)
zeroOrPositive i =
    ("Must be zero or higher.", i < 0)


-- UTILS

-- | `mapM` over a list including the index.
indexedMapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
indexedMapM action =
    go 0
  where
    go index = \case
        [] -> return []
        next : rest ->
            (:) <$> action index next <*> go (succ index) rest
