{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Validation
    ( Validation(..)
    , singleError
    , required
    , doesntExist
    , minimumLength
    ) where

import Control.Monad.Trans (lift)
import Control.Arrow (second)
import Data.Aeson (ToJSON(..), encode, object)
import Data.Monoid ((<>))
import Database.Persist (PersistEntityBackend, PersistEntity, Unique, getBy)
import Database.Persist.Sql (SqlBackend)
import Servant (throwError, err422, errBody)

import Server

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


type FieldName = T.Text
type ErrorMessage = T.Text

class Validation a where
    -- | Validate an item, returning a 500 error with a body containing the
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
            lift . throwError $ err422
                { errBody = encode errorMap }
        where processErrors =
                map $ second concatErrors
              concatErrors =
                concatMap (\(message, hasError) -> [message | hasError])

    -- | Return a list of validators by field name. Each field contains
    -- a list of messages and whether they are valid. An empty field name
    -- corresponds to a general error.
    validators :: a -> App [(FieldName, [(ErrorMessage, Bool)])]
    validators _ = return []

-- | Return a single general error in the same format as the Validation
-- typeclass.
singleError :: T.Text -> App a
singleError text =
    lift . throwError $
        err422 { errBody = encode $ object [ ( "", toJSON [ text ] ) ] }

required :: T.Text -> (T.Text, Bool)
required text =
    ("This field is required", T.null text)

doesntExist :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
            => Unique r -> App Bool
doesntExist uniqueKey = do
    maybeItem <- runDB $ getBy uniqueKey
    case maybeItem of
        Nothing ->
            return False
        Just _ -> return True

minimumLength :: Int -> T.Text -> (T.Text, Bool)
minimumLength minLength text =
    ("Must be at least " <> T.pack (show minLength) <> " characters", T.length text < minLength)
