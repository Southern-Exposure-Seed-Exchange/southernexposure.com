{-# LANGUAGE OverloadedStrings #-}
module Models.Utils
    ( slugify
    , truncateDescription
    , getChildCategoryIds
    , getParentCategories
    ) where

import Data.Char (isAlphaNum)
import Data.Monoid ((<>))
import Database.Persist ((==.), Entity(..), Key(..), get, selectKeysList)
import Text.HTML.TagSoup (parseTags, innerText)

import Models.DB
import Server

import qualified Data.Text as T


-- | Turn a name into a URL-safe string.
slugify :: T.Text -> T.Text
slugify =
    T.intercalate "-" . T.words . T.toLower . replaceBy isValidChar
    where replaceBy predicate text =
            flip T.map text $ \c -> if predicate c then c else ' '
          isValidChar =
            isAny [ isAlphaNum, (==) '-', (==) '_' ]
          isAny preds value =
              case preds of
                predicate:ps ->
                    predicate value || isAny ps value
                [] ->
                    False


-- | Trim a Product's Description.
truncateDescription :: Entity Product -> Entity Product
truncateDescription (Entity pId p) =
            let
                strippedDescription =
                    innerText . parseTags $ productLongDescription p
                truncatedDescription =
                    T.unwords . take 40 $ T.words strippedDescription
                newDescription =
                    if truncatedDescription /= strippedDescription then
                        truncatedDescription <> "..."
                    else
                        truncatedDescription
            in
                Entity pId $ p { productLongDescription = newDescription }


-- | Return the ID's of a Category's Child Categories, including the passed
-- CategoryId.
getChildCategoryIds  :: Key Category -> App [Key Category]
getChildCategoryIds categoryId = do
    childrenKeys <- runDB $ selectKeysList [CategoryParentId ==. Just categoryId] []
    subKeys <- concat <$> mapM getChildCategoryIds childrenKeys
    return $ categoryId : subKeys

getParentCategories :: Key Category -> App [Entity Category]
getParentCategories categoryId = do
    maybeCategory <- runDB $ get categoryId
    flip (maybe $ return []) maybeCategory $ \category ->
        case categoryParentId category of
            Nothing ->
                return [Entity categoryId category]
            Just parentId ->
                (Entity categoryId category :) <$> getParentCategories parentId
