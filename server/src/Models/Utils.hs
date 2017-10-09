{-# LANGUAGE OverloadedStrings #-}
module Models.Utils
    ( slugify
    , truncateDescription
    , getChildCategoryIds
    , getParentCategories
    , getTaxRate
    , applyTaxRate
    ) where

import Data.Char (isAlphaNum)
import Data.Monoid ((<>))
import Database.Persist ((==.), Entity(..), Key(..), get, getBy, selectKeysList)
import Text.HTML.TagSoup (parseTags, innerText)

import Models.DB
import Models.Fields
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


-- | Return a TaxRate for a potential Country & Region. On failure, it will
-- fallback to a TaxRate for just the Country if one exists.
getTaxRate :: Maybe Country -> Maybe Region -> AppSQL (Maybe TaxRate)
getTaxRate maybeCountry maybeRegion =
    fmap (\(Entity _ e) -> e) <$> case (maybeCountry, maybeRegion) of
        (Just country, Nothing) ->
            getBy $ UniqueTaxRate country maybeRegion
        (Just country, Just _) ->
            getBy (UniqueTaxRate country maybeRegion)
            >>= maybe (getBy $ UniqueTaxRate country Nothing) (return . Just)
        _ ->
            return Nothing

-- | Apply a Tax Rate to an Amount for a Product, returning 0 if the
-- Product is excluded from the Tax Rate.
applyTaxRate :: Cents -> ProductId -> TaxRate -> Cents
applyTaxRate amount productId taxRate =
    if productId `notElem` taxRateExcludedProductIds taxRate then
        Cents . round
            $ (toRational . toInteger $ taxRateRate taxRate)
            / 1000
            * toRational (fromCents amount)
    else
        Cents 0
