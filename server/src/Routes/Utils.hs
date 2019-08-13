{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Routes.Utils
    ( paginatedSelect
    , generateUniqueToken
    , hashPassword
    ) where

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist (Entity(..), getBy, PersistEntityBackend, PersistEntity)
import Database.Persist.Sql (SqlBackend)
import Servant (errBody, err500)

import Models
import Models.Fields (Cents(..))
import Server

import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Esqueleto as E


-- PRODUCTS


paginatedSelect :: Maybe T.Text -> Maybe Int -> Maybe Int
                -> (E.SqlExpr (Entity Product) -> E.SqlExpr (Maybe (Entity SeedAttribute)) -> E.SqlExpr (E.Value Bool))
                -> AppSQL ([Entity Product], Int)
paginatedSelect maybeSorting maybePage maybePerPage productFilters =
    let sorting = fromMaybe "" maybeSorting in
    case sorting of
        "name-asc" ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductName])
        "name-desc" ->
            productsSelect (\p -> E.orderBy [E.desc $ p E.^. ProductName])
        "number-asc" ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductBaseSku])
        "price-asc" ->
            variantSorted (\f -> [E.asc f]) offset perPage productFilters
        "price-desc" ->
            variantSorted (\f -> [E.desc f]) offset perPage productFilters
        "created-asc" ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductCreatedAt])
        "created-desc" ->
            productsSelect (\p -> E.orderBy [E.desc $ p E.^. ProductCreatedAt])
        _ ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductName])
    where perPage =
            fromIntegral $ fromMaybe 25 maybePerPage
          page =
            fromIntegral $ fromMaybe 1 maybePage
          offset =
            (page - 1) * perPage
          productsSelect ordering = do
            products <- E.select $ E.from $ \(p `E.LeftOuterJoin` sa) -> do
                E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
                E.where_ $
                    productFilters p sa
                    E.&&. p E.^. ProductIsActive E.==. E.val True
                    E.&&. activeVariantExists p
                void $ ordering p
                E.limit perPage
                E.offset offset
                return p
            productsCount <- countProducts productFilters
            return (products, productsCount)

variantSorted :: (E.SqlExpr (E.Value (Maybe Cents)) -> [E.SqlExpr E.OrderBy])
              -> Int64 -> Int64
              -> (E.SqlExpr (Entity Product) -> E.SqlExpr (Maybe (Entity SeedAttribute)) -> E.SqlExpr (E.Value Bool))
              -> AppSQL ([Entity Product], Int)
variantSorted ordering offset perPage filters = do
    productsAndPrice <- E.select $ E.from $ \(p `E.InnerJoin` v `E.LeftOuterJoin` sa) ->
        let minPrice = E.min_ $ v E.^. ProductVariantPrice in
        E.distinctOnOrderBy (ordering minPrice ++ [E.asc $ p E.^. ProductName]) $ do
        E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
        E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId
                E.&&. v E.^. ProductVariantIsActive E.==. E.val True)
        E.groupBy $ p E.^. ProductId
        E.where_ $ filters p sa E.&&. p E.^. ProductIsActive E.==. E.val True
        E.limit perPage
        E.offset offset
        return (p, minPrice)
    pCount <- countProducts filters
    let (ps, _) = unzip productsAndPrice
    return (ps, pCount)

-- | Count the number of results with the given filters.
countProducts
    :: (E.SqlExpr (Entity Product) -> E.SqlExpr (Maybe (Entity SeedAttribute)) -> E.SqlExpr (E.Value Bool))
    -> AppSQL Int
countProducts filters =
    rowsToCount . E.select $ E.from $ \(p `E.LeftOuterJoin` sa) -> do
        E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
        E.where_ $ filters p sa E.&&. p E.^. ProductIsActive E.==. E.val True
            E.&&. activeVariantExists p
        return (E.countRows :: E.SqlExpr (E.Value Int))
  where
    rowsToCount :: AppSQL [E.Value Int] -> AppSQL Int
    rowsToCount = fmap $ maybe 0 E.unValue . listToMaybe

-- | Determine if the Product has an active ProductVariant.
activeVariantExists :: E.SqlExpr (Entity Product) -> E.SqlExpr (E.Value Bool)
activeVariantExists p =  E.exists $ E.from $ \v -> E.where_ $
    p E.^. ProductId E.==. v E.^. ProductVariantProductId E.&&.
    v E.^. ProductVariantIsActive E.==. E.val True


-- CUSTOMERS


generateUniqueToken :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r)
                    => (T.Text -> Unique r) -> App T.Text
generateUniqueToken uniqueConstraint = do
    token <- UUID.toText <$> liftIO UUID4.nextRandom
    maybeCustomer <- runDB . getBy $ uniqueConstraint token
    case maybeCustomer of
        Just _ ->
            generateUniqueToken uniqueConstraint
        Nothing ->
            return token

hashPassword :: T.Text -> App T.Text
hashPassword password = do
    maybePass <- liftIO . BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy
        $ encodeUtf8 password
    case maybePass of
        Nothing ->
            serverError $ err500 { errBody = "Misconfigured Hashing Policy" }
        Just pass ->
            return $ decodeUtf8 pass
