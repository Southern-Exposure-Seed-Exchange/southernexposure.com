{-# LANGUAGE OverloadedStrings #-}
module Routes.Utils
    ( paginatedSelect
    ) where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Database.Persist (Entity(..))

import Models
import Models.Fields (Cents(..))
import Server

import qualified Data.Text as T
import qualified Database.Esqueleto as E

paginatedSelect :: Maybe T.Text -> Maybe Int -> Maybe Int
                -> (E.SqlExpr (Entity Product) -> E.SqlExpr (Maybe (Entity SeedAttribute)) -> E.SqlExpr (E.Value Bool))
                -> App ([Entity Product], Int)
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
        _ ->
            productsSelect (\p -> E.orderBy [E.asc $ p E.^. ProductName])
    where perPage =
            fromIntegral $ fromMaybe 25 maybePerPage
          page =
            fromIntegral $ fromMaybe 1 maybePage
          offset =
            (page - 1) * perPage
          productsSelect ordering =
            runDB $ do
                products <- E.select $ E.from $ \(p `E.LeftOuterJoin` sa) -> do
                    E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
                    E.where_ $ productFilters p sa
                    _ <- ordering p
                    E.limit perPage
                    E.offset offset
                    return p
                productsCount <- E.select $ E.from $ \(p `E.LeftOuterJoin` sa) -> do
                    E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
                    E.where_ $ productFilters p sa
                    return (E.countRows :: E.SqlExpr (E.Value Int))
                return (products, E.unValue $ head productsCount)

variantSorted :: (E.SqlExpr (E.Value (Maybe Cents)) -> [E.SqlExpr E.OrderBy])
              -> Int64 -> Int64
              -> (E.SqlExpr (Entity Product) -> E.SqlExpr (Maybe (Entity SeedAttribute)) -> E.SqlExpr (E.Value Bool))
              -> App ([Entity Product], Int)
variantSorted ordering offset perPage filters = runDB $ do
    productsAndPrice <- E.select $ E.from $ \(p `E.InnerJoin` v `E.LeftOuterJoin` sa) -> do
        E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
        E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
        let minPrice = E.min_ $ v E.^. ProductVariantPrice
        E.groupBy $ p E.^. ProductId
        E.orderBy $ ordering minPrice
        E.where_ $ filters p sa
        E.limit perPage
        E.offset offset
        return (p, minPrice)
    pCount <- E.select $ E.from $ \(p `E.LeftOuterJoin` sa) -> do
        E.on (E.just (p E.^. ProductId) E.==. sa E.?. SeedAttributeProductId)
        E.where_ $ filters p sa
        return (E.countRows :: E.SqlExpr (E.Value Int))
    let (ps, _) = unzip productsAndPrice
    return (ps, E.unValue $ head pCount)
