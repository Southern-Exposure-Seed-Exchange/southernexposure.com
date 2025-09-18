{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- Export any Product Variants with Zero Mass Lot Sizes & Non-Standard
Masses Greater Than 42g.

-}
import Control.Monad (forM)
import Control.Monad.Logger (runNoLoggingT)
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ratio ((%), Ratio, numerator, denominator)
import Database.Persist
import Database.Persist.Postgresql
    ( ConnectionPool, SqlPersistT, runSqlPool )
import Numeric.Natural (Natural)

import Models
import Models.Fields
import Utils (makeSqlPool)

import qualified Data.Text as T

main :: IO ()
main = do
    massVariants <- connectToPostgres >>= runSqlPool getMassVariants
    let zeroMassVariants = getZeroMassVariants massVariants
        nonZeroMassVariants = getNonZeroMassVariants massVariants
    exportZeroMassVariants zeroMassVariants
    exportNonZeroMassVariants nonZeroMassVariants


type ProductAndVariants = (Entity Product, [Entity ProductVariant])


connectToPostgres :: IO ConnectionPool
connectToPostgres =
    runNoLoggingT $ makeSqlPool 1

-- | Get Active Products join with any Active Variants.
getMassVariants :: SqlPersistT IO [ProductAndVariants]
getMassVariants = do
    products <- selectList [] [Asc ProductBaseSku]
    fmap catMaybes . forM products $ \e@(Entity pId _) -> do
        vs <- filter (isMass . productVariantLotSize . entityVal) <$> selectList
            [ ProductVariantProductId ==. pId
            , ProductVariantIsActive ==. True
            ]
            [ Asc ProductVariantSkuSuffix
            ]
        return $ nullToNothing e vs
  where
    isMass :: Maybe LotSize -> Bool
    isMass = \case
        Just (Mass _) ->
            True
        _ ->
            False


-- | Return only Variants that have a Lot Size of 0mg.
getZeroMassVariants :: [ProductAndVariants] -> [ProductAndVariants]
getZeroMassVariants =
    mapMaybe getZeroMasses
  where
    getZeroMasses :: ProductAndVariants -> Maybe ProductAndVariants
    getZeroMasses (p, vs) =
        let matchingVariants =
                filter (\v -> productVariantLotSize (entityVal v) == Just (Mass $ Milligrams 0)) vs
        in
            nullToNothing p matchingVariants

-- | Return only Variants that have a Lot Size that the client does not
-- prettify (e.g., 454g => 1lb).
getNonZeroMassVariants :: [ProductAndVariants] -> [ProductAndVariants]
getNonZeroMassVariants =
    mapMaybe getNonZeroMasses
  where
    getNonZeroMasses :: ProductAndVariants -> Maybe ProductAndVariants
    getNonZeroMasses (p, vs) =
        let matchingVariants =
                filter (\v -> isNonZeroMass v && isOddMass v && isLargeEnough v) vs
        in
            nullToNothing p matchingVariants
    isNonZeroMass :: Entity ProductVariant -> Bool
    isNonZeroMass v =
        getMass v /= 0
    isLargeEnough :: Entity ProductVariant -> Bool
    isLargeEnough v =
        getMass v > 42000
    isOddMass :: Entity ProductVariant -> Bool
    isOddMass v =
        getMass v `notElem` map (* 1000)
            [ 114
            , 228
            , 342
            , 454
            , 568
            , 680
            , 908
            , 1135
            , 1816
            , 2270
            ]

-- | Get the Mass of a ProductVariant, Defaulting to 0.
getMass :: Entity ProductVariant -> Natural
getMass v =
    case productVariantLotSize (entityVal v) of
        Just (Mass (Milligrams m)) ->
            fromIntegral m
        _ ->
            0

-- | Return nothing is the list is empty. Otherwise join the parameters.
nullToNothing :: a -> [b] -> Maybe (a, [b])
nullToNothing x ys =
    if null ys then
        Nothing
    else
        Just (x, ys)



-- | Create the export for Zero-Mass Variants.
exportZeroMassVariants :: [ProductAndVariants] -> IO ()
exportZeroMassVariants =
    buildExport buildExportLine "export-zero-mass.csv"
  where
    buildExportLine (Entity _ p) (Entity _ v) =
        [ productBaseSku p <> productVariantSkuSuffix v
        , T.replace "," "" $ productName p
        ]

-- | Create the export for Non-Standard Mass Variants, including the mass
-- in grams, ounces, and pounds.
exportNonZeroMassVariants :: [ProductAndVariants] -> IO ()
exportNonZeroMassVariants =
    buildExport buildExportLine "export-non-zero-mass.csv"
  where
    buildExportLine (Entity _ p) ve@(Entity _ v) =
        let grams = getMass ve % 1000 in
        [ productBaseSku p <> productVariantSkuSuffix v
        , T.replace "," "" $ productName p
        , display grams
        , display $ grams / 28
        , display $ grams / 454
        ]
    display :: (Ord a, Show a, Integral a) => Ratio a -> T.Text
    display rat = T.pack $ if num < 0 then "-" else "" ++ shows d ("." ++ take 4 (go next))
        where
            (d, next) = abs num `quotRem` den
            num = numerator rat
            den = denominator rat
            go 0 = ""
            go x = let (d_, next_) = (10 * x) `quotRem` den
                in shows d_ (go next_)


-- | Build a generic export using a line-generating function.
buildExport
    :: (Entity Product -> Entity ProductVariant -> [T.Text])
    -> FilePath
    -> [ProductAndVariants]
    -> IO ()
buildExport builder name ps =
    writeFile name (unlines $ concatMap buildLines ps)
  where
    buildLines :: ProductAndVariants -> [String]
    buildLines (p, vs) =
        map (intercalate "," . map T.unpack . builder p) vs
