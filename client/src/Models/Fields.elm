module Models.Fields
    exposing
        ( Cents(..)
        , centsMap
        , centsMap2
        , Milligrams(..)
        , milligramsToString
        )

import Decimal


type Cents
    = Cents Int


centsMap : (Int -> Int) -> Cents -> Cents
centsMap f (Cents c) =
    f c |> Cents


centsMap2 : (Int -> Int -> Int) -> Cents -> Cents -> Cents
centsMap2 f (Cents a) (Cents b) =
    f a b |> Cents


type Milligrams
    = Milligrams Int


{-| TODO: Remove trailing zeros, special cases for ounces/pounds
-}
milligramsToString : Milligrams -> String
milligramsToString (Milligrams i) =
    Decimal.fromInt i
        |> Decimal.mul (Decimal.unsafeFromString "0.001")
        |> Decimal.round -2
        |> Decimal.toString
