module Models.Fields
    exposing
        ( Cents(..)
        , centsToString
        , Milligrams(..)
        , milligramsToString
        )

import Decimal


type Cents
    = Cents Int


centsToString : Cents -> String
centsToString (Cents i) =
    Decimal.fromInt i
        |> Decimal.mul (Decimal.unsafeFromString "0.01")
        |> Decimal.round -2
        |> Decimal.toString


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
