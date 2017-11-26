module Models.Fields
    exposing
        ( Cents(..)
        , centsMap
        , centsMap2
        , centsDecoder
        , Milligrams(..)
        , milligramsToString
        )

import Decimal
import Json.Decode as Decode exposing (Decoder)


type Cents
    = Cents Int


centsMap : (Int -> Int) -> Cents -> Cents
centsMap f (Cents c) =
    f c |> Cents


centsMap2 : (Int -> Int -> Int) -> Cents -> Cents -> Cents
centsMap2 f (Cents a) (Cents b) =
    f a b |> Cents


centsDecoder : Decoder Cents
centsDecoder =
    Decode.map Cents Decode.int


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
