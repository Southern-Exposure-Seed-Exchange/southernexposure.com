module Models.Fields exposing
    ( Cents(..)
    , Milligrams(..)
    , centsDecoder
    , centsFromDecimal
    , centsFromString
    , centsMap
    , centsMap2
    , milligramsToString
    )

import Decimal exposing (Decimal)
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


centsFromDecimal : Decimal -> Cents
centsFromDecimal =
    Decimal.truncate -2
        >> Decimal.mul (Decimal.fromInt 100)
        >> Decimal.toFloat
        >> round
        >> Cents


centsFromString : String -> Maybe Cents
centsFromString =
    Decimal.fromString >> Maybe.map centsFromDecimal


type Milligrams
    = Milligrams Int


{-| TODO: Remove trailing zeros, special cases for ounces/pounds
-}
milligramsToString : Milligrams -> String
milligramsToString (Milligrams i) =
    Decimal.fromInt i
        |> Decimal.mul (Decimal.fromIntWithExponent 1 -3)
        |> Decimal.round -2
        |> Decimal.toString
