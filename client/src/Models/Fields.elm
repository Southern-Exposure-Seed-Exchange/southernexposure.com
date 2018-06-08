module Models.Fields
    exposing
        ( Cents(..)
        , centsMap
        , centsMap2
        , centsDecoder
        , centsFromDecimal
        , centsFromString
        , Milligrams(..)
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
        |> Decimal.mul (Decimal.unsafeFromString "0.001")
        |> Decimal.round -2
        |> Decimal.toString
