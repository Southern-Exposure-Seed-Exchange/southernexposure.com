module Models.Fields exposing
    ( Cents(..)
    , ImageData
    , Milligrams(..)
    , centsDecoder
    , centsFromDecimal
    , centsFromString
    , centsMap
    , centsMap2
    , imageDecoder
    , imageToSrcSet
    , imgSrcFallback
    , milligramsToString
    )

import Decimal exposing (Decimal)
import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Json.Decode as Decode exposing (Decoder)
import Views.Images exposing (media)


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


type alias ImageData =
    { original : String
    , xs : Maybe ScaledImage
    , sm : Maybe ScaledImage
    , md : Maybe ScaledImage
    , lg : Maybe ScaledImage
    , xl : Maybe ScaledImage
    }


imageDecoder : Decoder ImageData
imageDecoder =
    Decode.map6 ImageData
        (Decode.field "original" Decode.string)
        (Decode.field "xs" <| Decode.nullable scaledImageDecoder)
        (Decode.field "sm" <| Decode.nullable scaledImageDecoder)
        (Decode.field "md" <| Decode.nullable scaledImageDecoder)
        (Decode.field "lg" <| Decode.nullable scaledImageDecoder)
        (Decode.field "xl" <| Decode.nullable scaledImageDecoder)


imageToSrcSet : ImageData -> Attribute msg
imageToSrcSet i =
    [ i.xs, i.sm, i.md, i.lg, i.xl ]
        |> List.filterMap identity
        |> List.map (\x -> media x.path ++ " " ++ String.fromInt x.width ++ "w")
        |> String.join ", "
        |> attribute "srcset"


imgSrcFallback : ImageData -> String
imgSrcFallback i =
    let
        asum xs =
            case xs of
                Nothing :: rest ->
                    asum rest

                (Just x) :: _ ->
                    Just x

                [] ->
                    Nothing
    in
    asum [ i.md, i.lg, i.sm, i.xl, i.xs ]
        |> Maybe.map .path
        |> Maybe.withDefault i.original
        |> media


type alias ScaledImage =
    { width : Int
    , path : String
    }


scaledImageDecoder : Decoder ScaledImage
scaledImageDecoder =
    Decode.map2 ScaledImage
        (Decode.field "width" Decode.int)
        (Decode.field "src" Decode.string)
