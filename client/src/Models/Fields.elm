module Models.Fields exposing
    ( Cents(..)
    , ImageData
    , LotSize(..)
    , Milligrams(..)
    , blankImage
    , centsDecoder
    , centsEncoder
    , centsFromDecimal
    , centsFromString
    , centsMap
    , centsMap2
    , centsToString
    , imageDataLightboxConfig
    , imageDecoder
    , imageToSrcSet
    , imgSrcFallback
    , lotSizeDecoder
    , lotSizeEncoder
    , lotSizeToString
    , milligramsFromString
    , milligramsToGrams
    , milligramsToString
    )

import BootstrapGallery as Gallery
import Decimal exposing (Decimal)
import Html exposing (Attribute)
import Html.Attributes exposing (attribute)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Views.Images exposing (media, noImagePath)


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


centsEncoder : Cents -> Value
centsEncoder (Cents c) =
    Encode.int c


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


centsToString : Cents -> String
centsToString (Cents c) =
    Decimal.fromInt c
        |> Decimal.mul (Decimal.fromIntWithExponent 1 -2)
        |> Decimal.toString


type Milligrams
    = Milligrams Int


milligramsDecoder : Decoder Milligrams
milligramsDecoder =
    Decode.map Milligrams Decode.int


milligramsEncoder : Milligrams -> Value
milligramsEncoder (Milligrams mg) =
    Encode.int mg


milligramsFromString : String -> Maybe Milligrams
milligramsFromString =
    Decimal.fromString >> Maybe.map milligramsFromDecimal


milligramsFromDecimal : Decimal -> Milligrams
milligramsFromDecimal =
    Decimal.truncate -3
        >> Decimal.mul (Decimal.fromInt 1000)
        >> Decimal.toFloat
        >> round
        >> Milligrams


{-| Note: When modifying the case matches, update the Server code in
Models.Fields as well.
-}
milligramsToString : Milligrams -> String
milligramsToString ((Milligrams i) as m) =
    case i of
        84000 ->
            "3 oz"

        114000 ->
            "¼ lb"

        171000 ->
            "6 oz"

        228000 ->
            "½ lb"

        342000 ->
            "¾ lb"

        454000 ->
            "1 lb"

        568000 ->
            "1¼ lbs"

        680000 ->
            "1½ lbs"

        908000 ->
            "2 lbs"

        1135000 ->
            "2½ lbs"

        1140000 ->
            "2½ lbs"

        1816000 ->
            "4 lbs"

        2270000 ->
            "5 lbs"

        _ ->
            milligramsToGrams m ++ " g"


{-| Convert a Milligrams amount into a string representation of grams.

Does not include a trailing `g`.

-}
milligramsToGrams : Milligrams -> String
milligramsToGrams (Milligrams i) =
    let
        stripZeroes str =
            if String.right 1 str == "0" then
                stripZeroes <| String.dropRight 1 str

            else if String.right 1 str == "." then
                String.dropRight 1 str

            else
                str
    in
    Decimal.fromInt i
        |> Decimal.mul (Decimal.fromIntWithExponent 1 -3)
        |> Decimal.round -2
        |> Decimal.toString
        |> stripZeroes


type LotSize
    = Mass Milligrams
    | Bulbs Int
    | Slips Int
    | Plugs Int
    | CustomLotSize String


lotSizeDecoder : Decoder LotSize
lotSizeDecoder =
    let
        decodeSize type_ =
            case type_ of
                "mass" ->
                    Decode.map Mass
                        (Decode.field "size" milligramsDecoder)

                "bulbs" ->
                    Decode.map Bulbs
                        (Decode.field "size" Decode.int)

                "slips" ->
                    Decode.map Slips
                        (Decode.field "size" Decode.int)

                "plugs" ->
                    Decode.map Plugs
                        (Decode.field "size" Decode.int)

                "custom" ->
                    Decode.map CustomLotSize
                        (Decode.field "size" Decode.string)

                _ ->
                    Decode.fail <| "Unexpected LotSize Type: " ++ type_
    in
    Decode.field "type" Decode.string
        |> Decode.andThen decodeSize


lotSizeEncoder : LotSize -> Value
lotSizeEncoder ls =
    let
        ( type_, size ) =
            case ls of
                Mass mg ->
                    ( "mass", milligramsEncoder mg )

                Bulbs q ->
                    ( "bulbs", Encode.int q )

                Slips q ->
                    ( "slips", Encode.int q )

                Plugs q ->
                    ( "plugs", Encode.int q )

                CustomLotSize str ->
                    ( "custom", Encode.string str )
    in
    Encode.object
        [ ( "type", Encode.string type_ )
        , ( "size", size )
        ]


lotSizeToString : LotSize -> String
lotSizeToString l =
    case l of
        Mass mg ->
            milligramsToString mg

        Bulbs i ->
            String.fromInt i ++ " Bulbs"

        Slips i ->
            String.fromInt i ++ " Slips"

        Plugs i ->
            String.fromInt i ++ " Plugs"

        CustomLotSize t ->
            t


type alias ImageData =
    { original : String
    , xs : Maybe ScaledImage
    , sm : Maybe ScaledImage
    , md : Maybe ScaledImage
    , lg : Maybe ScaledImage
    , xl : Maybe ScaledImage
    }


blankImage : ImageData
blankImage =
    ImageData "" Nothing Nothing Nothing Nothing Nothing


imageDecoder : Decoder ImageData
imageDecoder =
    Decode.map6 ImageData
        (Decode.field "original" <|
            Decode.map (Maybe.map media >> Maybe.withDefault noImagePath) <|
                Decode.nullable Decode.string
        )
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
        |> Maybe.map (.path >> media)
        |> Maybe.withDefault i.original


imageDataLightboxConfig : Gallery.Config ImageData
imageDataLightboxConfig =
    { thumbnailUrl = always Nothing
    , imageUrl = .original
    }


type alias ScaledImage =
    { width : Int
    , path : String
    }


scaledImageDecoder : Decoder ScaledImage
scaledImageDecoder =
    Decode.map2 ScaledImage
        (Decode.field "width" Decode.int)
        (Decode.field "src" Decode.string)
