module Locations
    exposing
        ( Location
        , locationDecoder
        , AddressLocations
        , addressLocationsDecoder
        , Region(..)
        , fromRegion
        , armedForcesCodes
        , regionDecoder
        , regionEncoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Location =
    { code : String
    , name : String
    }


locationDecoder : Decoder Location
locationDecoder =
    Decode.map2 Location
        (Decode.field "code" Decode.string)
        (Decode.field "name" Decode.string)


type alias AddressLocations =
    { countries : List Location
    , states : List Location
    , armedForces : List Location
    , provinces : List Location
    }


addressLocationsDecoder : Decoder AddressLocations
addressLocationsDecoder =
    Decode.map4 AddressLocations
        (Decode.field "countries" <| Decode.list locationDecoder)
        (Decode.field "states" <| Decode.list locationDecoder)
        (Decode.field "armedForces" <| Decode.list locationDecoder)
        (Decode.field "provinces" <| Decode.list locationDecoder)



-- States / Provinces


type Region
    = USState String
    | ArmedForces String
    | CAProvince String
    | Custom String


fromRegion : Region -> String
fromRegion region =
    case region of
        USState c ->
            c

        ArmedForces c ->
            c

        CAProvince c ->
            c

        Custom s ->
            s


armedForcesCodes : List String
armedForcesCodes =
    [ "AA", "AE", "AP" ]


regionDecoder : Decoder Region
regionDecoder =
    [ ( USState, "state" )
    , ( ArmedForces, "armedForces" )
    , ( CAProvince, "province" )
    , ( Custom, "custom" )
    ]
        |> List.map
            (\( constructor, field ) ->
                Decode.map constructor (Decode.field field Decode.string)
            )
        |> Decode.oneOf


regionEncoder : Region -> Value
regionEncoder region =
    let
        regionObject key value =
            Encode.object [ ( key, Encode.string value ) ]
    in
        case region of
            USState code ->
                regionObject "state" code

            ArmedForces code ->
                regionObject "armedForces" code

            CAProvince code ->
                regionObject "province" code

            Custom str ->
                regionObject "custom" str
