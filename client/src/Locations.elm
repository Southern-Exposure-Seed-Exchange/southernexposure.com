module Locations exposing
    ( AddressLocations
    , Location
    , Region(..)
    , addressLocationsDecoder
    , armedForcesCodes
    , findName
    , fromRegion
    , locationDecoder
    , regionDecoder
    , regionEncoder
    , regionName
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


findName : List Location -> String -> Maybe String
findName locations code =
    case locations of
        [] ->
            Nothing

        x :: xs ->
            if x.code == code then
                Just x.name

            else
                findName xs code


{-| TODO: Use type instead of alias, maybe keep a dict for fast lookups as well
-}
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


regionName : AddressLocations -> Region -> Maybe String
regionName locations region =
    case region of
        USState code ->
            findName locations.states code

        ArmedForces code ->
            findName locations.armedForces code

        CAProvince code ->
            findName locations.provinces code

        Custom str ->
            Just str


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
