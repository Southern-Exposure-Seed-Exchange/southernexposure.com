module Data.Category exposing
    ( Category
    , CategoryId(..)
    , decoder
    , idDecoder
    , idEncoder
    , idParser
    , initial
    , maybeIdParser
    )

import Data.Fields exposing (ImageData, blankImage, imageDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type CategoryId
    = CategoryId Int


{-| Parse a potential Category ID from the string-representation of an Integer.
An empty string signals a Nothing value.
-}
idParser : String -> Result String CategoryId
idParser val =
    case String.toInt val of
        Just i ->
            Ok <| CategoryId i

        Nothing ->
            Err "Could not parse category ID."


{-| Parse a CategoryId or a Blank Option.
-}
maybeIdParser : String -> Result String (Maybe CategoryId)
maybeIdParser val =
    if String.isEmpty val then
        Ok Nothing

    else
        Result.map Just <| idParser val


idEncoder : CategoryId -> Encode.Value
idEncoder (CategoryId c) =
    Encode.int c


idDecoder : Decoder CategoryId
idDecoder =
    Decode.map CategoryId Decode.int


type alias Category =
    { id : CategoryId
    , name : String
    , slug : String
    , parentId : Maybe CategoryId
    , description : String
    , image : ImageData
    , order : Int
    }


initial : Category
initial =
    Category (CategoryId 0) "" "" Nothing "" blankImage 0


decoder : Decoder Category
decoder =
    Decode.map7 Category
        (Decode.field "id" idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "parentId" << Decode.nullable <| Decode.map CategoryId Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "image" imageDecoder)
        (Decode.field "order" Decode.int)
