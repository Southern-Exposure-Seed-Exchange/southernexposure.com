module Category exposing
    ( Category
    , CategoryId(..)
    , decoder
    , initial
    )

import Json.Decode as Decode exposing (Decoder)
import Models.Fields exposing (ImageData, blankImage, imageDecoder)


type CategoryId
    = CategoryId Int


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
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "parentId" << Decode.nullable <| Decode.map CategoryId Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "image" imageDecoder)
        (Decode.field "order" Decode.int)
