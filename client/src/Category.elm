module Category
    exposing
        ( CategoryId(..)
        , Category
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)


type CategoryId
    = CategoryId Int


type alias Category =
    { id : CategoryId
    , name : String
    , slug : String
    , parentId : Maybe CategoryId
    , description : String
    , imageURL : String
    , order : Int
    }


decoder : Decoder Category
decoder =
    Decode.map7 Category
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "parentId" << Decode.nullable <| Decode.map CategoryId Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "imageUrl" Decode.string)
        (Decode.field "order" Decode.int)
