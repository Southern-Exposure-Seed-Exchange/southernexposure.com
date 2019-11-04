module StaticPage exposing
    ( StaticPage
    , StaticPageId
    , decoder
    , idDecoder
    )

import Json.Decode as Decode exposing (Decoder)


type StaticPageId
    = StaticPageId Int


idDecoder : Decoder StaticPageId
idDecoder =
    Decode.map StaticPageId Decode.int


type alias StaticPage =
    { name : String
    , slug : String
    , content : String
    }


decoder : Decoder StaticPage
decoder =
    Decode.map3 StaticPage
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "content" Decode.string)
