module StaticPage exposing
    ( StaticPage
    , StaticPageId
    , decoder
    , idDecoder
    , idEncoder
    , idPath
    , idToString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser as Url


type StaticPageId
    = StaticPageId Int


idDecoder : Decoder StaticPageId
idDecoder =
    Decode.map StaticPageId Decode.int


idEncoder : StaticPageId -> Value
idEncoder (StaticPageId pId) =
    Encode.int pId


idPath : Url.Parser (StaticPageId -> a) a
idPath =
    Url.map StaticPageId Url.int


idToString : StaticPageId -> String
idToString (StaticPageId i) =
    String.fromInt i


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
