module Search
    exposing
        ( Data
        , initial
        , resetQuery
        , encode
        , toQueryString
        , fromQueryString
        )

import Json.Encode as Encode exposing (Value)
import UrlParser as Url exposing ((<?>))


type alias Data =
    { query : String }


initial : Data
initial =
    { query = "" }


resetQuery : Data -> Data
resetQuery data =
    { data | query = "" }


encode : Data -> Value
encode { query } =
    Encode.object [ ( "query", Encode.string query ) ]


toQueryString : Data -> String
toQueryString { query } =
    if String.isEmpty query then
        ""
    else
        "q=" ++ query


fromQueryString :
    Url.Parser ((Data -> c) -> String -> c) (String -> b)
    -> Url.Parser (b -> c1) c1
fromQueryString pathParser =
    Url.map (\constructor -> constructor << Data)
        (pathParser <?> Url.customParam "q" (Maybe.withDefault ""))
