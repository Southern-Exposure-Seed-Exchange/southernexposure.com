module Products.Pagination
    exposing
        ( Data
        , default
        , toQueryString
        , fromQueryString
        )

import UrlParser as Url exposing ((<?>))
import Routing.Utils exposing (optionalIntParam)


type alias Data =
    { page : Int
    , perPage : Int
    }


default : Data
default =
    Data 1 25


toQueryString : Data -> String
toQueryString { page, perPage } =
    [ ( .page, page, "page" )
    , ( .perPage, perPage, "perPage" )
    ]
        |> List.map
            (\( selector, value, param ) ->
                ( selector default /= value
                , param ++ "=" ++ toString value
                )
            )
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> String.join "&"


fromQueryString :
    Url.Parser ((Data -> a) -> Int -> Int -> a) (Int -> Int -> b)
    -> Url.Parser (b -> c) c
fromQueryString pathParser =
    Url.map (\constructor page -> constructor << Data page)
        (pathParser
            <?> optionalIntParam "page" (default.page)
            <?> optionalIntParam "perPage" (default.perPage)
        )
