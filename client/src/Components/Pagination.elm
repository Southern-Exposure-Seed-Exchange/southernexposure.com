module Components.Pagination exposing
    ( Data
    , default
    , fromQueryString
    , toQueryString
    )

import Components.Sorting as Sorting
import Data.Routing.Utils exposing (optionalIntParam)
import Url.Parser as Url exposing ((<?>))


type alias Data =
    { page : Int
    , perPage : Int
    , sorting : Sorting.Option
    }


default : Data
default =
    Data 1 25 Sorting.default


toQueryString : Data -> String
toQueryString { page, perPage, sorting } =
    [ ( .page, page, "page" )
    , ( .perPage, perPage, "perPage" )
    ]
        |> List.map
            (\( selector, value, param ) ->
                ( selector default /= value
                , param ++ "=" ++ String.fromInt value
                )
            )
        |> (\fs -> ( Sorting.default /= sorting, Sorting.toQueryString sorting ) :: fs)
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> String.join "&"


fromQueryString :
    Url.Parser ((Data -> c) -> Int -> Int -> Sorting.Option -> c) (Int -> Int -> Sorting.Option -> b)
    -> Url.Parser (b -> c1) c1
fromQueryString pathParser =
    Url.map (\constructor page perPage -> constructor << Data page perPage)
        (pathParser
            <?> optionalIntParam "page" default.page
            <?> optionalIntParam "perPage" default.perPage
        )
        |> Sorting.fromQueryString
