module Products.Pagination
    exposing
        ( Data
        , default
        , toQueryString
        , fromQueryString
        )

import UrlParser as Url exposing ((<?>))
import Products.Sorting as Sorting
import Routing.Utils exposing (optionalIntParam)


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
                , param ++ "=" ++ toString value
                )
            )
        |> (\fs -> ( Sorting.default /= sorting, Sorting.toQueryString sorting ) :: fs)
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> String.join "&"


fromQueryString pathParser =
    Url.map (\constructor page perPage -> constructor << Data page perPage)
        (pathParser
            <?> optionalIntParam "page" (default.page)
            <?> optionalIntParam "perPage" (default.perPage)
            <?> Url.customParam "sortBy" (Maybe.withDefault "" >> Sorting.fromQueryValue)
        )
