module Routing
    exposing
        ( Route(..)
        , parseRoute
        , reverse
        )

import Navigation
import UrlParser as Url exposing ((</>))
import Products.Pagination as Pagination
import Products.Sorting as Sorting


type Route
    = ProductDetails String
    | CategoryDetails String Pagination.Data Sorting.Option


parseRoute : Navigation.Location -> Route
parseRoute =
    let
        routeParser =
            Url.oneOf
                [ Url.map ProductDetails (Url.s "products" </> Url.string)
                , Sorting.fromQueryString <|
                    Pagination.fromQueryString <|
                        Url.map CategoryDetails (Url.s "categories" </> Url.string)
                ]
    in
        Url.parsePath routeParser
            >> Maybe.withDefault (ProductDetails "green-pod-red-seed-asparagus-yardlong-bean-7-g")


reverse : Route -> String
reverse route =
    let
        joinPath paths =
            String.join "/" <| "" :: paths ++ [ "" ]

        joinQueryStrings =
            List.filter (not << String.isEmpty)
                >> String.join "&"
                >> (\s ->
                        if String.isEmpty s then
                            ""
                        else
                            "?" ++ s
                   )
    in
        case route of
            ProductDetails slug ->
                joinPath [ "products", slug ]

            CategoryDetails slug pagination sortData ->
                joinPath [ "categories", slug ]
                    ++ joinQueryStrings
                        [ Pagination.toQueryString pagination
                        , Sorting.toQueryString sortData
                        ]
