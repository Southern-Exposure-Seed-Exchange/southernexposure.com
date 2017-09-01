module Routing
    exposing
        ( Route(..)
        , parseRoute
        , reverse
        )

import Navigation
import UrlParser as Url exposing ((</>))
import Products.Pagination as Pagination
import Search


type Route
    = ProductDetails String
    | CategoryDetails String Pagination.Data
    | SearchResults Search.Data Pagination.Data


parseRoute : Navigation.Location -> Route
parseRoute =
    let
        routeParser =
            Url.oneOf
                [ Url.map ProductDetails (Url.s "products" </> Url.string)
                , Url.map CategoryDetails (Url.s "categories" </> Url.string)
                    |> Pagination.fromQueryString
                , Url.map SearchResults (Url.s "search")
                    |> Search.fromQueryString
                    |> Pagination.fromQueryString
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

            CategoryDetails slug pagination ->
                joinPath [ "categories", slug ]
                    ++ joinQueryStrings
                        [ Pagination.toQueryString pagination ]

            SearchResults data pagination ->
                joinPath [ "search" ]
                    ++ joinQueryStrings
                        [ Search.toQueryString data
                        , Pagination.toQueryString pagination
                        ]
