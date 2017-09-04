module Routing
    exposing
        ( Route(..)
        , parseRoute
        , reverse
        )

import Navigation
import UrlParser as Url exposing ((</>))
import Products.Pagination as Pagination
import Routing.Utils exposing (joinPath, withQueryStrings)
import Search


type Route
    = ProductDetails String
    | CategoryDetails String Pagination.Data
    | AdvancedSearch
    | SearchResults Search.Data Pagination.Data
    | PageDetails String


parseRoute : Navigation.Location -> Route
parseRoute =
    let
        routeParser =
            Url.oneOf
                [ Url.map (PageDetails "home") Url.top
                , Url.map ProductDetails (Url.s "products" </> Url.string)
                , Url.map CategoryDetails (Url.s "categories" </> Url.string)
                    |> Pagination.fromQueryString
                , Url.map AdvancedSearch (Url.s "search" </> Url.s "advanced")
                , Url.map SearchResults (Url.s "search")
                    |> Search.fromQueryString
                    |> Pagination.fromQueryString
                , Url.map PageDetails (Url.string)
                ]
    in
        Url.parsePath routeParser
            >> Maybe.withDefault (ProductDetails "green-pod-red-seed-asparagus-yardlong-bean-7-g")


reverse : Route -> String
reverse route =
    case route of
        ProductDetails slug ->
            joinPath [ "products", slug ]

        CategoryDetails slug pagination ->
            joinPath [ "categories", slug ]
                ++ withQueryStrings
                    [ Pagination.toQueryString pagination ]

        AdvancedSearch ->
            joinPath [ "search", "advanced" ]

        SearchResults data pagination ->
            joinPath [ "search" ]
                ++ withQueryStrings
                    [ Search.toQueryString data
                    , Pagination.toQueryString pagination
                    ]

        PageDetails slug ->
            if slug == "home" then
                ""
            else
                joinPath [ slug ]
