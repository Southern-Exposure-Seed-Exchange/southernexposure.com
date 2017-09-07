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
import SeedAttribute
import Search exposing (UniqueSearch(..))


type Route
    = ProductDetails String
    | CategoryDetails String Pagination.Data
    | AdvancedSearch
    | SearchResults Search.Data Pagination.Data
    | PageDetails String
    | NotFound


parseRoute : Navigation.Location -> Route
parseRoute =
    let
        searchParser =
            [ ( "all-products", identity )
            , ( "organic", (\s -> { s | isOrganic = True }) )
            , ( "heirloom", (\s -> { s | isHeirloom = True }) )
            , ( "south-east", (\s -> { s | isRegional = True }) )
            , ( "ecological", (\s -> { s | isEcological = True }) )
            ]
                |> List.map
                    (\( slug, modifier ) ->
                        Url.s slug
                            |> Url.map (SearchResults <| modifier Search.initial)
                            |> Pagination.fromQueryString
                    )
                |> (::)
                    (Url.map SearchResults (Url.s "search")
                        |> Search.fromQueryString
                        |> Pagination.fromQueryString
                    )
                |> Url.oneOf

        routeParser =
            Url.oneOf
                [ Url.map (PageDetails "home") Url.top
                , Url.map ProductDetails (Url.s "products" </> Url.string)
                , Url.map CategoryDetails (Url.s "categories" </> Url.string)
                    |> Pagination.fromQueryString
                , Url.map AdvancedSearch (Url.s "search" </> Url.s "advanced")
                , searchParser
                , Url.map SearchResults (Url.s "search")
                    |> Search.fromQueryString
                    |> Pagination.fromQueryString
                , Url.map PageDetails (Url.string)
                ]
    in
        Url.parsePath routeParser
            >> Maybe.withDefault NotFound


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
            let
                specialSearchUrl str =
                    joinPath [ str ]
                        ++ withQueryStrings
                            [ Pagination.toQueryString pagination ]
            in
                case Search.uniqueSearch data of
                    Nothing ->
                        joinPath [ "search" ]
                            ++ withQueryStrings
                                [ Search.toQueryString data
                                , Pagination.toQueryString pagination
                                ]

                    Just searchType ->
                        case searchType of
                            AllProducts ->
                                specialSearchUrl "all-products"

                            AttributeSearch (SeedAttribute.Organic) ->
                                specialSearchUrl "organic"

                            AttributeSearch (SeedAttribute.Heirloom) ->
                                specialSearchUrl "heirloom"

                            AttributeSearch (SeedAttribute.Regional) ->
                                specialSearchUrl "south-east"

                            AttributeSearch (SeedAttribute.Ecological) ->
                                specialSearchUrl "ecological"

        PageDetails slug ->
            if slug == "home" then
                ""
            else
                joinPath [ slug ]

        NotFound ->
            joinPath [ "page-not-found" ]
