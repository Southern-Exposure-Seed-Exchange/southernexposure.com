module Routing exposing
    ( AdminRoute(..)
    , Key
    , Route(..)
    , authRequired
    , isAdminRoute
    , newUrl
    , parseRoute
    , reverse
    )

import Browser.Navigation
import Category exposing (CategoryId(..))
import Products.Pagination as Pagination
import Routing.Utils exposing (joinPath, optionalIntParam, parseFlag, queryFlag, queryParameter, withQueryStrings)
import Search exposing (UniqueSearch(..))
import SeedAttribute
import StaticPage exposing (StaticPageId)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Route
    = ProductDetails String
    | CategoryDetails String Pagination.Data
    | AdvancedSearch
    | SearchResults Search.Data Pagination.Data
    | PageDetails String
    | CreateAccount
    | CreateAccountSuccess
    | Login (Maybe String)
    | ResetPassword (Maybe String)
    | MyAccount
    | EditLogin
    | EditAddress
    | OrderDetails Int
    | Cart
    | QuickOrder
    | Checkout
    | CheckoutSuccess Int Bool
    | Admin AdminRoute
    | NotFound


{-| Does the 'Route' specify a Page to the Admin Backend?
-}
isAdminRoute : Route -> Bool
isAdminRoute route =
    case route of
        Admin _ ->
            True

        _ ->
            False


type AdminRoute
    = Dashboard
    | CategoryList
    | CategoryNew
    | CategoryEdit CategoryId
    | PageList
    | PageNew
    | PageEdit StaticPageId
    | OrderList Int Int


parseRoute : Url -> Route
parseRoute =
    let
        searchParser =
            [ ( "all-products", identity )
            , ( "organic", \s -> { s | isOrganic = True } )
            , ( "heirloom", \s -> { s | isHeirloom = True } )
            , ( "south-east", \s -> { s | isRegional = True } )
            , ( "small-grower", \s -> { s | isSmallGrower = True } )
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

        accountParser =
            Url.oneOf
                [ Url.map Login (Url.s "login" <?> Query.string "redirect")
                , Url.map CreateAccount (Url.s "create")
                , Url.map CreateAccountSuccess (Url.s "create" </> Url.s "success")
                , Url.map ResetPassword (Url.s "reset-password" <?> Query.string "code")
                , Url.map MyAccount Url.top
                , Url.map EditLogin (Url.s "edit")
                , Url.map EditAddress (Url.s "addresses")
                , Url.map OrderDetails (Url.s "order" </> Url.int)
                ]

        adminParser =
            Url.oneOf
                [ Url.map Dashboard Url.top
                , Url.map CategoryList (Url.s "categories")
                , Url.map CategoryNew (Url.s "categories" </> Url.s "new")
                , Url.map (CategoryEdit << CategoryId) (Url.s "categories" </> Url.s "edit" </> Url.int)
                , Url.map PageList (Url.s "pages")
                , Url.map PageNew (Url.s "pages" </> Url.s "new")
                , Url.map PageEdit (Url.s "pages" </> Url.s "edit" </> StaticPage.idPath)
                , Url.map OrderList (Url.s "orders" <?> optionalIntParam "page" 1 <?> optionalIntParam "perPage" 50)
                ]

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
                , Url.s "account" </> accountParser
                , Url.map Cart (Url.s "cart")
                , Url.map QuickOrder (Url.s "quick-order")
                , Url.map Checkout (Url.s "checkout")
                , Url.map CheckoutSuccess (Url.s "checkout" </> Url.s "success" </> Url.int <?> parseFlag "newAccount")
                , Url.map Admin <| Url.s "admin" </> adminParser
                , Url.map PageDetails Url.string
                ]
    in
    Url.parse routeParser
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

                        AttributeSearch SeedAttribute.Organic ->
                            specialSearchUrl "organic"

                        AttributeSearch SeedAttribute.Heirloom ->
                            specialSearchUrl "heirloom"

                        AttributeSearch SeedAttribute.Regional ->
                            specialSearchUrl "south-east"

                        AttributeSearch SeedAttribute.SmallGrower ->
                            specialSearchUrl "small-grower"

        PageDetails slug ->
            if slug == "home" then
                "/"

            else
                joinPath [ slug ]

        CreateAccount ->
            joinPath [ "account", "create" ]

        CreateAccountSuccess ->
            joinPath [ "account", "create", "success" ]

        Login redirectTo ->
            let
                queryParams =
                    case redirectTo of
                        Nothing ->
                            ""

                        Just url ->
                            withQueryStrings [ queryParameter ( "redirect", url ) ]
            in
            joinPath [ "account", "login" ] ++ queryParams

        ResetPassword _ ->
            joinPath [ "account", "reset-password" ]

        MyAccount ->
            joinPath [ "account" ]

        EditLogin ->
            joinPath [ "account", "edit" ]

        EditAddress ->
            joinPath [ "account", "addresses" ]

        OrderDetails orderId ->
            joinPath [ "account", "order", String.fromInt orderId ]

        Cart ->
            joinPath [ "cart" ]

        QuickOrder ->
            joinPath [ "quick-order" ]

        Checkout ->
            joinPath [ "checkout" ]

        CheckoutSuccess orderId newAccount ->
            joinPath [ "checkout", "success", String.fromInt orderId ]
                ++ withQueryStrings [ queryFlag "newAccount" newAccount ]

        Admin adminRoute ->
            reverseAdmin adminRoute

        NotFound ->
            joinPath [ "page-not-found" ]


reverseAdmin : AdminRoute -> String
reverseAdmin route =
    let
        basePath =
            case route of
                Dashboard ->
                    []

                CategoryList ->
                    [ "categories" ]

                CategoryNew ->
                    [ "categories", "new" ]

                CategoryEdit (CategoryId cid) ->
                    [ "categories", "edit", String.fromInt cid ]

                PageList ->
                    [ "pages" ]

                PageNew ->
                    [ "pages", "new" ]

                PageEdit pageId ->
                    [ "pages", "edit", StaticPage.idToString pageId ]

                OrderList _ _ ->
                    [ "orders" ]

        queryStrings =
            case route of
                Dashboard ->
                    []

                CategoryList ->
                    []

                CategoryNew ->
                    []

                CategoryEdit _ ->
                    []

                PageList ->
                    []

                PageNew ->
                    []

                PageEdit _ ->
                    []

                OrderList page perPage ->
                    [ queryParameter ( "page", String.fromInt page )
                    , queryParameter ( "perPage", String.fromInt perPage )
                    ]
    in
    joinPath ("admin" :: basePath)
        ++ withQueryStrings queryStrings


authRequired : Route -> Bool
authRequired route =
    case route of
        ProductDetails _ ->
            False

        CategoryDetails _ _ ->
            False

        AdvancedSearch ->
            False

        SearchResults _ _ ->
            False

        PageDetails _ ->
            False

        CreateAccount ->
            False

        CreateAccountSuccess ->
            True

        Login _ ->
            False

        ResetPassword _ ->
            False

        MyAccount ->
            True

        EditLogin ->
            True

        EditAddress ->
            True

        OrderDetails _ ->
            True

        Cart ->
            False

        QuickOrder ->
            False

        Checkout ->
            False

        CheckoutSuccess _ _ ->
            True

        Admin _ ->
            True

        NotFound ->
            False


type alias Key =
    Browser.Navigation.Key


newUrl : Key -> Route -> Cmd msg
newUrl key =
    reverse >> Browser.Navigation.pushUrl key
