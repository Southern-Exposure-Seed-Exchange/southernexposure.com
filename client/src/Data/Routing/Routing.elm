module Data.Routing.Routing exposing
    ( AdminRoute(..)
    , Key
    , Route(..)
    , authRequired
    , homePage
    , isAdminRoute
    , newUrl
    , parseRoute
    , reverse
    , showSearchbar
    )

import Browser.Navigation
import Components.Products.Pagination as Pagination
import Data.Category as Category exposing (CategoryId(..))
import Data.Product as Product exposing (ProductId(..), ProductVariantId(..))
import Data.Routing.Utils exposing (fromStringParam, joinPath, optionalIntParam, parseFlag, queryFlag, queryParameter, withQueryStrings)
import Data.Search as Search exposing (UniqueSearch(..))
import Data.SeedAttribute as SeedAttribute
import Data.StaticPage as StaticPage exposing (StaticPageId)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Route
    = ProductDetails String (Maybe ProductVariantId)
    | CategoryDetails String Pagination.Data
    | AdvancedSearch
    | SearchResults Search.Data Pagination.Data
    | PageDetails String (Maybe String)
    | CreateAccount
    | VerifyEmail String
    | CreateAccountSuccess
    | Login (Maybe String) Bool
    | VerificationRequired Int
    | ResetPassword (Maybe String)
    | MyAccount
    | EditLogin
    | EditAddress
    | OrderDetails Int (Maybe String)
    | Cart
    | QuickOrder
    | Checkout
    | CheckoutSuccess Int (Maybe String)
    | Admin AdminRoute
    | Redirect String
    | NotFound


{-| The Homepage Route.
-}
homePage : Route
homePage =
    PageDetails "home" Nothing


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
    | OrderList { page : Int, perPage : Int, query : String }
    | AdminOrderDetails Int
    | CustomerList { page : Int, perPage : Int, query : String }
    | CustomerEdit Int
    | ProductList
    | ProductNew
    | ProductEdit ProductId
    | CouponList
    | CouponNew
    | CouponEdit Int
    | Surcharges
    | ShippingMethods
    | Settings
    | ProductSaleList
    | ProductSaleNew
    | ProductSaleEdit Int
    | CategorySaleList
    | CategorySaleNew
    | CategorySaleEdit Int


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
                [ Url.map Login (Url.s "login" <?> Query.string "redirect" <?> parseFlag "clearCart")
                , Url.map VerificationRequired (Url.s "unverified" </> Url.int)
                , Url.map CreateAccount (Url.s "create")
                , Url.map CreateAccountSuccess (Url.s "create" </> Url.s "success")
                , Url.map ResetPassword (Url.s "reset-password" <?> Query.string "code")
                , Url.map MyAccount Url.top
                , Url.map EditLogin (Url.s "edit")
                , Url.map EditAddress (Url.s "addresses")
                , Url.map OrderDetails (Url.s "order" </> Url.int <?> Query.string "token")
                , Url.map VerifyEmail (Url.s "verify" </> Url.string)
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
                , Url.map OrderList (Url.s "orders" </> adminPaginationQueryParser)
                , Url.map AdminOrderDetails (Url.s "orders" </> Url.s "details" </> Url.int)
                , Url.map CustomerList (Url.s "customers" </> adminPaginationQueryParser)
                , Url.map CustomerEdit (Url.s "customers" </> Url.s "edit" </> Url.int)
                , Url.map ProductList (Url.s "products")
                , Url.map ProductNew (Url.s "products" </> Url.s "new")
                , Url.map ProductEdit (Url.s "products" </> Url.s "edit" </> Url.map ProductId Url.int)
                , Url.map CouponList (Url.s "coupons")
                , Url.map CouponNew (Url.s "coupons" </> Url.s "new")
                , Url.map CouponEdit (Url.s "coupons" </> Url.s "edit" </> Url.int)
                , Url.map Surcharges (Url.s "surcharges")
                , Url.map ShippingMethods (Url.s "shipping-methods")
                , Url.map Settings (Url.s "settings")
                , Url.map ProductSaleList (Url.s "product-sales")
                , Url.map ProductSaleNew (Url.s "product-sales" </> Url.s "new")
                , Url.map ProductSaleEdit (Url.s "product-sales" </> Url.s "edit" </> Url.int)
                , Url.map CategorySaleList (Url.s "category-sales")
                , Url.map CategorySaleNew (Url.s "category-sales" </> Url.s "new")
                , Url.map CategorySaleEdit (Url.s "category-sales" </> Url.s "edit" </> Url.int)
                ]

        adminPaginationQueryParser =
            Url.map (\page perPage query -> { page = page, perPage = perPage, query = query })
                (Url.top
                    <?> optionalIntParam "page" 1
                    <?> optionalIntParam "perPage" 50
                    <?> fromStringParam "query" identity
                )

        redirectParser =
            Url.oneOf
                [ Url.map (Redirect << prefixPaths "/blog")
                    (Url.s "blog" </> parseRest 10)
                , Url.map (Redirect << prefixPaths "/newsletter")
                    (Url.s "newsletter" </> parseRest 10)
                , Url.map (Redirect << prefixPaths "https://seedracks.southernexposure.com")
                    (Url.s "seedracks" </> parseRest 10)
                , Url.map (Redirect << always "https://heritageharvestfestival.com")
                    (Url.s "hhf" </> parseRest 10)
                , Url.map (Redirect << always "https://gardenplanner.southernexposure.com")
                    (Url.s "gardenplanner" </> parseRest 10)
                ]

        prefixPaths prefix paths =
            prefix ++ "/" ++ String.join "/" paths

        routeParser =
            Url.oneOf
                [ Url.map (PageDetails "home" Nothing) Url.top
                , Url.map (PageDetails "home" Nothing) (Url.s "index.html")
                , Url.map ProductDetails (Url.s "products" </> Url.string <?> Query.map (Maybe.map ProductVariantId) (Query.int "variant"))
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
                , Url.map CheckoutSuccess (Url.s "checkout" </> Url.s "success" </> Url.int <?> Query.string "token")
                , Url.map Admin <| Url.s "admin" </> adminParser
                , redirectParser
                , Url.map PageDetails (Url.string </> Url.fragment identity)
                ]
    in
    Url.parse routeParser
        >> Maybe.withDefault NotFound


{-| Parse the remainder of the path
-}
parseRest : Int -> Url.Parser (List String -> a) a
parseRest depth =
    if depth < 1 then
        Url.map [] Url.top

    else
        Url.oneOf
            [ Url.map [] Url.top
            , Url.map (\str list -> str :: list) (Url.string </> parseRestHelper (depth - 1))
            ]


{-| `parseRest` needs this helper function to typecheck properly for some reason...
-}
parseRestHelper : Int -> Url.Parser (List String -> a) a
parseRestHelper depth =
    if depth < 1 then
        Url.map [] Url.top

    else
        Url.oneOf
            [ Url.map [] Url.top
            , Url.map (\str list -> str :: list) (Url.string </> parseRest (depth - 1))
            ]


reverse : Route -> String
reverse route =
    case route of
        ProductDetails slug id ->
            joinPath [ "products", slug ]
                ++ withQueryStrings
                    (Maybe.map
                        ((\(ProductVariantId i) -> i)
                            >> String.fromInt
                            >> Tuple.pair "variant"
                            >> queryParameter
                            >> List.singleton
                        )
                        id
                        |> Maybe.withDefault []
                    )

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

        PageDetails slug fragment ->
            if slug == "home" then
                "/" ++ reverseFragment fragment

            else
                joinPath [ slug ] ++ reverseFragment fragment

        CreateAccount ->
            joinPath [ "account", "create" ]

        VerifyEmail uuid ->
            joinPath [ "account", "verify", uuid ]

        CreateAccountSuccess ->
            joinPath [ "account", "create", "success" ]

        Login redirectTo clearCart ->
            let
                queryParams =
                    case redirectTo of
                        Nothing ->
                            withQueryStrings [ queryFlag "clearCart" clearCart ]

                        Just url ->
                            withQueryStrings
                                [ queryParameter ( "redirect", url )
                                , queryFlag "clearCart" clearCart
                                ]
            in
            joinPath [ "account", "login" ] ++ queryParams

        VerificationRequired customerId ->
            joinPath [ "account", "unverified", String.fromInt customerId ]

        ResetPassword _ ->
            joinPath [ "account", "reset-password" ]

        MyAccount ->
            joinPath [ "account" ]

        EditLogin ->
            joinPath [ "account", "edit" ]

        EditAddress ->
            joinPath [ "account", "addresses" ]

        OrderDetails orderId token ->
            joinPath [ "account", "order", String.fromInt orderId ]
                ++ withQueryStrings
                    [ case token of
                        Nothing ->
                            ""

                        Just tok ->
                            queryParameter ( "token", tok )
                    ]

        Cart ->
            joinPath [ "cart" ]

        QuickOrder ->
            joinPath [ "quick-order" ]

        Checkout ->
            joinPath [ "checkout" ]

        CheckoutSuccess orderId token ->
            joinPath [ "checkout", "success", String.fromInt orderId ]
                ++ withQueryStrings
                    [ case token of
                        Nothing ->
                            ""

                        Just tok ->
                            queryParameter ( "token", tok )
                    ]

        Admin adminRoute ->
            reverseAdmin adminRoute

        Redirect path ->
            path

        NotFound ->
            joinPath [ "page-not-found" ]


reverseFragment : Maybe String -> String
reverseFragment =
    Maybe.map (\s -> "#" ++ s)
        >> Maybe.withDefault ""


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

                OrderList _ ->
                    [ "orders" ]

                AdminOrderDetails orderId ->
                    [ "orders", "details", String.fromInt orderId ]

                CustomerList _ ->
                    [ "customers" ]

                CustomerEdit customerId ->
                    [ "customers", "edit", String.fromInt customerId ]

                ProductList ->
                    [ "products" ]

                ProductNew ->
                    [ "products", "new" ]

                ProductEdit (ProductId pId) ->
                    [ "products", "edit", String.fromInt pId ]

                CouponList ->
                    [ "coupons" ]

                CouponNew ->
                    [ "coupons", "new" ]

                CouponEdit couponId ->
                    [ "coupons", "edit", String.fromInt couponId ]

                Surcharges ->
                    [ "surcharges" ]

                ShippingMethods ->
                    [ "shipping-methods" ]

                Settings ->
                    [ "settings" ]

                ProductSaleList ->
                    [ "product-sales" ]

                ProductSaleNew ->
                    [ "product-sales", "new" ]

                ProductSaleEdit id ->
                    [ "product-sales", "edit", String.fromInt id ]

                CategorySaleList ->
                    [ "category-sales" ]

                CategorySaleNew ->
                    [ "category-sales", "new" ]

                CategorySaleEdit id ->
                    [ "category-sales", "edit", String.fromInt id ]

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

                OrderList params ->
                    recordToQueryParams params

                AdminOrderDetails _ ->
                    []

                CustomerList params ->
                    recordToQueryParams params

                CustomerEdit _ ->
                    []

                ProductList ->
                    []

                ProductNew ->
                    []

                ProductEdit _ ->
                    []

                CouponList ->
                    []

                CouponNew ->
                    []

                CouponEdit _ ->
                    []

                Surcharges ->
                    []

                ShippingMethods ->
                    []

                Settings ->
                    []

                ProductSaleList ->
                    []

                ProductSaleNew ->
                    []

                ProductSaleEdit _ ->
                    []

                CategorySaleList ->
                    []

                CategorySaleNew ->
                    []

                CategorySaleEdit _ ->
                    []

        recordToQueryParams { page, perPage, query } =
            List.concat
                [ unlessDefault "page" (String.fromInt page) "1"
                , unlessDefault "perPage" (String.fromInt perPage) "50"
                , unlessDefault "query" query ""
                ]

        unlessDefault name value default =
            if value == default then
                []

            else
                [ queryParameter ( name, value ) ]
    in
    joinPath ("admin" :: basePath)
        ++ withQueryStrings queryStrings


authRequired : Route -> Bool
authRequired route =
    case route of
        ProductDetails _ _ ->
            False

        CategoryDetails _ _ ->
            False

        AdvancedSearch ->
            False

        SearchResults _ _ ->
            False

        PageDetails _ _ ->
            False

        CreateAccount ->
            False

        VerifyEmail _ ->
            False

        CreateAccountSuccess ->
            True

        Login _ _ ->
            False

        VerificationRequired _ ->
            False

        ResetPassword _ ->
            False

        MyAccount ->
            True

        EditLogin ->
            True

        EditAddress ->
            True

        OrderDetails _ token ->
            token == Nothing

        Cart ->
            False

        QuickOrder ->
            False

        Checkout ->
            False

        CheckoutSuccess _ token ->
            token == Nothing

        Admin _ ->
            True

        Redirect _ ->
            False

        NotFound ->
            False


type alias Key =
    Browser.Navigation.Key


newUrl : Key -> Route -> Cmd msg
newUrl key =
    reverse >> Browser.Navigation.pushUrl key


showSearchbar : Route -> Bool
showSearchbar route =
    case route of
        Checkout ->
            False

        CheckoutSuccess _ _ ->
            False

        ProductDetails _ _ ->
            False

        MyAccount ->
            False

        EditLogin ->
            False

        EditAddress ->
            False

        CreateAccount ->
            False

        Login _ _ ->
            False

        VerifyEmail _ ->
            False

        CreateAccountSuccess ->
            False

        VerificationRequired _ ->
            False

        OrderDetails _ _ ->
            False

        Redirect _ ->
            False

        NotFound ->
            False

        ResetPassword _ ->
            False

        Cart ->
            False

        _ ->
            True
