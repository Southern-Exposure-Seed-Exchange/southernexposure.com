module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, text, div, h1, h3, h4, hr, node, br, a, img, span, button, ul, li, small, table, tbody, tr, td, b, label, select, option)
import Html.Attributes exposing (attribute, id, class, href, src, type_, target, tabindex, title, value, for, selected)
import Html.Attributes.Extra exposing (innerHtml)
import Html.Events exposing (on, targetValue)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode
import Navigation
import Paginate exposing (PaginatedList)
import RemoteData exposing (WebData)
import UrlParser as Url exposing ((</>), (<?>))
import Category exposing (CategoryId(..), Category)
import Product exposing (Product, ProductVariant, SeedAttribute)
import Products.Pagination as Pagination
import Products.Sorting as Sorting


main : Program Never Model Msg
main =
    Navigation.program (parseRoute >> UrlUpdate)
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- ROUTING


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


routeLinkAttributes : Route -> List (Html.Attribute Msg)
routeLinkAttributes route =
    [ onClickPreventDefault <| NavigateTo route
    , href <| reverse route
    ]



-- MODEL


type alias Model =
    { navData : WebData CategoryNavData
    , route : Route
    , routeData : RouteData
    }


type alias RouteData =
    { categoryDetails : WebData CategoryDetailsData
    , productDetails : WebData ProductDetailsData
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseRoute location

        ( model, cmd ) =
            fetchDataForRoute
                { navData = RemoteData.Loading
                , route = route
                , routeData =
                    { categoryDetails = RemoteData.NotAsked
                    , productDetails = RemoteData.NotAsked
                    }
                }
    in
        ( model
        , Cmd.batch
            [ cmd
            , getCategoryNavData
            ]
        )


type alias ProductDetailsData =
    { product : Product
    , variants : List ProductVariant
    , maybeSeedAttribute : Maybe SeedAttribute
    , categories : List Category
    }


type alias CategoryDetailsData =
    { category : Category
    , subCategories : List Category
    , products : PaginatedList ( Product, List ProductVariant, Maybe SeedAttribute )
    }


type alias CategoryNavData =
    { roots : List Category
    , children : Dict Int (List Category)
    }



-- COMMANDS


fetchDataForRoute : Model -> ( Model, Cmd Msg )
fetchDataForRoute ({ route, routeData } as model) =
    let
        ( data, cmd ) =
            case route of
                ProductDetails slug ->
                    ( { routeData | productDetails = RemoteData.Loading }
                    , getProductDetailsData slug
                    )

                CategoryDetails slug _ _ ->
                    ( { routeData | categoryDetails = RemoteData.Loading }
                    , getCategoryDetailsData slug
                    )
    in
        ( { model | routeData = data }, cmd )


getProductDetailsData : String -> Cmd Msg
getProductDetailsData slug =
    Http.get ("/api/products/" ++ slug ++ "/") productDetailsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetProductDetailsData


getCategoryDetailsData : String -> Cmd Msg
getCategoryDetailsData slug =
    Http.get ("/api/categories/details/" ++ slug ++ "/") categoryDetailsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetCategoryDetailsData


getCategoryNavData : Cmd Msg
getCategoryNavData =
    Http.get "/api/categories/nav/" categoryNavDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetCategoryNavData


productDetailsDecoder : Decode.Decoder ProductDetailsData
productDetailsDecoder =
    Decode.map4 ProductDetailsData
        (Decode.field "product" Product.decoder)
        (Decode.field "variants" <| Decode.list Product.variantDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable Product.seedAttributeDecoder)
        (Decode.field "categories" <| Decode.list Category.decoder)


categoryDetailsDecoder : Decode.Decoder CategoryDetailsData
categoryDetailsDecoder =
    let
        productDataDecoder =
            Decode.map (Paginate.fromList (.perPage Pagination.default)) <|
                Decode.list <|
                    Decode.map3 (,,)
                        (Decode.field "product" Product.decoder)
                        (Decode.field "variants" <| Decode.list Product.variantDecoder)
                        (Decode.field "seedAttribute" <| Decode.nullable Product.seedAttributeDecoder)
    in
        Decode.map3 CategoryDetailsData
            (Decode.field "category" Category.decoder)
            (Decode.field "subCategories" <| Decode.list Category.decoder)
            (Decode.field "products" productDataDecoder)


stringToIntKeys : Dict String v -> Dict Int v
stringToIntKeys =
    Dict.foldl
        (\key value newDict ->
            case String.toInt key of
                Err _ ->
                    newDict

                Ok newKey ->
                    Dict.insert newKey value newDict
        )
        Dict.empty


categoryNavDecoder : Decode.Decoder CategoryNavData
categoryNavDecoder =
    Decode.map2 CategoryNavData
        (Decode.field "rootCategories" <| Decode.list Category.decoder)
        (Decode.field "childrenCategories" <|
            Decode.map stringToIntKeys <|
                Decode.dict <|
                    Decode.list Category.decoder
        )



-- UPDATE


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | GetProductDetailsData (WebData ProductDetailsData)
    | GetCategoryDetailsData (WebData CategoryDetailsData)
    | GetCategoryNavData (WebData CategoryNavData)


urlUpdate : Route -> Model -> ( Model, Cmd Msg )
urlUpdate newRoute ({ routeData } as model) =
    let
        modelWithNewRoute =
            { model | route = newRoute }
    in
        case ( newRoute, model.route ) of
            ( CategoryDetails newSlug newPagination newSort, CategoryDetails oldSlug _ _ ) ->
                if newSlug /= oldSlug then
                    fetchDataForRoute modelWithNewRoute
                else
                    ( { modelWithNewRoute | routeData = updateRouteData newRoute routeData }
                    , Cmd.none
                    )

            _ ->
                fetchDataForRoute modelWithNewRoute


updateRouteData : Route -> RouteData -> RouteData
updateRouteData route data =
    let
        modifyCategoryProductsByParams pagination sortData webData =
            sortAndSetPaginatedList pagination
                (Sorting.apply sortData)
                (\d ps -> { d | products = ps })
                .products
                webData
    in
        case route of
            ProductDetails _ ->
                data

            CategoryDetails _ pagination sortData ->
                { data | categoryDetails = modifyCategoryProductsByParams pagination sortData data.categoryDetails }


sortAndSetPaginatedList :
    Pagination.Data
    -> (List a -> List a)
    -> (b -> PaginatedList a -> b)
    -> (b -> PaginatedList a)
    -> WebData b
    -> WebData b
sortAndSetPaginatedList { page, perPage } sortFunction updateFunction selector data =
    let
        setPaginatedListConstraints =
            Paginate.changeItemsPerPage perPage
                >> Paginate.goTo page

        sortPaginatedList =
            Paginate.map sortFunction
    in
        RemoteData.map
            (\d ->
                selector d
                    |> setPaginatedListConstraints
                    |> sortPaginatedList
                    |> updateFunction d
            )
            data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ routeData } as model) =
    case msg of
        UrlUpdate route ->
            urlUpdate route model

        NavigateTo route ->
            ( model, Navigation.newUrl <| reverse route )

        GetProductDetailsData response ->
            let
                updatedRouteData =
                    { routeData | productDetails = response }
            in
                ( { model | routeData = updatedRouteData }, Cmd.none )

        GetCategoryDetailsData response ->
            let
                updatedRouteData =
                    { routeData | categoryDetails = response }
                        |> updateRouteData model.route
            in
                ( { model | routeData = updatedRouteData }, Cmd.none )

        GetCategoryNavData response ->
            ( { model | navData = logUnsuccessfulRequest response }, Cmd.none )


logUnsuccessfulRequest : WebData a -> WebData a
logUnsuccessfulRequest response =
    case response of
        RemoteData.Success _ ->
            response

        _ ->
            Debug.log "Unsuccessful Request Returned" response



-- VIEW


view : Model -> Html Msg
view { route, routeData, navData } =
    let
        siteHeader =
            div [ class "container" ]
                [ div [ id "site-header", class "row clearfix" ]
                    [ div [ class "col-sm-7 col-lg-6" ]
                        [ div [ class "media" ]
                            [ a [ href "/" ]
                                [ img
                                    [ id "site-logo"
                                    , class "float-left mx-3"
                                    , src "/static/img/logos/sese.png"
                                    ]
                                    []
                                ]
                            , div [ id "site-title", class "media-body my-auto" ]
                                [ h1 [ class "media-heading m-0" ]
                                    [ a [ href "/" ]
                                        [ text "Southern Exposure"
                                        , br [] []
                                        , text "Seed Exchange"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "col-sm-5 col-lg-6 d-none d-sm-block text-right" ]
                        [ text "LINKS / SEARCH" ]
                    ]
                ]

        categoryNavigation =
            RemoteData.toMaybe navData
                |> Maybe.map (\data -> List.map (rootCategory data.children) data.roots)
                |> Maybe.withDefault []

        rootCategory children category =
            let
                (CategoryId categoryId) =
                    category.id

                dropdownCategories children =
                    let
                        childItems =
                            List.map childCategory children

                        itemsPerColumn =
                            ceiling <| (toFloat <| List.length childItems) / 3.0

                        splitItems items =
                            [ List.take itemsPerColumn items
                            , List.drop itemsPerColumn items |> List.take itemsPerColumn
                            , List.drop (itemsPerColumn * 2) items
                            ]
                    in
                        if List.length childItems < 15 then
                            childItems
                        else
                            [ div [ class "row no-gutters multi-column-dropdown" ] <|
                                List.map (\i -> div [ class "col" ] i)
                                    (splitItems childItems)
                            ]
            in
                case Dict.get categoryId children of
                    Nothing ->
                        li [ class "nav-item" ]
                            [ a
                                ([ class "nav-link" ]
                                    ++ (routeLinkAttributes <|
                                            CategoryDetails category.slug
                                                Pagination.default
                                                Sorting.default
                                       )
                                )
                                [ text category.name ]
                            ]

                    Just children ->
                        li [ class "nav-item dropdown" ]
                            [ a
                                [ class "nav-link dropdown-toggle"
                                , href <|
                                    reverse <|
                                        CategoryDetails category.slug
                                            Pagination.default
                                            Sorting.default
                                , attribute "data-toggle" "dropdown"
                                , attribute "aria-haspopup" "true"
                                , attribute "aria-expanded" "false"
                                ]
                                [ text category.name ]
                            , div [ class "dropdown-menu mt-0" ] <| dropdownCategories children
                            ]

        childCategory category =
            a
                ([ class "dropdown-item" ]
                    ++ (routeLinkAttributes <|
                            CategoryDetails
                                category.slug
                                Pagination.default
                                Sorting.default
                       )
                )
                [ text category.name ]

        navigation =
            div [ id "navigation", class "container" ]
                [ node "nav"
                    [ class "navbar navbar-expand-md navbar-light bg-success" ]
                    [ button
                        [ class "navbar-toggler"
                        , type_ "button"
                        , attribute "data-toggle" "collapse"
                        , attribute "data-target" "#category-navbar"
                        , attribute "aria-controls" "navbarSupportedContent"
                        , attribute "aria-expanded" "false"
                        , attribute "aria-label" "Toggle navigation"
                        ]
                        [ span [ class "navbar-toggler-icon" ] [] ]
                    , div [ id "category-navbar", class "collapse navbar-collapse" ]
                        [ ul [ class "navbar-nav mx-auto d-flex text-left" ]
                            categoryNavigation
                        ]
                    ]
                ]

        middleContent =
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col order-md-2" ] pageContent
                    , sidebar
                    ]
                ]

        footer =
            div [ id "footer", class "container" ]
                [ node "footer"
                    []
                    [ div [ class "row" ]
                        [ div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Information" ] ]
                        , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Important Links" ] ]
                        , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Contact Us" ] ]
                        , div [ class "col-sm-12 text-center" ]
                            [ text "Copyright © 2017 Southern Exposure Seed Exchange"
                            ]
                        ]
                    ]
                ]

        sidebar =
            div [ id "sidebar", class "col-12 col-md-3 col-lg-3 col-xl-2 order-md-1" ]
                [ div [ class "card mb-3" ]
                    [ div [ class "card-body text-center" ]
                        [ a [ target "_blank", href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts" ]
                            [ img [ class "img-fluid", src <| staticImage "logos/facebook-big-icon.png" ] [] ]
                        , hr [] []
                        , div [ class "text-center font-weight-bold" ] [ text "Our Partners" ]
                        , a [ target "_blank", href "http://www.smartgardener.com/" ]
                            [ img [ class "mb-3 img-fluid", src <| staticImage "logos/smart-gardener.jpg" ] [] ]
                        , br [] []
                        , a [ target "_blank", href "http://www.localharvest.org/" ]
                            [ img [ class "img-fluid", src <| staticImage "logos/local-harvest.jpg" ] [] ]
                        ]
                    ]
                ]

        pageContent =
            case route of
                ProductDetails _ ->
                    withIntermediateText productDetailsView routeData.productDetails

                CategoryDetails _ pagination sortData ->
                    withIntermediateText (categoryDetailsView pagination sortData) routeData.categoryDetails
    in
        div []
            [ siteHeader
            , navigation
            , middleContent
            , footer
            ]


staticImage : String -> String
staticImage path =
    "/static/img/" ++ path


mediaImage : String -> String
mediaImage path =
    "/media/" ++ path


htmlOrBlank : (a -> Html msg) -> Maybe a -> Html msg
htmlOrBlank renderFunction =
    Maybe.map renderFunction
        >> Maybe.withDefault (text "")


seedAttributeIcons : SeedAttribute -> Html msg
seedAttributeIcons { isOrganic, isHeirloom, isRegional, isEcological } =
    List.filter Tuple.first
        [ ( isOrganic, ( "Certified Organic", "icons/organic-certified.png" ) )
        , ( isHeirloom, ( "Heirloom", "icons/heirloom.png" ) )
        , ( isRegional, ( "Especially well-suited to the Southeast", "icons/southeast.png" ) )
        , ( isEcological, ( "Ecologically Grown", "icons/ecologically-grown.png" ) )
        ]
        |> List.map
            (Tuple.second
                >> (\( iconTitle, url ) ->
                        img [ class "my-auto", title iconTitle, src <| staticImage url ] []
                   )
            )
        |> span [ class "d-inline-block ml-2" ]


withIntermediateText : (a -> List (Html msg)) -> WebData a -> List (Html msg)
withIntermediateText view data =
    case data of
        RemoteData.Loading ->
            [ text "Loading..." ]

        RemoteData.Success d ->
            view d

        e ->
            [ text <| toString e ]


productDetailsView : ProductDetailsData -> List (Html Msg)
productDetailsView { product, variants, maybeSeedAttribute, categories } =
    let
        categoryBlocks =
            List.filter (not << String.isEmpty << .description) categories
                |> List.map
                    (\category ->
                        div [ class "product-category" ]
                            [ h3 [ class "mt-3" ]
                                [ a (routeLinkAttributes <| CategoryDetails category.slug Pagination.default Sorting.default)
                                    [ text category.name ]
                                ]
                            , div [ innerHtml category.description ] []
                            ]
                    )
    in
        [ h1 []
            [ text product.name
            , htmlOrBlank seedAttributeIcons maybeSeedAttribute
            ]
        , hr [] []
        , div [ class "product-details" ]
            [ div [ class "clearfix" ]
                [ div [ class "float-left col-sm-4 col-md-5 col-lg-4" ]
                    [ div
                        [ class "card" ]
                        [ div [ class "card-body text-center p-1" ]
                            [ img
                                [ src << mediaImage <| "products/" ++ product.imageURL
                                , class "img-fluid"
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "float-right col-sm-4 col-md-3 col-lg-3" ]
                    [ div [ class "card" ]
                        [ div [ class "card-body text-center p-2" ]
                            [ h4 [] [ text "$999.99" ]
                            , text "ADD TO CART BUTTON"
                            , small [ class "text-muted d-block" ]
                                [ text <| "Item #" ++ product.baseSKU
                                ]
                            ]
                        ]
                    ]
                , div [ class "col" ]
                    [ div [ innerHtml product.longDescription ] [] ]
                , div [ class "col-12" ] categoryBlocks
                ]
            ]
        ]


categoryDetailsView : Pagination.Data -> Sorting.Option -> CategoryDetailsData -> List (Html Msg)
categoryDetailsView pagination sortData { category, subCategories, products } =
    let
        subCategoryCards =
            if List.length subCategories > 0 then
                List.map subCategoryCard subCategories
                    |> div [ class "row" ]
            else
                text ""

        subCategoryCard category =
            div [ class "col-6 col-sm-4 col-md-3 mb-2" ]
                [ a (routeLinkAttributes <| CategoryDetails category.slug Pagination.default Sorting.default)
                    [ div [ class "h-100 text-center" ]
                        [ img [ class "img-fluid mx-auto", src <| mediaImage category.imageURL ] []
                        , div [ class "my-auto" ] [ text category.name ]
                        ]
                    ]
                ]

        sortHtml =
            if Paginate.length products > 1 then
                div [ class "d-flex mb-2 justify-content-between align-items-center" ] [ sortingInput ]
            else
                text ""

        sortingInput : Html Msg
        sortingInput =
            div [ class "form-inline" ]
                [ label [ class "col-form-label font-weight-bold", for "product-sort-select" ]
                    [ text "Sort by:" ]
                , text " "
                , select
                    [ id "product-sort-select"
                    , class "form-control form-control-sm ml-2"
                    , onProductsSortSelect (NavigateTo << CategoryDetails category.slug pagination)
                    ]
                  <|
                    List.map
                        (\data ->
                            option
                                [ value <| Sorting.toQueryValue data
                                , selected (data == sortData)
                                ]
                                [ text <| Sorting.toDescription data ]
                        )
                        Sorting.all
                ]

        onProductsSortSelect : (Sorting.Option -> msg) -> Html.Attribute msg
        onProductsSortSelect msg =
            targetValue
                |> Decode.map (Sorting.fromQueryValue >> msg)
                |> on "change"

        paginationHtml =
            div [ class "d-flex mb-2 justify-content-between align-items-center" ] [ pagingText, pager ]

        pagingText =
            if Paginate.length products == 0 then
                text ""
            else
                span []
                    [ text "Displaying "
                    , b [] [ text <| pagingStart () ]
                    , text " to "
                    , b [] [ text <| pagingEnd () ]
                    , text " (of "
                    , b [] [ text <| toString <| Paginate.length products ]
                    , text " products)"
                    ]

        pagingStart _ =
            toString <|
                (Paginate.currentPage products - 1)
                    * pagination.perPage
                    + 1

        pagingEnd _ =
            toString <|
                if Paginate.isLast products || Paginate.length products < pagination.perPage then
                    Paginate.length products
                else
                    (Paginate.currentPage products * pagination.perPage)

        pager =
            if Paginate.totalPages products <= 1 then
                text ""
            else
                node "nav"
                    [ attribute "aria-label" "Category Product Pages" ]
                    [ ul [ class "pagination pagination-sm mb-0" ] <|
                        previousLink ()
                            :: Paginate.pager renderPager products
                            ++ [ nextLink () ]
                    ]

        previousLink _ =
            let
                previousPage =
                    max 1 (pagination.page - 1)

                previousRoute =
                    CategoryDetails category.slug { pagination | page = previousPage } sortData
            in
                prevNextLink Paginate.isFirst previousRoute "« Prev"

        nextLink _ =
            let
                nextPage =
                    min (Paginate.totalPages products) (pagination.page + 1)

                nextRoute =
                    CategoryDetails category.slug { pagination | page = nextPage } sortData
            in
                prevNextLink Paginate.isLast nextRoute "Next »"

        prevNextLink isDisabled route content =
            let
                ( itemClass, linkAttrs ) =
                    if isDisabled products then
                        ( " disabled", [ tabindex -1 ] )
                    else
                        ( "", [] )
            in
                li [ class <| "page-item" ++ itemClass ]
                    [ a (class "page-link" :: linkAttrs ++ routeLinkAttributes route)
                        [ text content ]
                    ]

        renderPager page isCurrent =
            let
                itemClass =
                    if isCurrent then
                        "page-item active"
                    else
                        "page-item"
            in
                li [ class itemClass ]
                    [ a
                        ([ class "page-link" ]
                            ++ routeLinkAttributes
                                (CategoryDetails category.slug
                                    { pagination | page = page }
                                    sortData
                                )
                        )
                        [ text <| toString page ]
                    ]

        productRows =
            flip List.map (Paginate.page products) <|
                \( product, variants, maybeSeedAttribute ) ->
                    tr []
                        [ td [ class "category-product-image text-center align-middle" ]
                            [ a (routeLinkAttributes <| ProductDetails product.slug)
                                [ img
                                    [ src <| mediaImage <| "products/" ++ product.imageURL
                                    ]
                                    []
                                ]
                            ]
                        , td []
                            [ h3 [ class "mb-0" ]
                                [ a
                                    ([ innerHtml product.name ]
                                        ++ (routeLinkAttributes <| ProductDetails product.slug)
                                    )
                                    []
                                , htmlOrBlank seedAttributeIcons maybeSeedAttribute
                                ]
                            , div [ innerHtml product.longDescription ] []
                            ]
                        , td [ class "text-center align-middle" ]
                            [ div []
                                [ div [ class "font-weight-bold" ] [ text "$999.99" ]
                                , div [] [ text "CART_INPUT" ]
                                , small [ class "text-muted" ] [ text <| "Item # " ++ product.baseSKU ]
                                ]
                            ]
                        ]
    in
        [ div [ class "d-flex align-items-center" ]
            [ img [ class "img-fluid", src <| mediaImage category.imageURL ] []
            , h1 [ class "mb-0 pl-2" ] [ text category.name ]
            ]
        , hr [ class "mt-2" ] []
        , div [ innerHtml category.description ] []
        , subCategoryCards
        , sortHtml
        , paginationHtml
        , table [ class "category-products table table-striped table-sm mb-2" ]
            [ tbody [] <| productRows ]
        , paginationHtml
        ]
