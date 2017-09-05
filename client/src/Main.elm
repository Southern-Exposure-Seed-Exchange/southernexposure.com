module Main exposing (main)

import Html exposing (Html, text, div, h1, h3, h4, hr, node, br, a, img, span, button, ul, li, small, table, tbody, tr, td, b, label, select, option, p, form, input)
import Html.Attributes exposing (attribute, id, class, href, src, type_, target, tabindex, title, value, for, selected)
import Html.Attributes.Extra exposing (innerHtml)
import Html.Events exposing (on, targetValue, onSubmit)
import Http
import Json.Decode as Decode
import Navigation
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import AdvancedSearch
import Category exposing (Category, CategoryId(..))
import Messages exposing (Msg(..))
import PageData exposing (PageData, ProductData)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(..), reverse, parseRoute)
import Search exposing (UniqueSearch(..))
import SeedAttribute exposing (SeedAttribute)
import SiteUI exposing (NavigationData)
import SiteUI.Footer as SiteFooter
import SiteUI.Header as SiteHeader
import SiteUI.Navigation as SiteNavigation
import SiteUI.Search as SiteSearch
import SiteUI.Sidebar as SiteSidebar
import StaticPage exposing (StaticPage)
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes, htmlOrBlank)


main : Program Never Model Msg
main =
    Navigation.program (parseRoute >> UrlUpdate)
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { navigationData : WebData NavigationData
    , route : Route
    , pageData : PageData
    , searchData : Search.Data
    , advancedSearchData : Search.Data
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseRoute location

        ( model, cmd ) =
            fetchDataForRoute
                { navigationData = RemoteData.Loading
                , route = route
                , pageData = PageData.initial
                , searchData = Search.initial
                , advancedSearchData = Search.initial
                }
    in
        ( model
        , Cmd.batch
            [ cmd
            , getNavigationData
            ]
        )



-- COMMANDS


{-| TODO: Move to PageData module?
-}
fetchDataForRoute : Model -> ( Model, Cmd Msg )
fetchDataForRoute ({ route, pageData } as model) =
    let
        discardCmd f ( a, _ ) =
            f a

        updateCategoryDetails slug pagination products =
            products
                |> Paginate.updateData PageData.categoryConfig
                    { slug = slug, sorting = pagination.sorting }
                |> discardCmd (Paginate.updatePerPage PageData.categoryConfig pagination.perPage)
                |> discardCmd (Paginate.jumpTo PageData.categoryConfig pagination.page)

        ( data, cmd ) =
            case route of
                ProductDetails slug ->
                    ( { pageData | productDetails = RemoteData.Loading }
                    , getProductDetailsData slug
                    )

                CategoryDetails slug pagination ->
                    updateCategoryDetails slug pagination pageData.categoryDetails
                        |> Tuple.mapFirst (\cd -> { pageData | categoryDetails = cd })
                        |> Tuple.mapSecond (Cmd.map CategoryPaginationMsg)

                AdvancedSearch ->
                    ( { pageData | advancedSearch = RemoteData.Loading }
                    , getAdvancedSearchData
                    )

                SearchResults data pagination ->
                    pageData.searchResults
                        |> Paginate.updateData PageData.searchConfig
                            { data = data, sorting = pagination.sorting }
                        |> discardCmd (Paginate.updatePerPage PageData.searchConfig pagination.perPage)
                        |> discardCmd (Paginate.jumpTo PageData.searchConfig pagination.page)
                        |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                        |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)

                PageDetails slug ->
                    ( { pageData | pageDetails = RemoteData.Loading }
                    , getPageDetails slug
                    )
    in
        ( { model | pageData = data }, cmd )


sendRequest : (WebData a -> msg) -> Http.Request a -> Cmd msg
sendRequest msg =
    RemoteData.sendRequest >> Cmd.map msg


getProductDetailsData : String -> Cmd Msg
getProductDetailsData slug =
    Http.get ("/api/products/details/" ++ slug ++ "/")
        PageData.productDetailsDecoder
        |> sendRequest GetProductDetailsData


getNavigationData : Cmd Msg
getNavigationData =
    Http.get "/api/categories/nav/" SiteUI.navigationDecoder
        |> sendRequest GetNavigationData


getAdvancedSearchData : Cmd Msg
getAdvancedSearchData =
    Http.get "/api/categories/search/" PageData.advancedSearchDecoder
        |> sendRequest GetAdvancedSearchData


getPageDetails : String -> Cmd Msg
getPageDetails slug =
    Http.get ("/api/pages/details/" ++ slug ++ "/")
        (Decode.field "page" StaticPage.decoder)
        |> sendRequest GetPageDetailsData



-- UPDATE


{-| TODO: Refactor pagedata messages into separate msg & update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ pageData } as model) =
    case msg of
        UrlUpdate route ->
            { model | route = route }
                |> fetchDataForRoute
                |> clearSearchForm

        NavigateTo route ->
            ( model, Navigation.newUrl <| reverse route )

        SearchMsg subMsg ->
            let
                ( searchData, cmd ) =
                    SiteSearch.update subMsg model.searchData
            in
                ( { model | searchData = searchData }, cmd )

        AdvancedSearchMsg subMsg ->
            ( { model | advancedSearchData = AdvancedSearch.update subMsg model.advancedSearchData }
            , Cmd.none
            )

        GetProductDetailsData response ->
            let
                updatedPageData =
                    { pageData | productDetails = response }
            in
                ( { model | pageData = updatedPageData }, Cmd.none )

        GetNavigationData response ->
            ( { model | navigationData = logUnsuccessfulRequest response }, Cmd.none )

        GetAdvancedSearchData response ->
            let
                updatedPageData =
                    { pageData | advancedSearch = response }
            in
                ( { model | pageData = updatedPageData }, Cmd.none )

        GetPageDetailsData response ->
            let
                updatedPageData =
                    { pageData | pageDetails = response }
            in
                ( { model | pageData = updatedPageData }, Cmd.none )

        CategoryPaginationMsg subMsg ->
            pageData.categoryDetails
                |> Paginate.update PageData.categoryConfig subMsg
                |> Tuple.mapSecond (Cmd.map CategoryPaginationMsg)
                |> (\( ps, cmd ) ->
                        ( ps, Cmd.batch [ cmd, updatePageFromPagination model.route ps ] )
                   )
                |> Tuple.mapFirst (\cd -> { pageData | categoryDetails = cd })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })

        SearchPaginationMsg subMsg ->
            Paginate.update PageData.searchConfig subMsg pageData.searchResults
                |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)
                |> (\( sr, cmd ) ->
                        ( sr, Cmd.batch [ cmd, updatePageFromPagination model.route sr ] )
                   )
                |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })


updatePageFromPagination : Route -> Paginated a b c -> Cmd msg
updatePageFromPagination route paginated =
    let
        ( maybePage, newRouteConstructor ) =
            case route of
                CategoryDetails slug pagination ->
                    ( Just pagination.page, \p -> CategoryDetails slug { pagination | page = p } )

                SearchResults data pagination ->
                    ( Just pagination.page, \p -> SearchResults data { pagination | page = p } )

                _ ->
                    ( Nothing, always route )

        newPage =
            Paginate.getPage paginated
    in
        case maybePage of
            Nothing ->
                Cmd.none

            Just page ->
                if page == newPage then
                    Cmd.none
                else
                    Navigation.newUrl << reverse <| newRouteConstructor newPage


clearSearchForm : ( Model, Cmd msg ) -> ( Model, Cmd msg )
clearSearchForm ( model, cmd ) =
    flip (,) cmd <|
        case model.route of
            AdvancedSearch ->
                { model | searchData = Search.initial }

            SearchResults _ _ ->
                model

            _ ->
                { model
                    | searchData = Search.initial
                    , advancedSearchData = Search.initial
                }


logUnsuccessfulRequest : WebData a -> WebData a
logUnsuccessfulRequest response =
    case response of
        RemoteData.Success _ ->
            response

        _ ->
            Debug.log "Unsuccessful Request Returned" response



-- VIEW
-- TODO: Refactor into modules


view : Model -> Html Msg
view { route, pageData, navigationData, searchData, advancedSearchData } =
    let
        middleContent =
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col order-md-2" ] pageContent
                    , SiteSidebar.view route
                    ]
                ]

        pageContent =
            case route of
                ProductDetails _ ->
                    withIntermediateText productDetailsView pageData.productDetails

                CategoryDetails _ pagination ->
                    if Paginate.isLoading pageData.categoryDetails then
                        [ text "Loading..." ]
                    else
                        categoryDetailsView pagination
                            pageData.categoryDetails

                AdvancedSearch ->
                    withIntermediateText
                        (AdvancedSearch.view NavigateTo AdvancedSearchMsg advancedSearchData)
                        pageData.advancedSearch

                SearchResults data pagination ->
                    if Paginate.isLoading pageData.searchResults then
                        [ text "Loading..." ]
                    else
                        searchResultsView data pagination pageData.searchResults

                PageDetails _ ->
                    withIntermediateText staticPageView pageData.pageDetails

        activeCategories =
            case route of
                CategoryDetails _ _ ->
                    Paginate.getResponseData pageData.categoryDetails
                        |> Maybe.map .predecessors
                        |> Maybe.withDefault []

                ProductDetails _ ->
                    RemoteData.toMaybe pageData.productDetails
                        |> Maybe.map .predecessors
                        |> Maybe.withDefault []

                _ ->
                    []
    in
        div []
            [ SiteHeader.view SearchMsg searchData
            , SiteNavigation.view navigationData activeCategories searchData
            , middleContent
            , SiteFooter.view
            ]


withIntermediateText : (a -> List (Html msg)) -> WebData a -> List (Html msg)
withIntermediateText view data =
    case data of
        RemoteData.Loading ->
            [ text "Loading..." ]

        RemoteData.Success d ->
            view d

        e ->
            [ text <| toString e ]


productDetailsView : PageData.ProductDetails -> List (Html Msg)
productDetailsView { product, variants, maybeSeedAttribute, categories } =
    let
        categoryBlocks =
            List.filter (not << String.isEmpty << .description) categories
                |> List.map
                    (\category ->
                        div [ class "product-category" ]
                            [ h3 [ class "mt-3" ]
                                [ a (routeLinkAttributes <| CategoryDetails category.slug Pagination.default)
                                    [ text category.name ]
                                ]
                            , div [ innerHtml category.description ] []
                            ]
                    )
    in
        [ h1 []
            [ text product.name
            , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
            ]
        , hr [] []
        , div [ class "product-details" ]
            [ div [ class "clearfix" ]
                [ div [ class "float-left col-sm-4 col-md-5 col-lg-4" ]
                    [ div
                        [ class "card" ]
                        [ div [ class "card-body text-center p-1" ]
                            [ img
                                [ src << Images.media <| "products/" ++ product.imageURL
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


categoryDetailsView :
    Pagination.Data
    -> Paginated ProductData { slug : String, sorting : Sorting.Option } PageData.CategoryDetails
    -> List (Html Msg)
categoryDetailsView pagination products =
    let
        { category, subCategories } =
            case Paginate.getResponseData products of
                Just r ->
                    r

                Nothing ->
                    { category = Category.initial, subCategories = [], predecessors = [] }

        subCategoryCards =
            if List.length subCategories > 0 then
                List.map subCategoryCard subCategories
                    |> div [ class "row" ]
            else
                text ""

        subCategoryCard category =
            div [ class "col-6 col-sm-4 col-md-3 mb-2" ]
                [ a (routeLinkAttributes <| CategoryDetails category.slug Pagination.default)
                    [ div [ class "h-100 text-center" ]
                        [ img
                            [ class "img-fluid mx-auto"
                            , src <| Images.media <| "categories/" ++ category.imageURL
                            ]
                            []
                        , div [ class "my-auto" ] [ text category.name ]
                        ]
                    ]
                ]
    in
        [ div [ class "d-flex align-items-center" ]
            [ img
                [ class "img-fluid"
                , src <| Images.media <| "categories/" ++ category.imageURL
                ]
                []
            , h1 [ class "mb-0 pl-2" ] [ text category.name ]
            ]
        , hr [ class "mt-2" ] []
        , div [ innerHtml category.description ] []
        , subCategoryCards
        ]
            ++ productsList (CategoryDetails category.slug) pagination products


searchResultsView : Search.Data -> Pagination.Data -> PageData.SearchResults -> List (Html Msg)
searchResultsView ({ query } as data) pagination products =
    let
        uniqueSearch =
            Search.uniqueSearch data

        header =
            case uniqueSearch of
                Nothing ->
                    "Search Results"

                Just searchType ->
                    case searchType of
                        AllProducts ->
                            "All Products"

                        AttributeSearch (SeedAttribute.Organic) ->
                            "Organic Products"

                        AttributeSearch (SeedAttribute.Heirloom) ->
                            "Heirloom Products"

                        AttributeSearch (SeedAttribute.Regional) ->
                            "South-Eastern Products"

                        AttributeSearch (SeedAttribute.Ecological) ->
                            "Ecologically Grown Products"

        searchDescription =
            if uniqueSearch == Nothing then
                p []
                    [ queryDescription
                    , filterDescriptions
                    ]
            else
                text ""

        queryDescription =
            if String.isEmpty query then
                text ""
            else
                span []
                    [ text "Found "
                    , b [] [ text <| toString (Paginate.getTotalItems products) ]
                    , text " results for “"
                    , b [] [ text query ]
                    , text "”."
                    ]

        filterDescriptions =
            case ( categoryDescription, attributeDescriptions ) of
                ( "", [] ) ->
                    text ""

                ( "", attrs ) ->
                    div []
                        [ text "Showing Products that are "
                        , span [] attrs
                        , text "."
                        ]

                ( cat, [] ) ->
                    div []
                        [ text "Showing Products in the "
                        , b [] [ text cat ]
                        , text " category."
                        ]

                ( cat, attrs ) ->
                    div []
                        [ text "Showing Products that are "
                        , span [] attrs
                        , text ", & in the "
                        , b [] [ text cat ]
                        , text " category."
                        ]

        categoryDescription =
            case Paginate.getResponseData products of
                Nothing ->
                    ""

                Just name ->
                    name

        attributeDescriptions =
            [ ( .isOrganic, "Organic" )
            , ( .isHeirloom, "Heirloom" )
            , ( .isRegional, "Suitable for the South-East" )
            , ( .isEcological, "Ecologically Grown" )
            ]
                |> List.filter (\( selector, _ ) -> selector data)
                |> List.map (\( _, name ) -> b [] [ text name ])
                |> List.intersperse (text ", ")
    in
        [ h1 [] [ text header ]
        , hr [] []
        , searchDescription
        ]
            ++ productsList (SearchResults data) pagination products


staticPageView : StaticPage -> List (Html Msg)
staticPageView { name, slug, content } =
    let
        header =
            if slug == "home" then
                text ""
            else
                h1 [] [ text name ]
    in
        [ header
        , div [ innerHtml content ] []
        ]


productsList :
    (Pagination.Data -> Route)
    -> Pagination.Data
    -> Paginated ProductData a c
    -> List (Html Msg)
productsList routeConstructor pagination products =
    let
        sortHtml =
            if productsCount > 1 then
                div [ class "d-flex mb-2 justify-content-between align-items-center" ]
                    [ sortingInput
                    , perPageLinks
                    ]
            else
                text ""

        productsCount =
            Paginate.getTotalItems products

        sortingInput : Html Msg
        sortingInput =
            div [ class "form-inline" ]
                [ label [ class "col-form-label font-weight-bold", for "product-sort-select" ]
                    [ text "Sort by:" ]
                , text " "
                , select
                    [ id "product-sort-select"
                    , class "form-control form-control-sm ml-2"
                    , onProductsSortSelect (NavigateTo << routeConstructor)
                    ]
                  <|
                    List.map
                        (\data ->
                            option
                                [ value <| Sorting.toQueryValue data
                                , selected (data == pagination.sorting)
                                ]
                                [ text <| Sorting.toDescription data ]
                        )
                        Sorting.all
                ]

        onProductsSortSelect : (Pagination.Data -> msg) -> Html.Attribute msg
        onProductsSortSelect msg =
            targetValue
                |> Decode.map (\str -> msg <| { pagination | sorting = Sorting.fromQueryValue str })
                |> on "change"

        perPageLinks =
            [ 10, 25, 50, 75, 100 ]
                |> List.map
                    (\c ->
                        if c == pagination.perPage then
                            span [ class "font-weight-bold" ]
                                [ text <| toString c ]
                        else
                            a (routeLinkAttributes <| routeConstructor { pagination | perPage = c })
                                [ text <| toString c ]
                    )
                |> List.intersperse (text " | ")
                |> (\ps -> span [] <| span [ class "font-weight-bold" ] [ text "Products per page: " ] :: ps)

        paginationHtml =
            div [ class "d-flex mb-2 justify-content-between align-items-center" ] [ pagingText, pager ]

        pagingText =
            if productsCount == 0 then
                text ""
            else
                span []
                    [ text "Displaying "
                    , b [] [ text <| pagingStart () ]
                    , text " to "
                    , b [] [ text <| pagingEnd () ]
                    , text " (of "
                    , b [] [ text <| toString productsCount ]
                    , text " products)"
                    ]

        pagingStart _ =
            toString <|
                (currentPage - 1)
                    * pagination.perPage
                    + 1

        pagingEnd _ =
            toString <|
                if (not << Paginate.hasNext) products || productsCount < pagination.perPage then
                    productsCount
                else
                    (currentPage * pagination.perPage)

        totalPages =
            Paginate.getTotalPages products

        currentPage =
            Paginate.getPage products

        pager =
            if totalPages <= 1 then
                text ""
            else
                node "nav"
                    [ attribute "aria-label" "Category Product Pages" ]
                    [ ul [ class "pagination pagination-sm mb-0" ] <|
                        previousLink ()
                            :: (renderSections ())
                            ++ [ nextLink () ]
                    ]

        renderSections _ =
            Paginate.bootstrapPager
                (\p -> routeLinkAttributes <| routeConstructor { pagination | page = p })
                2
                2
                products

        previousLink _ =
            let
                previousPage =
                    max 1 (pagination.page - 1)

                previousRoute =
                    routeConstructor { pagination | page = previousPage }
            in
                prevNextLink (not << Paginate.hasPrevious) previousRoute "« Prev"

        nextLink _ =
            let
                nextPage =
                    min totalPages (pagination.page + 1)

                nextRoute =
                    routeConstructor { pagination | page = nextPage }
            in
                prevNextLink (not << Paginate.hasNext) nextRoute "Next »"

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

        productRows =
            flip List.map (Paginate.getCurrent products) <|
                \( product, variants, maybeSeedAttribute ) ->
                    tr []
                        [ td [ class "category-product-image text-center align-middle" ]
                            [ a (routeLinkAttributes <| ProductDetails product.slug)
                                [ img
                                    [ src << Images.media <| "products/" ++ product.imageURL
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
                                , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
                                ]
                            , div [ innerHtml product.longDescription ] []
                            ]
                        , td [ class "text-center align-middle" ]
                            [ div []
                                [ div [ class "font-weight-bold" ] [ text "$999.99" ]
                                , div [] [ text "CART_INPUT" ]
                                , small [ class "text-muted" ]
                                    [ text <| "Item # " ++ product.baseSKU ]
                                ]
                            ]
                        ]
    in
        if productsCount /= 0 then
            [ sortHtml
            , paginationHtml
            , table [ class "category-products table table-striped table-sm mb-2" ]
                [ tbody [] <| productRows ]
            , paginationHtml
            , SeedAttribute.legend
            ]
        else
            []
