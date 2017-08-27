module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, text, div, h1, h3, h4, hr, node, br, a, img, span, button, ul, li, small, table, tbody, tr, td, b)
import Html.Attributes exposing (attribute, id, class, href, src, type_, target, tabindex)
import Html.Attributes.Extra exposing (innerHtml)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode
import Navigation
import Paginate exposing (PaginatedList)
import RemoteData exposing (WebData)
import UrlParser as Url exposing ((</>), (<?>))


main : Program Never Model Msg
main =
    Navigation.program (parseRoute >> UrlUpdate)
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- ROUTING


type alias PaginationData =
    { page : Int
    , perPage : Int
    }


paginationToQueryString : PaginationData -> String
paginationToQueryString { page, perPage } =
    [ ( .page, page, "page" )
    , ( .perPage, perPage, "perPage" )
    ]
        |> List.map
            (\( selector, value, param ) ->
                ( selector defaultPagination /= value
                , param ++ "=" ++ toString value
                )
            )
        |> List.filter Tuple.first
        |> List.map Tuple.second
        |> String.join "&"
        |> (\s ->
                if String.isEmpty s then
                    ""
                else
                    "?" ++ s
           )


defaultPagination : PaginationData
defaultPagination =
    PaginationData 1 25


type Route
    = ProductDetails String
    | CategoryDetails String PaginationData


parseRoute : Navigation.Location -> Route
parseRoute =
    let
        optionalIntParam param default =
            Url.customParam param
                (Maybe.andThen (String.toInt >> Result.toMaybe)
                    >> Maybe.withDefault default
                )

        parsePaginationParams pathParser =
            Url.map (\constructor page -> constructor << PaginationData page)
                (pathParser
                    <?> optionalIntParam "page" (defaultPagination.page)
                    <?> optionalIntParam "perPage" (defaultPagination.perPage)
                )

        routeParser =
            Url.oneOf
                [ Url.map ProductDetails (Url.s "products" </> Url.string)
                , parsePaginationParams <| Url.map CategoryDetails (Url.s "categories" </> Url.string)
                ]
    in
        Url.parsePath routeParser
            >> Maybe.withDefault (ProductDetails "green-pod-red-seed-asparagus-yardlong-bean-7-g")


reverse : Route -> String
reverse route =
    case route of
        ProductDetails slug ->
            "/products/" ++ slug ++ "/"

        CategoryDetails slug pagination ->
            "/categories/" ++ slug ++ "/" ++ paginationToQueryString pagination


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


type Cents
    = Cents Int


type Milligrams
    = Milligrams Int


type CategoryId
    = CategoryId Int


type alias Category =
    { id : CategoryId
    , name : String
    , slug : String
    , parentId : Maybe CategoryId
    , description : String
    , imageURL : String
    , order : Int
    }


type ProductId
    = ProductId Int


type alias Product =
    { id : ProductId
    , name : String
    , slug : String
    , baseSKU : String
    , shortDescription : String
    , longDescription : String
    , imageURL : String
    }


type ProductVariantId
    = ProductVariantId Int


type alias ProductVariant =
    { id : ProductVariantId
    , product : ProductId
    , skuSuffix : String
    , price : Cents
    , quantity : Int
    , weight : Milligrams
    , isActive : Bool
    }


type SeedAttributeId
    = SeedAttributeId Int


type alias SeedAttribute =
    { id : SeedAttributeId
    , product : ProductId
    , isOrganic : Bool
    , isHeirloom : Bool
    , isEcological : Bool
    , isRegional : Bool
    }


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

                CategoryDetails slug _ ->
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
        (Decode.field "product" productDecoder)
        (Decode.field "variants" <| Decode.list productVariantDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable seedAttributeDecoder)
        (Decode.field "categories" <| Decode.list categoryDecoder)


categoryDetailsDecoder : Decode.Decoder CategoryDetailsData
categoryDetailsDecoder =
    let
        productDataDecoder =
            Decode.map (Paginate.fromList defaultPagination.perPage) <|
                Decode.list <|
                    Decode.map3 (,,)
                        (Decode.field "product" productDecoder)
                        (Decode.field "variants" <| Decode.list productVariantDecoder)
                        (Decode.field "seedAttribute" <| Decode.nullable seedAttributeDecoder)
    in
        Decode.map3 CategoryDetailsData
            (Decode.field "category" categoryDecoder)
            (Decode.field "subCategories" <| Decode.list categoryDecoder)
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
        (Decode.field "rootCategories" <| Decode.list categoryDecoder)
        (Decode.field "childrenCategories" <|
            Decode.map stringToIntKeys <|
                Decode.dict <|
                    Decode.list categoryDecoder
        )


categoryDecoder : Decode.Decoder Category
categoryDecoder =
    Decode.map7 Category
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "parentId" << Decode.nullable <| Decode.map CategoryId Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "imageUrl" Decode.string)
        (Decode.field "order" Decode.int)


productDecoder : Decode.Decoder Product
productDecoder =
    Decode.map7 Product
        (Decode.field "id" <| Decode.map ProductId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "baseSku" Decode.string)
        (Decode.field "shortDescription" Decode.string)
        (Decode.field "longDescription" Decode.string)
        (Decode.field "imageUrl" Decode.string)


productVariantDecoder : Decode.Decoder ProductVariant
productVariantDecoder =
    Decode.map7 ProductVariant
        (Decode.field "id" <| Decode.map ProductVariantId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "skuSuffix" Decode.string)
        (Decode.field "price" <| Decode.map Cents Decode.int)
        (Decode.field "quantity" Decode.int)
        (Decode.field "weight" <| Decode.map Milligrams Decode.int)
        (Decode.field "isActive" Decode.bool)


seedAttributeDecoder : Decode.Decoder SeedAttribute
seedAttributeDecoder =
    Decode.map6 SeedAttribute
        (Decode.field "id" <| Decode.map SeedAttributeId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "isOrganic" Decode.bool)
        (Decode.field "isHeirloom" Decode.bool)
        (Decode.field "isEcological" Decode.bool)
        (Decode.field "isRegional" Decode.bool)



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

        updateCategoryProductsPagination pagination ({ products } as data) =
            { data | products = updatePagination pagination products }

        updatePagination { page, perPage } =
            Paginate.changeItemsPerPage perPage
                >> Paginate.goTo page
    in
        case ( newRoute, model.route ) of
            ( CategoryDetails newSlug newPagination, CategoryDetails oldSlug oldPagination ) ->
                let
                    categoryDetails =
                        RemoteData.map (updateCategoryProductsPagination newPagination) routeData.categoryDetails

                    updatedRouteData () =
                        { routeData | categoryDetails = categoryDetails }
                in
                    if newSlug /= oldSlug then
                        fetchDataForRoute modelWithNewRoute
                    else
                        ( { modelWithNewRoute | routeData = updatedRouteData () }, Cmd.none )

            _ ->
                fetchDataForRoute modelWithNewRoute


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
                                    ++ (routeLinkAttributes <| CategoryDetails category.slug defaultPagination)
                                )
                                [ text category.name ]
                            ]

                    Just children ->
                        li [ class "nav-item dropdown" ]
                            [ a
                                [ class "nav-link dropdown-toggle"
                                , href <| reverse <| CategoryDetails category.slug defaultPagination
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
                    ++ (routeLinkAttributes <| CategoryDetails category.slug defaultPagination)
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

                CategoryDetails _ pagination ->
                    withIntermediateText (categoryDetailsView pagination) routeData.categoryDetails
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
        [ ( isOrganic, "icons/organic-certified.png" )
        , ( isHeirloom, "icons/heirloom.png" )
        , ( isRegional, "icons/southeast.png" )
        , ( isEcological, "icons/ecologically-grown.png" )
        ]
        |> List.map
            (Tuple.second
                >> (\url ->
                        img [ class "my-auto", src <| staticImage url ] []
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
                                [ a (routeLinkAttributes <| CategoryDetails category.slug defaultPagination)
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


categoryDetailsView : PaginationData -> CategoryDetailsData -> List (Html Msg)
categoryDetailsView pagination { category, subCategories, products } =
    let
        subCategoryCards =
            if List.length subCategories > 0 then
                List.map subCategoryCard subCategories
                    |> div [ class "row" ]
            else
                text ""

        subCategoryCard category =
            div [ class "col-6 col-sm-4 col-md-3 mb-2" ]
                [ a (routeLinkAttributes <| CategoryDetails category.slug defaultPagination)
                    [ div [ class "h-100 text-center" ]
                        [ img [ class "img-fluid mx-auto", src <| mediaImage category.imageURL ] []
                        , div [ class "my-auto" ] [ text category.name ]
                        ]
                    ]
                ]

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
                    CategoryDetails category.slug { pagination | page = previousPage }
            in
                prevNextLink Paginate.isFirst previousRoute "« Prev"

        nextLink _ =
            let
                nextPage =
                    min (Paginate.totalPages products) (pagination.page + 1)

                nextRoute =
                    CategoryDetails category.slug { pagination | page = nextPage }
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
        , paginationHtml
        , table [ class "category-products table table-striped table-sm mb-2" ]
            [ tbody [] <| productRows ]
        , paginationHtml
        ]
