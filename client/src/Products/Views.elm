module Products.Views exposing (details, list)

import Html exposing (..)
import Html.Attributes exposing (attribute, id, class, src, for, value, selected, tabindex)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode
import Markdown
import Paginate exposing (Paginated)
import Messages exposing (Msg(..))
import PageData exposing (ProductData)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(..))
import SeedAttribute
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes, htmlOrBlank)


details : PageData.ProductDetails -> List (Html Msg)
details { product, variants, maybeSeedAttribute, categories } =
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
                            , div [] [ Markdown.toHtml [] category.description ]
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
                    [ div [] [ Markdown.toHtml [] product.longDescription ] ]
                , div [ class "col-12" ] categoryBlocks
                ]
            ]
        ]


list : (Pagination.Data -> Route) -> Pagination.Data -> Paginated ProductData a c -> List (Html Msg)
list routeConstructor pagination products =
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
                |> Decode.map
                    (\str ->
                        msg <|
                            { pagination | sorting = Sorting.fromQueryValue str }
                    )
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
                |> (\ps ->
                        span [ class "font-weight-bold" ] [ text "Products per page: " ]
                            :: ps
                            |> span []
                   )

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
                                [ a (routeLinkAttributes <| ProductDetails product.slug)
                                    [ Markdown.toHtml [] product.name ]
                                , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
                                ]
                            , div [] [ Markdown.toHtml [] product.longDescription ]
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
