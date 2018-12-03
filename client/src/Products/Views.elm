module Products.Views exposing (details, list)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (attribute, id, class, src, for, value, selected, tabindex, type_)
import Html.Events exposing (on, targetValue, onInput, onSubmit)
import Json.Decode as Decode
import Markdown
import Paginate exposing (Paginated)
import Messages exposing (Msg(..))
import Model exposing (CartForms)
import Models.Fields exposing (Cents(..), Milligrams(..), milligramsToString)
import PageData exposing (ProductData)
import Product exposing (ProductId(..), Product, ProductVariantId(..), ProductVariant)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(..))
import SeedAttribute
import Views.Format as Format
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes, onIntInput, htmlOrBlank)


details : CartForms -> PageData.ProductDetails -> List (Html Msg)
details addToCartForms { product, variants, maybeSeedAttribute, categories } =
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
                            , Markdown.toHtml [] category.description
                            ]
                    )
    in
        [ h1 [ class "product-details-title" ]
            [ Markdown.toHtml [] product.name
            , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
            ]
        , hr [] []
        , div [ class "product-details" ]
            [ div [ class "clearfix" ]
                [ div [ class "float-left w-25 mr-3 mb-2" ]
                    [ div
                        [ class "card" ]
                        [ div [ class "card-body text-center p-1" ]
                            [ img
                                [ src << Images.media <| "products/" ++ product.imageURL
                                , class "img-fluid mb-2"
                                ]
                                []
                            , cartForm addToCartForms product variants
                            ]
                        ]
                    ]
                , Markdown.toHtml [] product.longDescription
                , div [] categoryBlocks
                ]
            ]
        ]


list : (Pagination.Data -> Route) -> Pagination.Data -> CartForms -> Paginated ProductData a c -> List (Html Msg)
list routeConstructor pagination addToCartForms products =
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
                                [ text <| String.fromInt c ]
                        else
                            a (routeLinkAttributes <| routeConstructor { pagination | perPage = c })
                                [ text <| String.fromInt c ]
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
                    , b [] [ text <| String.fromInt productsCount ]
                    , text " products)"
                    ]

        pagingStart _ =
            String.fromInt <|
                (currentPage - 1)
                    * pagination.perPage
                    + 1

        pagingEnd _ =
            String.fromInt <|
                if (not << Paginate.hasNext) products || productsCount < pagination.perPage then
                    productsCount
                else
                    currentPage * pagination.perPage

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
                            :: renderSections ()
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
            List.map renderProduct (Paginate.getCurrent products)

        renderProduct ( product, variants, maybeSeedAttribute ) =
            tr []
                [ td [ class "row-product-image text-center align-middle" ]
                    [ a (routeLinkAttributes <| ProductDetails product.slug)
                        [ img
                            [ src << Images.media <| "products/" ++ product.imageURL ]
                            []
                        ]
                    ]
                , td [ class "row-product-description" ]
                    [ h3 [ class "mb-0" ]
                        [ a (routeLinkAttributes <| ProductDetails product.slug)
                            [ Markdown.toHtml [] product.name ]
                        , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
                        ]
                    , Markdown.toHtml [] product.longDescription
                    ]
                , td [ class "text-center align-middle" ]
                    [ cartForm addToCartForms product variants ]
                ]
    in
        if productsCount /= 0 then
            [ sortHtml
            , paginationHtml
            , table [ class "products-table table table-striped table-sm mb-2" ]
                [ tbody [] <| productRows ]
            , paginationHtml
            , SeedAttribute.legend
            ]
        else
            []


cartForm : CartForms -> Product -> Dict Int ProductVariant -> Html Msg
cartForm addToCartForms product variants =
    let
        formAttributes =
            (::) (class "add-to-cart-form") <|
                case maybeSelectedVariantId of
                    Just variantId ->
                        [ onSubmit <| SubmitAddToCart product.id variantId ]

                    Nothing ->
                        []

        selectedPrice =
            maybeSelectedVariant
                |> htmlOrBlank (\v -> h4 [] [ text <| Format.cents v.price ])

        hasMultipleVariants =
            Dict.size variants > 1

        variantSelect _ =
            select
                [ id <| "inputVariant-" ++ String.fromInt (fromProductId product.id)
                , class "variant-select form-control mb-1 mx-auto"
                , onSelectInt <| ChangeCartFormVariantId product.id
                ]
                (List.map variantOption <| List.sortBy .skuSuffix <| Dict.values variants)

        addToCartInput =
            div [ class "input-group mx-auto justify-content-center add-to-cart-group mb-2" ]
                [ input
                    [ type_ "number"
                    , class "form-control"
                    , A.min "1"
                    , A.step "1"
                    , value <| String.fromInt quantity
                    , onIntInput <| ChangeCartFormQuantity product.id
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ button [ class "btn btn-primary", type_ "submit" ]
                        [ text "Add" ]
                    ]
                ]

        selectedItemNumber =
            case maybeSelectedVariant of
                Nothing ->
                    product.baseSKU

                Just v ->
                    product.baseSKU ++ v.skuSuffix

        maybeSelectedVariant =
            maybeSelectedVariantId
                |> Maybe.andThen (\id -> Dict.get (fromVariantId id) variants)

        ( maybeSelectedVariantId, quantity ) =
            Dict.get (fromProductId product.id) addToCartForms
                |> Maybe.withDefault { variant = Nothing, quantity = 1 }
                |> (\v -> ( v.variant |> ifNothing maybeFirstVariantId, v.quantity ))

        ifNothing valIfNothing maybe =
            case maybe of
                Just x ->
                    maybe

                Nothing ->
                    valIfNothing

        maybeFirstVariantId =
            Dict.values variants
                |> List.sortBy .skuSuffix
                |> List.head
                |> Maybe.map .id

        onSelectInt msg =
            targetValue
                |> Decode.andThen
                    (String.toInt
                        >> Maybe.map (ProductVariantId >> Decode.succeed)
                        >> Maybe.withDefault (Decode.fail "")
                    )
                |> Decode.map msg
                |> on "change"

        variantOption variant =
            [ milligramsToString variant.weight ++ "g"
            , Format.cents variant.price
            ]
                |> String.join " - "
                |> text
                |> List.singleton
                |> option
                    [ value <| String.fromInt <| fromVariantId variant.id
                    , selected (Just variant == maybeSelectedVariant)
                    ]

        htmlWhen test renderer =
            if test then
                renderer ()
            else
                text ""

        fromVariantId (ProductVariantId i) =
            i

        fromProductId (ProductId i) =
            i
    in
        form formAttributes
            [ selectedPrice
            , htmlWhen hasMultipleVariants variantSelect
            , addToCartInput
            , small [ class "text-muted d-block" ]
                [ text <| "Item #" ++ selectedItemNumber ]
            ]
