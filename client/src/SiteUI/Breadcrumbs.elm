module SiteUI.Breadcrumbs exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Paginate
import RemoteData
import Messages exposing (Msg(NavigateTo))
import PageData exposing (PageData)
import Products.Pagination as Pagination
import Routing exposing (Route(..))
import Views.Utils exposing (routeLinkAttributes)


view : Route -> PageData -> Html Msg
view route pageData =
    let
        items =
            if List.isEmpty childItems then
                [ activeItem "Home" ]
            else
                inactiveItem "Home" (PageDetails "home") :: childItems

        activeItem content =
            li [ class "breadcrumb-item active" ] [ text content ]

        inactiveItem content route =
            li [ class "breadcrumb-item" ]
                [ a (routeLinkAttributes route) [ text content ]
                ]

        childItems =
            case route of
                PageDetails "home" ->
                    []

                PageDetails _ ->
                    pageData.pageDetails
                        |> RemoteData.toMaybe
                        |> maybeToList (.name >> activeItem >> List.singleton)

                CategoryDetails _ _ ->
                    Paginate.getResponseData pageData.categoryDetails
                        |> maybeToList categoryDetailsToBreadcrumbs

                ProductDetails _ ->
                    pageData.productDetails
                        |> RemoteData.toMaybe
                        |> maybeToList productDetailsToBreadcrumbs

                AdvancedSearch ->
                    singleItem "Advanced Search"

                SearchResults _ _ ->
                    singleItem "Search Results"

                NotFound ->
                    singleItem "Page Not Found"

        categoryDetailsToBreadcrumbs { category, predecessors } =
            List.map predecessorCategoryToInactiveItem
                (predecessors
                    |> List.reverse
                    |> List.tail
                    |> Maybe.withDefault []
                    |> List.reverse
                )
                ++ [ activeItem category.name ]

        productDetailsToBreadcrumbs { product, predecessors } =
            List.map predecessorCategoryToInactiveItem predecessors
                ++ [ activeItem product.name ]

        singleItem content =
            [ activeItem content ]

        predecessorCategoryToInactiveItem { name, slug } =
            inactiveItem name (CategoryDetails slug Pagination.default)

        maybeToList f =
            Maybe.map f >> Maybe.withDefault []
    in
        div [ class "container pb-3" ] [ ol [ class "breadcrumb mb-0" ] items ]
