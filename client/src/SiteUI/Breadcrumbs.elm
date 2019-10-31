module SiteUI.Breadcrumbs exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Messages exposing (Msg(..))
import PageData exposing (PageData)
import Paginate
import Products.Pagination as Pagination
import RemoteData
import Routing exposing (AdminRoute(..), Route(..))
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

        inactiveItem content itemRoute =
            li [ class "breadcrumb-item" ]
                [ a (routeLinkAttributes itemRoute) [ text content ]
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

                CreateAccount ->
                    singleItem "Create an Account"

                CreateAccountSuccess ->
                    [ inactiveItem "Create An Account" CreateAccount
                    , activeItem "Success"
                    ]

                Login _ ->
                    singleItem "Login"

                ResetPassword Nothing ->
                    [ inactiveItem "Login" <| Login Nothing
                    , activeItem "Reset Password"
                    ]

                ResetPassword (Just _) ->
                    [ inactiveItem "Login" <| Login Nothing
                    , activeItem "Change Password"
                    ]

                MyAccount ->
                    singleItem "My Account"

                EditLogin ->
                    [ inactiveItem "My Account" MyAccount
                    , activeItem "Edit Login Details"
                    ]

                EditAddress ->
                    [ inactiveItem "My Account" MyAccount
                    , activeItem "Edit Addresses"
                    ]

                OrderDetails orderId ->
                    [ inactiveItem "My Account" MyAccount
                    , activeItem <| "Order #" ++ String.fromInt orderId
                    ]

                Cart ->
                    singleItem "Shopping Cart"

                QuickOrder ->
                    singleItem "Quick Order"

                Checkout ->
                    singleItem "Checkout"

                CheckoutSuccess _ _ ->
                    singleItem "Order Complete"

                Admin CategoryList ->
                    singleItem "Categories"

                Admin CategoryNew ->
                    [ inactiveItem "Categories" (Admin CategoryList)
                    , activeItem "New Category"
                    ]

                NotFound ->
                    singleItem "Page Not Found"

        categoryDetailsToBreadcrumbs { category, predecessors } =
            List.map predecessorCategoryToInactiveItem predecessors
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
    div [ id "breadcrumbs", class "container" ]
        [ ol [ class "breadcrumb mb-0" ] items ]
