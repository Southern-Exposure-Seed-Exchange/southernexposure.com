module Components.SiteUI.Breadcrumbs exposing (view)

import Components.Aria as Aria
import Components.Microdata as Microdata
import Components.Pagination as Pagination
import Data.Msg exposing (Msg(..))
import Data.PageData exposing (PageData)
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Html exposing (Html, a, div, li, node, ol, span, text)
import Html.Attributes exposing (class, id)
import Paginate
import RemoteData
import Utils.View exposing (routeLinkAttributes)


view : Route -> PageData -> Html Msg
view route pageData =
    let
        items =
            List.indexedMap (\i f -> f <| i + 1) <|
                inactiveItem "Home" Routing.homePage
                    :: childItems

        activeItem content position =
            li (class "breadcrumb-item tw:shrink-0 tw:whitespace-nowrap active" :: Microdata.itemListElement)
                [ span [ Microdata.name ] [ text content ]
                , Microdata.positionMeta position
                ]

        inactiveItem content itemRoute position =
            li (class "breadcrumb-item tw:shrink-0 tw:whitespace-nowrap" :: Microdata.itemListElement)
                [ a (Microdata.item :: routeLinkAttributes itemRoute)
                    [ span [ Microdata.name ] [ text content ]
                    ]
                , Microdata.positionMeta position
                ]

        childItems =
            case route of
                PageDetails "home" _ ->
                    []

                PageDetails _ _ ->
                    pageData.pageDetails
                        |> RemoteData.toMaybe
                        |> maybeToList (.name >> activeItem >> List.singleton)

                CategoryDetails _ _ ->
                    Paginate.getResponseData pageData.categoryDetails
                        |> maybeToList categoryDetailsToBreadcrumbs

                ProductDetails _ _ ->
                    pageData.productDetails
                        |> RemoteData.toMaybe
                        |> maybeToList productDetailsToBreadcrumbs

                AdvancedSearch ->
                    singleItem "Advanced Search"

                SearchResults _ _ ->
                    singleItem "Search Results"

                CreateAccount ->
                    singleItem "Create an Account"

                VerifyEmail _ ->
                    singleItem "Verify an email"

                VerificationRequired _ ->
                    singleItem "Email verification required"

                CreateAccountSuccess ->
                    [ inactiveItem "Create An Account" CreateAccount
                    , activeItem "Success"
                    ]

                Login _ _ ->
                    singleItem "Login"

                ResetPassword Nothing ->
                    [ inactiveItem "Login" <| Login Nothing False
                    , activeItem "Reset Password"
                    ]

                ResetPassword (Just _) ->
                    [ inactiveItem "Login" <| Login Nothing False
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

                OrderDetails orderId _ ->
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

                Admin Dashboard ->
                    singleItem "Dashboard"

                Admin CategoryList ->
                    singleItem "Categories"

                Admin CategoryNew ->
                    [ inactiveItem "Categories" (Admin CategoryList)
                    , activeItem "New Category"
                    ]

                Admin (CategoryEdit _) ->
                    [ inactiveItem "Categories" (Admin CategoryList)
                    , activeItem "Edit Category"
                    ]

                Admin PageList ->
                    singleItem "Pages"

                Admin PageNew ->
                    [ inactiveItem "Pages" (Admin PageList)
                    , activeItem "New Page"
                    ]

                Admin (PageEdit _) ->
                    [ inactiveItem "Pages" (Admin PageList)
                    , activeItem "Edit Page"
                    ]

                Admin (OrderList _) ->
                    singleItem "Orders"

                Admin (AdminOrderDetails orderId) ->
                    [ inactiveItem "Orders"
                        (Admin <| OrderList { page = 1, perPage = 50, query = "" })
                    , activeItem <| "Order #" ++ String.fromInt orderId
                    ]

                Admin (CustomerList _) ->
                    singleItem "Customers"

                Admin (CustomerEdit _) ->
                    [ inactiveItem "Customers"
                        (Admin <| CustomerList { page = 1, perPage = 50, query = "" })
                    , activeItem "Edit Customer"
                    ]

                Admin ProductList ->
                    singleItem "Products"

                Admin ProductNew ->
                    [ inactiveItem "Products" <| Admin ProductList
                    , activeItem "New"
                    ]

                Admin (ProductEdit _) ->
                    [ inactiveItem "Products" <| Admin ProductList
                    , activeItem "Edit Product"
                    ]

                Admin CouponList ->
                    singleItem "Coupons"

                Admin CouponNew ->
                    [ inactiveItem "Coupons" <| Admin CouponList
                    , activeItem "New Coupon"
                    ]

                Admin (CouponEdit _) ->
                    [ inactiveItem "Coupons" <| Admin CouponList
                    , activeItem "Edit Coupon"
                    ]

                Admin Surcharges ->
                    singleItem "Surcharges"

                Admin ShippingMethods ->
                    singleItem "Shipping Methods"

                Admin Settings ->
                    singleItem "Settings"

                Admin ProductSaleList ->
                    singleItem "Product Sales"

                Admin ProductSaleNew ->
                    [ inactiveItem "Product Sales" <| Admin ProductSaleList
                    , activeItem "New Sale"
                    ]

                Admin (ProductSaleEdit _) ->
                    [ inactiveItem "Product Sales" <| Admin ProductSaleList
                    , activeItem "Edit Sale"
                    ]

                Admin CategorySaleList ->
                    singleItem "Category Sales"

                Admin CategorySaleNew ->
                    [ inactiveItem "Category Sales" <| Admin CategorySaleList
                    , activeItem "New Sale"
                    ]

                Admin (CategorySaleEdit _) ->
                    [ inactiveItem "Category Sales" <| Admin CategorySaleList
                    , activeItem "Edit Sale"
                    ]

                Redirect _ ->
                    singleItem "Redirecting..."

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
    if List.isEmpty childItems then
        node "nav" [ class "tw:pb-[28px]" ] []

    else
        node "nav"
            [ id "breadcrumbs", class "se-container d-print-none", Aria.label "breadcrumb" ]
            [ div [ class "tw:py-[20px] tw:px-0 tw:lg:px-[16px]" ]
                [ ol (class "breadcrumb mb-0 tw:flex tw:flex-nowrap! tw:overflow-y-auto no-scrollbar" :: Microdata.breadcrumbList) items
                ]
            ]
