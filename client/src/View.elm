module View exposing (pageDescription, pageImage, pageTitle, view)

import AdvancedSearch
import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.MyAccount as MyAccount
import Auth.ResetPassword as ResetPassword
import Browser exposing (Document)
import Cart
import Categories.AdminViews as CategoryAdminViews
import Categories.Views as CategoryViews
import Checkout
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Messages exposing (Msg(..))
import Model exposing (CartForms, Model)
import OrderDetails
import PageData
import Paginate
import Products.AdminViews as ProductAdmin
import Products.Pagination as Pagination
import Products.Views as ProductViews
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..), isAdminRoute)
import Search exposing (UniqueSearch(..))
import SeedAttribute
import SiteUI.Breadcrumbs as SiteBreadcrumbs
import SiteUI.Footer as SiteFooter
import SiteUI.Header as SiteHeader
import SiteUI.Navigation as SiteNavigation
import SiteUI.Sidebar as SiteSidebar
import StaticPage exposing (StaticPage)
import User
import Views.CouponAdmin as CouponAdmin
import Views.CustomerAdmin as CustomerAdmin
import Views.OrderAdmin as OrderAdmin
import Views.StaticPageAdmin as StaticPageAdmin
import Views.Utils exposing (icon, rawHtml)


view : Model -> Document Msg
view ({ route, pageData, navigationData, zone } as model) =
    let
        middleContent =
            case route of
                Checkout ->
                    div [ class "container" ]
                        [ div [ class "row justify-content-center" ]
                            [ div [ class "col-md-10" ] pageContent
                            ]
                        ]

                Admin adminRoute ->
                    if not <| User.isAdmin model.currentUser then
                        withSidebar accessDeniedView

                    else
                        div [ class "admin container-fluid" ]
                            [ div [ class "row" ]
                                [ div [ class "col" ] <|
                                    renderAdminTitle adminRoute
                                        :: pageContent
                                ]
                            ]

                _ ->
                    withSidebar pageContent

        withSidebar content =
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col order-md-2" ] content
                    , SiteSidebar.view route
                    ]
                ]

        renderAdminTitle adminRoute =
            case adminRoute of
                AdminOrderDetails _ ->
                    text ""

                _ ->
                    h2 [] [ text <| adminTitle model adminRoute ]

        pageContent =
            case route of
                ProductDetails _ ->
                    withIntermediateText
                        (ProductViews.details model.addToCartForms)
                        pageData.productDetails

                CategoryDetails _ pagination ->
                    if Paginate.getResponseData pageData.categoryDetails == Nothing then
                        [ loadingInterstitial True ]

                    else if Paginate.getError pageData.categoryDetails /= Nothing then
                        notFoundView

                    else
                        loadingInterstitial False
                            :: CategoryViews.details pagination
                                model.addToCartForms
                                pageData.categoryDetails

                AdvancedSearch ->
                    withIntermediateText
                        (AdvancedSearch.view NavigateTo AdvancedSearchMsg model.advancedSearchData)
                        pageData.advancedSearch

                SearchResults data pagination ->
                    if Paginate.isLoading pageData.searchResults then
                        [ loadingInterstitial True ]

                    else
                        loadingInterstitial False
                            :: searchResultsView data pagination model.addToCartForms pageData.searchResults

                PageDetails _ _ ->
                    withIntermediateText staticPageView pageData.pageDetails

                CreateAccount ->
                    CreateAccount.view CreateAccountMsg model.createAccountForm

                CreateAccountSuccess ->
                    CreateAccount.successView

                Login redirectTo ->
                    Login.view LoginMsg model.loginForm redirectTo

                ResetPassword maybeCode ->
                    ResetPassword.view ResetPasswordMsg model.resetPasswordForm maybeCode

                MyAccount ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.myAccount
                        |> withIntermediateText (apply <| MyAccount.view zone)

                EditLogin ->
                    EditLogin.view EditLoginMsg model.editLoginForm model.currentUser

                EditAddress ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.addressDetails
                        |> withIntermediateText (apply <| EditAddress.view model.editAddressForm)
                        |> List.map (Html.map EditAddressMsg)

                OrderDetails orderId ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                        |> withIntermediateText (apply <| OrderDetails.view zone orderId)

                Cart ->
                    withIntermediateText (Cart.view model.editCartForm) pageData.cartDetails
                        |> List.map (Html.map EditCartMsg)

                QuickOrder ->
                    QuickOrder.view model.quickOrderForms
                        |> List.map (Html.map QuickOrderMsg)

                Checkout ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.checkoutDetails
                        |> withIntermediateText (apply <| Checkout.view model.checkoutForm model.currentUser)
                        |> List.map (Html.map CheckoutMsg)

                CheckoutSuccess orderId newAccount ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                        |> withIntermediateText (apply <| Checkout.successView zone LogOut orderId newAccount)

                Admin Dashboard ->
                    adminDashboardView

                Admin CategoryList ->
                    withIntermediateText CategoryAdminViews.list pageData.adminCategoryList

                Admin CategoryNew ->
                    withIntermediateText (CategoryAdminViews.new model.newCategoryForm) pageData.adminNewCategory
                        |> List.map (Html.map NewCategoryMsg)

                Admin (CategoryEdit cId) ->
                    RemoteData.map2 Tuple.pair pageData.adminNewCategory pageData.adminEditCategory
                        |> withIntermediateText (apply <| CategoryAdminViews.edit cId model.editCategoryForm)
                        |> List.map (Html.map EditCategoryMsg)

                Admin PageList ->
                    withIntermediateText StaticPageAdmin.list pageData.adminPageList

                Admin PageNew ->
                    StaticPageAdmin.new model.newPageForm
                        |> List.map (Html.map NewPageMsg)

                Admin (PageEdit _) ->
                    withIntermediateText (StaticPageAdmin.edit model.editPageForm)
                        pageData.adminEditPage
                        |> List.map (Html.map EditPageMsg)

                Admin (OrderList { query }) ->
                    withIntermediateText (\locs -> OrderAdmin.list zone locs query model.orderSearchForm pageData.adminOrderList)
                        pageData.locations
                        |> List.map (Html.map OrderSearchMsg)

                Admin (AdminOrderDetails orderId) ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.adminOrderDetails
                        |> withIntermediateText (apply <| OrderAdmin.details zone orderId model.orderDetailsForm)
                        |> List.map (Html.map OrderDetailsMsg)

                NotFound ->
                    notFoundView

                Redirect path ->
                    redirectView path

                Admin (CustomerList { query }) ->
                    CustomerAdmin.list query model.customerSearchForm pageData.adminCustomerList
                        |> List.map (Html.map CustomerSearchMsg)

                Admin (CustomerEdit _) ->
                    withIntermediateText (CustomerAdmin.edit model.editCustomerForm) pageData.adminEditCustomer
                        |> List.map (Html.map EditCustomerMsg)

                Admin ProductList ->
                    withIntermediateText (ProductAdmin.list model.productListForm) pageData.adminProductList
                        |> List.map (Html.map ProductListMsg)

                Admin ProductNew ->
                    withIntermediateText (ProductAdmin.new model.newProductForm) pageData.adminSharedProduct
                        |> List.map (Html.map NewProductMsg)

                Admin (ProductEdit _) ->
                    withIntermediateText (ProductAdmin.editForm model.editProductForm) pageData.adminSharedProduct
                        |> List.map (Html.map EditProductMsg)

                Admin CouponList ->
                    withIntermediateText (CouponAdmin.list zone) pageData.adminCouponList

                Admin CouponNew ->
                    CouponAdmin.new model.newCouponForm
                        |> List.map (Html.map NewCouponMsg)

                Admin (CouponEdit _) ->
                    withIntermediateText (CouponAdmin.edit model.editCouponForm) pageData.adminEditCoupon
                        |> List.map (Html.map EditCouponMsg)

        apply : (a -> b -> c) -> ( a, b ) -> c
        apply f ( a, b ) =
            f a b

        activeCategoryIds =
            case route of
                CategoryDetails _ _ ->
                    Paginate.getResponseData pageData.categoryDetails
                        |> Maybe.map
                            (\cd ->
                                cd.category.id :: List.map .id cd.predecessors
                            )
                        |> Maybe.withDefault []

                ProductDetails _ ->
                    RemoteData.toMaybe pageData.productDetails
                        |> Maybe.map (.predecessors >> List.map .id)
                        |> Maybe.withDefault []

                _ ->
                    []
    in
    Document (pageTitle model) <|
        if isAdminRoute route && User.isAdmin model.currentUser then
            [ SiteHeader.adminView
            , SiteNavigation.adminView route
            , middleContent
            ]

        else
            [ SiteHeader.view SearchMsg model.searchData model.currentUser model.cartItemCount
            , SiteNavigation.view route model.currentUser navigationData activeCategoryIds model.searchData
            , SiteBreadcrumbs.view route pageData
            , middleContent
            , SiteFooter.view
            ]


pageTitle : Model -> String
pageTitle ({ route, pageData } as model) =
    let
        getFromPageData :
            (PageData.PageData -> RemoteData.WebData a)
            -> (a -> String)
            -> String
        getFromPageData selector f =
            selector pageData
                |> RemoteData.toMaybe
                |> Maybe.map f
                |> Maybe.withDefault ""
    in
    case route of
        ProductDetails _ ->
            getFromPageData .productDetails (.product >> .name)

        CategoryDetails _ _ ->
            pageData.categoryDetails
                |> Paginate.getResponseData
                |> Maybe.map (.category >> .name)
                |> Maybe.withDefault ""

        AdvancedSearch ->
            "Advanced Search"

        SearchResults data _ ->
            case Search.uniqueSearch data of
                Nothing ->
                    "Search Results"

                Just searchType ->
                    case searchType of
                        AllProducts ->
                            "All Products"

                        AttributeSearch SeedAttribute.Organic ->
                            "Organic Products"

                        AttributeSearch SeedAttribute.Heirloom ->
                            "Heirloom Products"

                        AttributeSearch SeedAttribute.Regional ->
                            "South-Eastern Products"

                        AttributeSearch SeedAttribute.SmallGrower ->
                            "Products from Small Farms in our Grower Network"

        PageDetails _ _ ->
            getFromPageData .pageDetails .name

        CreateAccount ->
            "Create an Account"

        CreateAccountSuccess ->
            "Account Creation Successful"

        Login _ ->
            "Customer Login"

        ResetPassword Nothing ->
            "Reset Password"

        ResetPassword (Just _) ->
            "Change Password"

        MyAccount ->
            "My Account"

        EditLogin ->
            "Edit Login Details"

        EditAddress ->
            "Edit Addresses"

        OrderDetails orderId ->
            "Order #" ++ String.fromInt orderId

        Cart ->
            "Shopping Cart"

        QuickOrder ->
            "Quick Order"

        Checkout ->
            "Checkout"

        CheckoutSuccess _ _ ->
            "Order Complete"

        Admin adminRoute ->
            adminTitle model adminRoute ++ " - Admin"

        Redirect _ ->
            "Redirecting..."

        NotFound ->
            "Page Not Found"


adminTitle : Model -> AdminRoute -> String
adminTitle ({ pageData } as model) adminRoute =
    case adminRoute of
        Dashboard ->
            "Dashboard"

        CategoryList ->
            "Categories"

        CategoryNew ->
            "New Category"

        CategoryEdit _ ->
            pageData.adminEditCategory
                |> RemoteData.map (\c -> " - " ++ c.name)
                |> RemoteData.toMaybe
                |> Maybe.withDefault ""
                |> (\name -> "Edit Category" ++ name)

        PageList ->
            "Pages"

        PageNew ->
            "New Page"

        PageEdit _ ->
            pageData.adminEditPage
                |> RemoteData.map (\p -> " - " ++ p.title)
                |> RemoteData.toMaybe
                |> Maybe.withDefault ""
                |> (\title -> "Edit Page" ++ title)

        OrderList _ ->
            "Orders"

        AdminOrderDetails orderId ->
            "Order #" ++ String.fromInt orderId

        CustomerList _ ->
            "Customers"

        CustomerEdit _ ->
            pageData.adminEditCustomer
                |> RemoteData.map (\c -> " - " ++ c.email)
                |> RemoteData.toMaybe
                |> Maybe.withDefault ""
                |> (\email -> "Edit Customer" ++ email)

        ProductList ->
            "Products"

        ProductNew ->
            "New Product"

        ProductEdit _ ->
            model.editProductForm.productData
                |> RemoteData.map (\p -> " - " ++ p.name)
                |> RemoteData.toMaybe
                |> Maybe.withDefault ""
                |> (\name -> "Edit Product" ++ name)

        CouponList ->
            "Coupons"

        CouponNew ->
            "New Coupon"

        CouponEdit _ ->
            pageData.adminEditCoupon
                |> RemoteData.map (\c -> " - " ++ c.code)
                |> RemoteData.toMaybe
                |> Maybe.withDefault ""
                |> (\code -> "Edit Coupon" ++ code)


pageImage : Model -> Maybe String
pageImage { route, pageData } =
    case route of
        ProductDetails _ ->
            RemoteData.toMaybe pageData.productDetails
                |> Maybe.map (.product >> .image >> .original)

        CategoryDetails _ _ ->
            Paginate.getResponseData pageData.categoryDetails
                |> Maybe.map (.category >> .image >> .original)

        _ ->
            Nothing


{-| Get the meta description for a page.
-}
pageDescription : Model -> String
pageDescription { route, pageData } =
    case route of
        ProductDetails _ ->
            RemoteData.toMaybe pageData.productDetails
                |> Maybe.map (.product >> .longDescription)
                |> Maybe.withDefault ""

        CategoryDetails _ _ ->
            Paginate.getResponseData pageData.categoryDetails
                |> Maybe.map (.category >> .description)
                |> Maybe.withDefault ""

        PageDetails _ _ ->
            RemoteData.toMaybe pageData.pageDetails
                |> Maybe.map .content
                |> Maybe.withDefault ""

        NotFound ->
            "Sorry, this page no longer exists."

        Login _ ->
            "Log in to your Southern Exposure Seed Exchange account."

        CreateAccount ->
            "Create a free account to purchase products from our catalog."

        ResetPassword _ ->
            "Reset the password to your Southern Exposure Seed Exchange account."

        AdvancedSearch ->
            "Search our entire product catalog, filtering by types or categories."

        QuickOrder ->
            "Easily add multiple items to your shopping cart with our Quick Order form."

        Cart ->
            "View the contents of your shopping cart."

        Redirect path ->
            "This page has moved to: " ++ path

        SearchResults _ _ ->
            ""

        Checkout ->
            ""

        CheckoutSuccess _ _ ->
            ""

        CreateAccountSuccess ->
            ""

        MyAccount ->
            ""

        EditLogin ->
            ""

        EditAddress ->
            ""

        OrderDetails _ ->
            ""

        Admin _ ->
            ""


withIntermediateText : (a -> List (Html msg)) -> WebData a -> List (Html msg)
withIntermediateText subView data =
    case data of
        RemoteData.Loading ->
            [ loadingInterstitial True ]

        RemoteData.Success d ->
            loadingInterstitial False
                :: subView d

        RemoteData.Failure httpError ->
            remoteFailureView httpError

        RemoteData.NotAsked ->
            [ text "BUG! Reached NotAsked!" ]


loadingInterstitial : Bool -> Html msg
loadingInterstitial isVisible =
    let
        class_ =
            if isVisible then
                "loading-interstitial"

            else
                "loading-interstitial hidden"
    in
    div [ class class_ ]
        [ div [] [ icon "spinner fa-spin fa-5x" ]
        , div [ class "mt-4 font-weight-bold" ] [ text "Loading..." ]
        ]


remoteFailureView : Http.Error -> List (Html msg)
remoteFailureView error =
    case error of
        Http.BadStatus code ->
            if code == 404 then
                notFoundView

            else if code == 403 then
                accessDeniedView

            else if code >= 500 && code < 600 then
                serverErrorView code

            else
                unexpectedStatusCodeView code

        Http.NetworkError ->
            [ h1 [] [ text "Network Error" ]
            , p []
                [ text <|
                    "We are unable to connect to our server. "
                        ++ "Please verify your internet connection and try refreshing the page. "
                        ++ "If the problem continues, please contact us."
                ]
            ]

        Http.Timeout ->
            [ h1 [] [ text "Network Timeout" ]
            , p []
                [ text <|
                    "Our server took too long to respond. "
                        ++ "This may be due to temporary heavy-load, or a slow internet connection. "
                        ++ "Please refresh the page and try again."
                ]
            ]

        Http.BadUrl url ->
            [ h1 [] [ text "Invalid URL" ]
            , p []
                [ text <|
                    "We've used an invalid URL while talking to our server. "
                        ++ "If this problem continues, please contact us & include the following URL:"
                , pre [] [ text url ]
                ]
            ]

        Http.BadBody errorMsg ->
            [ h1 [] [ text "Unexpected Response" ]
            , p []
                [ text <|
                    "Our server responded with data that we were not expecting. "
                        ++ "If this problem continues, please contact us with the following information:"
                , pre [] [ text errorMsg ]
                ]
            ]


notFoundView : List (Html msg)
notFoundView =
    [ h1 [] [ text "Page Not Found" ]
    , p []
        [ text <|
            "Sorry, we couldn't find the page your were looking for. "
                ++ "If you got to this page from our site, please contact us so we can fix our links."
        ]
    ]


redirectView : String -> List (Html msg)
redirectView path =
    [ h1 [] [ text "Redirecting..." ]
    , p [] [ text <| "Please wait while we redirect you to " ++ path ++ "." ]
    ]


accessDeniedView : List (Html msg)
accessDeniedView =
    [ h1 [] [ text "Access Denied" ]
    , p [] [ text "Sorry, you do not have permission to view this page. If you think this is an error, please contact us." ]
    ]


serverErrorView : Int -> List (Html msg)
serverErrorView code =
    [ h1 [] [ text <| "Server Error - " ++ String.fromInt code ]
    , p []
        [ text <|
            "The server encountered an error while processing your request. "
                ++ "Please try refreshing the page or contact us with the error code if the error continues."
        ]
    ]


unexpectedStatusCodeView : Int -> List (Html msg)
unexpectedStatusCodeView code =
    [ h1 [] [ text <| "Unexpected Response Code - " ++ String.fromInt code ]
    , p []
        [ text <|
            "Our server responded with an unexpected status code."
                ++ " Please contact us if the error continues."
        ]
    ]


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
    , rawHtml content
    ]


adminDashboardView : List (Html msg)
adminDashboardView =
    [ text "This page will eventually contain sections like:"
    , ul []
        [ li [] [ text "Lastest Orders" ]
        , li [] [ text "Newest Accounts" ]
        , li [] [ text "Unapproved Reviews" ]
        , li [] [ text "Monthly Order Count & Totals" ]
        , li [] [ text "Graph of Monthly Order Totals" ]
        ]
    ]


searchResultsView : Search.Data -> Pagination.Data -> CartForms -> PageData.SearchResults -> List (Html Msg)
searchResultsView ({ query } as data) pagination addToCartForms products =
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

                        AttributeSearch SeedAttribute.Organic ->
                            "Organic Products"

                        AttributeSearch SeedAttribute.Heirloom ->
                            "Heirloom Products"

                        AttributeSearch SeedAttribute.Regional ->
                            "South-Eastern Products"

                        AttributeSearch SeedAttribute.SmallGrower ->
                            "Products from Small Farms in our Grower Network"

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
                    , b [] [ text <| String.fromInt (Paginate.getTotalItems products) ]
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
            , ( .isSmallGrower, "From Small Farms" )
            ]
                |> List.filter (\( selector, _ ) -> selector data)
                |> List.map (\( _, name ) -> b [] [ text name ])
                |> List.intersperse (text ", ")
    in
    [ h1 [] [ text header ]
    , hr [] []
    , searchDescription
    ]
        ++ ProductViews.list (SearchResults data) pagination addToCartForms products
