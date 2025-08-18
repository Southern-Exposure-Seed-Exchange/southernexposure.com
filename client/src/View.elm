module View exposing (pageDescription, pageImage, pageTitle, view)

import BootstrapGallery as Gallery
import Browser exposing (Document)
import Components.Admin.AdminDashboard as AdminDashboard
import Components.Admin.CategorySalesAdmin as CategorySalesAdmin
import Components.Admin.CouponAdmin as CouponAdmin
import Components.Admin.CustomerAdmin as CustomerAdmin
import Components.Admin.OrderAdmin as OrderAdmin
import Components.Admin.ProductSalesAdmin as ProductSalesAdmin
import Components.Admin.SettingsAdmin as SettingsAdmin
import Components.Admin.ShippingAdmin as ShippingAdmin
import Components.Admin.StaticPageAdmin as StaticPageAdmin
import Components.Admin.SurchargesAdmin as SurchargesAdmin
import Components.AdvancedSearch as AdvancedSearch
import Components.Aria as Aria
import Components.Categories.AdminViews as CategoryAdminViews
import Components.Categories.Views as CategoryViews
import Components.OrderDetails as OrderDetails
import Components.Products.AdminViews as ProductAdmin
import Components.Products.Pagination as Pagination
import Components.Products.Views as ProductViews
import Components.SiteUI.Breadcrumbs as SiteBreadcrumbs
import Components.SiteUI.Footer as SiteFooter
import Components.SiteUI.Header as SiteHeader
import Components.SiteUI.Navigation as SiteNavigation
import Components.SiteUI.Sidebar as SiteSidebar
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.PageData as PageData
import Data.Product exposing (productMainImage)
import Data.Routing.Routing exposing (AdminRoute(..), Route(..), isAdminRoute, showSearchbar)
import Data.Search as Search exposing (UniqueSearch(..))
import Data.SeedAttribute as SeedAttribute
import Data.Shared exposing (Shared)
import Data.StaticPage exposing (StaticPage)
import Data.User as User exposing (unauthorized)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (onClick)
import Http
import Mock.Home
import Pages.Cart.Cart as Cart
import Pages.Cart.Type as Cart
import Pages.Checkout as Checkout
import Pages.CreateAccount as CreateAccount
import Pages.EditAddress as EditAddress
import Pages.EditLogin as EditLogin
import Pages.Login as Login
import Pages.MyAccount as MyAccount
import Pages.QuickOrder as QuickOrder
import Pages.ResetPassword as ResetPassword
import Pages.VerificationRequired as VerificationRequired
import Pages.VerifyEmail as VerifyEmail
import Paginate
import RemoteData exposing (WebData)
import Utils.View exposing (pageOverlay, pageTitleView, rawHtml)


view : Model -> Document Msg
view ({ route, pageData, navigationData, zone, helcimUrl } as model) =
    let
        middleContent =
            case route of
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
                    withSidebar pageContentIncludingSearch

        withSidebar content =
            div [ class "container", Aria.live "polite" ]
                [ div [ class "tw:flex tw:flex-col-reverse tw:lg:flex-row tw:gap-[40px]" ]
                    [ SiteSidebar.view route
                    , div [ class "tw:w-full tw:grow", id "main" ] content
                    ]
                ]

        renderAdminTitle adminRoute =
            case adminRoute of
                AdminOrderDetails _ ->
                    text ""

                _ ->
                    h2 [] [ text <| adminTitle model adminRoute ]

        advanceSearchView showDetail =
            if showSearchbar route == True then
                AdvancedSearch.view
                    showDetail
                    NavigateTo
                    AdvancedSearchMsg
                    model.advancedSearchData
                    model.categoryListData

            else
                [ text "" ]

        pageContentIncludingSearch =
            div [] (advanceSearchView route) :: pageContent

        pageContent =
            case route of
                ProductDetails _ _ ->
                    withIntermediateText (ProductViews.detailView model.shared model.addToCartForms)
                        pageData.productDetails

                CategoryDetails _ pagination ->
                    if Paginate.getResponseData pageData.categoryDetails == Nothing then
                        [ loadingInterstitial True ]

                    else if Paginate.getError pageData.categoryDetails /= Nothing then
                        notFoundView

                    else
                        loadingInterstitial False
                            :: CategoryViews.details model.shared
                                pagination
                                model.addToCartForms
                                pageData.categoryDetails

                AdvancedSearch ->
                    []

                SearchResults data pagination ->
                    if Paginate.isLoading pageData.searchResults then
                        [ loadingInterstitial True ]

                    else
                        loadingInterstitial False
                            :: searchResultsView model.shared data pagination model.addToCartForms pageData.searchResults

                PageDetails _ _ ->
                    withIntermediateText staticPageView pageData.pageDetails

                CreateAccount ->
                    CreateAccount.view CreateAccountMsg model.createAccountForm

                VerifyEmail _ ->
                    VerifyEmail.view model.verifyEmailForm LogOut

                CreateAccountSuccess ->
                    CreateAccount.successView

                Login redirectTo clearCart ->
                    Login.view LoginMsg model.loginForm redirectTo clearCart

                VerificationRequired customerId ->
                    VerificationRequired.view customerId model.verificationRequiredForm VerificationRequiredMsg

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

                OrderDetails orderId _ ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                        |> withIntermediateText (apply <| OrderDetails.view zone orderId)

                Cart ->
                    withIntermediateText (Cart.view model.currentUser model.editCartForm) pageData.cartDetails
                        |> List.map (Html.map EditCartMsg)

                QuickOrder ->
                    QuickOrder.view model.quickOrderForms
                        |> List.map (Html.map QuickOrderMsg)

                Checkout ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.checkoutDetails
                        |> withIntermediateText (apply <| Checkout.view model.checkoutForm model.currentUser)
                        |> List.map (Html.map CheckoutMsg)

                CheckoutSuccess orderId _ ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                        |> withIntermediateText
                            (apply <| Checkout.successView zone orderId (model.currentUser == unauthorized))

                Admin Dashboard ->
                    withIntermediateText (AdminDashboard.view model.adminDashboard zone pageData.adminDashboard)
                        pageData.locations
                        |> List.map (Html.map DashboardMsg)

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
                    withIntermediateText (StaticPageAdmin.list model.pageListForm) pageData.adminPageList
                        |> List.map (Html.map PageListMsg)

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
                        |> withIntermediateText (apply <| OrderAdmin.details zone orderId helcimUrl model.orderDetailsForm)
                        |> List.map (Html.map OrderDetailsMsg)

                NotFound ->
                    notFoundView

                Redirect path ->
                    redirectView path

                Admin (CustomerList { query }) ->
                    CustomerAdmin.list query model.customerSearchForm pageData.adminCustomerList
                        |> List.map (Html.map CustomerSearchMsg)

                Admin (CustomerEdit _) ->
                    RemoteData.map2 Tuple.pair pageData.locations pageData.adminEditCustomer
                        |> withIntermediateText (apply <| CustomerAdmin.edit zone helcimUrl model.editCustomerForm)
                        |> List.map (Html.map EditCustomerMsg)

                Admin ProductList ->
                    withIntermediateText (ProductAdmin.list model.productListForm) pageData.adminProductList
                        |> List.map (Html.map ProductListMsg)

                Admin ProductNew ->
                    RemoteData.map2 Tuple.pair pageData.adminSharedProduct pageData.locations
                        |> withIntermediateText (apply <| ProductAdmin.new model.newProductForm)
                        |> List.map (Html.map NewProductMsg)

                Admin (ProductEdit _) ->
                    RemoteData.map2 Tuple.pair pageData.adminSharedProduct pageData.locations
                        |> withIntermediateText (apply <| ProductAdmin.editForm model.editProductForm)
                        |> List.map (Html.map EditProductMsg)

                Admin CouponList ->
                    withIntermediateText (CouponAdmin.list zone) pageData.adminCouponList

                Admin CouponNew ->
                    CouponAdmin.new model.newCouponForm
                        |> List.map (Html.map NewCouponMsg)

                Admin (CouponEdit _) ->
                    withIntermediateText (CouponAdmin.edit model.editCouponForm) pageData.adminEditCoupon
                        |> List.map (Html.map EditCouponMsg)

                Admin Surcharges ->
                    SurchargesAdmin.view model.surchargeForm
                        |> List.map (Html.map SurchargesMsg)

                Admin ShippingMethods ->
                    withIntermediateText (.countries >> ShippingAdmin.view model.shippingMethodForm)
                        pageData.locations
                        |> List.map (Html.map ShippingMsg)

                Admin Settings ->
                    SettingsAdmin.view model.settingsForm
                        |> List.map (Html.map SettingsMsg)

                Admin ProductSaleList ->
                    withIntermediateText (ProductSalesAdmin.list zone) pageData.adminProductSalesList

                Admin ProductSaleNew ->
                    withIntermediateText (ProductSalesAdmin.new model.newProductSaleForm)
                        pageData.adminProductSaleNew
                        |> List.map (Html.map NewProductSaleMsg)

                Admin (ProductSaleEdit _) ->
                    withIntermediateText (ProductSalesAdmin.edit model.editProductSaleForm)
                        pageData.adminEditProductSale
                        |> List.map (Html.map EditProductSaleMsg)

                Admin CategorySaleList ->
                    withIntermediateText (CategorySalesAdmin.list zone) pageData.adminCategorySaleList

                Admin CategorySaleNew ->
                    withIntermediateText (CategorySalesAdmin.new model.newCategorySaleForm) pageData.adminNewCategorySale
                        |> List.map (Html.map NewCategorySaleMsg)

                Admin (CategorySaleEdit _) ->
                    withIntermediateText (CategorySalesAdmin.edit model.editCategorySaleForm) pageData.adminEditCategorySale
                        |> List.map (Html.map EditCategorySaleMsg)

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

                ProductDetails _ _ ->
                    RemoteData.toMaybe pageData.productDetails
                        |> Maybe.map (.predecessors >> List.map .id)
                        |> Maybe.withDefault []

                _ ->
                    []

        skipLink =
            a
                [ class "sr-only sr-only-focusable"
                , href "#main"
                , target "_self"
                , onClick FocusMainContent
                ]
                [ text "Skip to main content" ]
    in
    Document (pageTitle model) <|
        if isAdminRoute route && User.isAdmin model.currentUser then
            [ SiteHeader.adminView
            , SiteNavigation.adminView route
            , middleContent
            ]

        else
            [ skipLink
            , SiteHeader.view model SearchMsg model.searchData model.currentUser model.cartItemCount
            , SiteNavigation.view route model.currentUser navigationData activeCategoryIds model.searchData
            , SiteBreadcrumbs.view route pageData
            , middleContent
            , SiteFooter.view
            , Gallery.modal model.productDetailsLightbox
                |> Html.map ProductDetailsLightbox
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
        ProductDetails _ _ ->
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

        VerifyEmail _ ->
            "Verify your email"

        CreateAccountSuccess ->
            "Account Creation Successful"

        Login _ _ ->
            "Customer Login"

        VerificationRequired _ ->
            "Verification required"

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

        OrderDetails orderId _ ->
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

        Surcharges ->
            "Edit Surcharges"

        ShippingMethods ->
            "Edit Shipping Methods"

        Settings ->
            "Edit Settings"

        ProductSaleList ->
            "Product Sales"

        ProductSaleNew ->
            "New Product Sale"

        ProductSaleEdit _ ->
            "Edit Product Sale"

        CategorySaleList ->
            "Category Sales"

        CategorySaleNew ->
            "New Category Sale"

        CategorySaleEdit _ ->
            "Edit Category Sale"


pageImage : Model -> Maybe String
pageImage { route, pageData } =
    case route of
        ProductDetails _ _ ->
            RemoteData.toMaybe pageData.productDetails
                |> Maybe.map (.product >> productMainImage >> .original)

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
        ProductDetails _ _ ->
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

        Login _ _ ->
            "Log in to your Southern Exposure Seed Exchange account."

        CreateAccount ->
            "Create a free account to purchase products from our catalog."

        VerifyEmail _ ->
            "Verify email to continue shopping"

        VerificationRequired _ ->
            "Verification required before you're able to login"

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

        OrderDetails _ _ ->
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
    pageOverlay isVisible "Loading..."


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
            [ h1 [] [ text "Unexpected Response - Please Refresh Page" ]
            , p []
                [ text <|
                    "Our server responded with data that we were not expecting. "
                        ++ "Please refresh the page to get the latest version of our website. "
                        ++ "If the problem continues, please contact us with the following information:"
                , pre [] [ text errorMsg ]
                ]
            ]


notFoundView : List (Html msg)
notFoundView =
    [ pageTitleView "Page Not Found"
    , p [ class "tw:pl-[8px]" ]
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
                h1 [ class "tw:pl-[8px] tw:pb-[32px]" ] [ text name ]

        html =
            rawHtml content

        -- Used when working on homepage:
        -- html = Mock.Home.view
    in
    [ header
    , div [ class "static-page tw:pl-[8px]" ] [ html ]
    ]


searchResultsView : Shared -> Search.Data -> Pagination.Data -> Cart.CartForms -> PageData.SearchResults -> List (Html Msg)
searchResultsView shared ({ query } as data) pagination addToCartForms products =
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
                p [ class "tw:px-[16px] tw:pt-[14px]" ]
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
    [ h1 [ class "font-semibold! tw:text-[40px]! tw:px-[16px]!" ] [ text header ]
    , searchDescription
    , div [ class "tw:pb-[40px]" ] []
    ]
        ++ ProductViews.listView shared (SearchResults data) pagination addToCartForms products
