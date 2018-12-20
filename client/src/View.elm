module View exposing (view)

import AdvancedSearch
import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.MyAccount as MyAccount
import Auth.ResetPassword as ResetPassword
import Browser exposing (Document)
import Cart
import Checkout
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Http
import Markdown
import Messages exposing (Msg(..))
import Model exposing (CartForms, Model)
import OrderDetails
import PageData
import Paginate exposing (Paginated)
import Products.Pagination as Pagination
import Products.Views as ProductViews
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Search exposing (UniqueSearch(..))
import SeedAttribute
import SiteUI.Breadcrumbs as SiteBreadcrumbs
import SiteUI.Footer as SiteFooter
import SiteUI.Header as SiteHeader
import SiteUI.Navigation as SiteNavigation
import SiteUI.Sidebar as SiteSidebar
import StaticPage exposing (StaticPage)
import Views.Category as CategoryViews


view : Model -> Document Msg
view ({ route, pageData, navigationData, zone } as model) =
    let
        middleContent =
            if route == Checkout then
                div [ class "container" ]
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-md-10" ] pageContent
                        ]
                    ]

            else
                div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class "col order-md-2" ] pageContent
                        , SiteSidebar.view route
                        ]
                    ]

        pageContent =
            case route of
                ProductDetails _ ->
                    withIntermediateText
                        (ProductViews.details model.addToCartForms)
                        pageData.productDetails

                CategoryDetails _ pagination ->
                    if Paginate.isLoading pageData.categoryDetails then
                        [ text "Loading..." ]

                    else if Paginate.getError pageData.categoryDetails /= Nothing then
                        notFoundView

                    else
                        CategoryViews.details pagination
                            model.addToCartForms
                            pageData.categoryDetails

                AdvancedSearch ->
                    withIntermediateText
                        (AdvancedSearch.view NavigateTo AdvancedSearchMsg model.advancedSearchData)
                        pageData.advancedSearch

                SearchResults data pagination ->
                    if Paginate.isLoading pageData.searchResults then
                        [ text "Loading..." ]

                    else
                        searchResultsView data pagination model.addToCartForms pageData.searchResults

                PageDetails _ ->
                    withIntermediateText staticPageView pageData.pageDetails

                CreateAccount ->
                    withIntermediateText
                        (CreateAccount.view CreateAccountMsg model.createAccountForm)
                        pageData.locations

                CreateAccountSuccess ->
                    CreateAccount.successView

                Login ->
                    Login.view LoginMsg model.loginForm

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

                NotFound ->
                    notFoundView

        apply : (a -> b -> c) -> ( a, b ) -> c
        apply f ( a, b ) =
            f a b

        pageTitle =
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

                                AttributeSearch SeedAttribute.Ecological ->
                                    "Ecologically Grown Products"

                PageDetails _ ->
                    getFromPageData .pageDetails .name

                CreateAccount ->
                    "Create an Account"

                CreateAccountSuccess ->
                    "Account Creation Successful"

                Login ->
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

                NotFound ->
                    "Page Not Found"

        -- TODO: Have "Error" & "Loading" titles?
        getFromPageData :
            (PageData.PageData -> RemoteData.WebData a)
            -> (a -> String)
            -> String
        getFromPageData selector f =
            selector pageData
                |> RemoteData.toMaybe
                |> Maybe.map f
                |> Maybe.withDefault ""

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
    Document pageTitle
        [ SiteHeader.view SearchMsg model.searchData model.currentUser model.cartItemCount
        , SiteNavigation.view navigationData activeCategories model.searchData
        , SiteBreadcrumbs.view route pageData
        , middleContent
        , SiteFooter.view
        ]


withIntermediateText : (a -> List (Html msg)) -> WebData a -> List (Html msg)
withIntermediateText subView data =
    case data of
        RemoteData.Loading ->
            [ text "Loading..." ]

        RemoteData.Success d ->
            subView d

        RemoteData.Failure (Http.BadStatus code) ->
            if code == 404 then
                notFoundView

            else
                [ text <| "Bad Status: " ++ String.fromInt code ]

        RemoteData.NotAsked ->
            [ text "BUG! Reached NotAsked!" ]

        _ ->
            [ text "BUG! Unhandled HTTP Error!" ]


notFoundView : List (Html msg)
notFoundView =
    [ h1 [] [ text "Page Not Found" ]
    , p [] [ text "Sorry, we couldn't find the page your were looking for." ]
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
    , Markdown.toHtml [] content
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

                        AttributeSearch SeedAttribute.Ecological ->
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
        ++ ProductViews.list (SearchResults data) pagination addToCartForms products
