module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Http
import Markdown
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import AdvancedSearch
import Auth.CreateAccount as CreateAccount
import Auth.EditContact as EditContact
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.MyAccount as MyAccount
import Auth.ResetPassword as ResetPassword
import Cart
import Checkout
import Views.Category as CategoryViews
import Messages exposing (Msg(..))
import Model exposing (Model, CartForms)
import PageData
import Products.Pagination as Pagination
import Products.Views as ProductViews
import QuickOrder
import Routing exposing (Route(..))
import Search exposing (UniqueSearch(..))
import SeedAttribute
import SiteUI.Breadcrumbs as SiteBreadcrumbs
import SiteUI.Footer as SiteFooter
import SiteUI.Header as SiteHeader
import SiteUI.Navigation as SiteNavigation
import SiteUI.Sidebar as SiteSidebar
import StaticPage exposing (StaticPage)


view : Model -> Html Msg
view ({ route, pageData, navigationData } as model) =
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
                    MyAccount.view

                EditLogin ->
                    EditLogin.view EditLoginMsg model.editLoginForm model.currentUser

                EditContact ->
                    withIntermediateText
                        (EditContact.view EditContactMsg model.editContactForm)
                        pageData.locations

                Cart ->
                    withIntermediateText (Cart.view model.editCartForm) pageData.cartDetails

                QuickOrder ->
                    QuickOrder.view model.quickOrderForms
                        |> List.map (Html.map QuickOrderMsg)

                Checkout ->
                    RemoteData.map2 (,) pageData.locations pageData.checkoutDetails
                        |> withIntermediateText (uncurry <| Checkout.view model.checkoutForm model.currentUser)
                        |> List.map (Html.map CheckoutMsg)

                CheckoutSuccess orderId newAccount ->
                    RemoteData.map2 (,) pageData.locations pageData.checkoutSuccess
                        |> withIntermediateText (uncurry <| Checkout.successView LogOut orderId newAccount)

                NotFound ->
                    notFoundView

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
            [ SiteHeader.view SearchMsg model.searchData model.currentUser model.cartItemCount
            , SiteNavigation.view navigationData activeCategories model.searchData
            , SiteBreadcrumbs.view route pageData
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

        RemoteData.Failure (Http.BadStatus resp) ->
            if resp.status.code == 404 then
                notFoundView
            else
                [ text <| toString e ]

        e ->
            [ text <| toString e ]


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
            ++ ProductViews.list (SearchResults data) pagination addToCartForms products
