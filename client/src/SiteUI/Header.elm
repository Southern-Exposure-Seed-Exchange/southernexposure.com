module SiteUI.Header exposing (adminView, view)

import Html exposing (Html, a, br, div, h1, img, li, small, text, ul)
import Html.Attributes exposing (alt, class, href, id, src, target)
import Html.Events.Extra exposing (onClickPreventDefault)
import Messages exposing (Msg(..))
import Routing exposing (AdminRoute(..), Route(..))
import SiteUI.Search as SiteSearch
import User exposing (AuthStatus(..))
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes)


view : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> Html Msg
view searchTagger searchData authStatus cartItemCount =
    div [ class "container" ]
        [ div [ id "site-header", class "row clearfix" ]
            [ div [ class "col" ] [ logoAndName Routing.homePage ]
            , div [ class "col-auto ml-auto d-none d-md-block text-right" ] <|
                linksAndSearch searchTagger searchData authStatus cartItemCount
            ]
        ]


logoAndName : Route -> Html Msg
logoAndName linkRoute =
    let
        logoImage =
            img
                [ id "site-logo"
                , class "float-left mr-2 mx-lg-3"
                , src <| Images.static "logos/sese.png"
                , alt "SESE's Logo - Two Hands Supporting a Growing Flower"
                ]
                []

        titleLink =
            a (class "d-block" :: routeLinkAttributes linkRoute)
                [ text "Southern Exposure"
                , br [ class "d-none d-md-block" ] []
                , text " Seed Exchange"
                ]
    in
    div [ class "media justify-content-center" ]
        [ a (class "my-auto" :: routeLinkAttributes linkRoute) [ logoImage ]
        , div [ id "site-title", class "media-body my-auto" ]
            [ h1 [ class "media-heading m-0" ] [ titleLink ]
            ]
        ]


linksAndSearch : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> List (Html Msg)
linksAndSearch searchTagger searchData authStatus cartItemCount =
    let
        quickLinks =
            routeLink "Quick Order" QuickOrder
                :: authLinks
                |> ul [ id "quick-links", class "list-unstyled" ]

        authLinks =
            case authStatus of
                Anonymous ->
                    [ routeLink "Register" CreateAccount
                    , cartLink
                    , routeLink "Log In" <| Login Nothing False
                    ]

                Authorized _ ->
                    [ routeLink "My Account" MyAccount
                    , cartLink
                    , linkItem [ href "/account/logout/", onClickPreventDefault LogOut, target "" ]
                        "Log Out"
                    ]

        cartLink =
            if cartItemCount > 0 then
                routeLink ("Cart (" ++ String.fromInt cartItemCount ++ ")") Cart

            else
                text ""

        routeLink content route =
            linkItem (routeLinkAttributes route) content

        linkItem attrs content =
            li [ class "d-inline-block ml-1" ]
                [ a (class "p-2" :: attrs) [ text content ] ]
    in
    [ quickLinks
    , SiteSearch.form searchTagger "primary" searchData
    , small []
        [ a (routeLinkAttributes AdvancedSearch)
            [ text "Advanced Search" ]
        ]
    ]


adminView : Html Msg
adminView =
    let
        rightLinks =
            ul [ class "list-unstyled" ]
                [ li []
                    [ a (routeLinkAttributes Routing.homePage) [ text "Home Page" ]
                    ]
                , li []
                    [ a [ href "/account/logout/", onClickPreventDefault LogOut, target "" ]
                        [ text "Log Out" ]
                    ]
                ]
    in
    div [ class "admin container-fluid" ]
        [ div [ id "site-header", class "row" ]
            [ div [ class "col" ] [ logoAndName <| Admin Dashboard ]
            , div [ class "col text-right" ] [ rightLinks ]
            ]
        ]
