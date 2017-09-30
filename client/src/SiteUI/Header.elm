module SiteUI.Header exposing (view)

import Html exposing (Html, div, a, img, h1, br, ul, li, text, small)
import Html.Attributes exposing (id, class, href, src)
import Html.Events.Extra exposing (onClickPreventDefault)
import Messages exposing (Msg(LogOut))
import Routing exposing (Route(..))
import SiteUI.Search as SiteSearch
import User exposing (AuthStatus(..))
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes)


view : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> Html Msg
view searchTagger searchData authStatus cartItemCount =
    div [ class "container" ]
        [ div [ id "site-header", class "row clearfix" ]
            [ div [ class "col-sm-7 col-lg-6" ] [ logoAndName ]
            , div [ class "col-auto ml-auto d-none d-md-block text-right" ] <|
                linksAndSearch searchTagger searchData authStatus cartItemCount
            ]
        ]


logoAndName : Html Msg
logoAndName =
    let
        logoImage =
            img
                [ id "site-logo"
                , class "float-left mx-3"
                , src <| Images.static "logos/sese.png"
                ]
                []

        titleLink =
            a (routeLinkAttributes <| PageDetails "home")
                [ text "Southern Exposure", br [] [], text "Seed Exchange" ]
    in
        div [ class "media" ]
            [ a (routeLinkAttributes <| PageDetails "home") [ logoImage ]
            , div [ id "site-title", class "media-body my-auto" ]
                [ h1 [ class "media-heading m-0" ] [ titleLink ]
                ]
            ]


linksAndSearch : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> List (Html Msg)
linksAndSearch searchTagger searchData authStatus cartItemCount =
    let
        quickLinks =
            routeLink "Quick Order" (PageDetails "home")
                :: authLinks
                |> ul [ id "quick-links", class "list-unstyled" ]

        authLinks =
            case authStatus of
                Anonymous ->
                    [ routeLink "Register" CreateAccount
                    , cartLink
                    , routeLink "Log In" Login
                    ]

                Authorized _ ->
                    [ routeLink "My Account" MyAccount
                    , cartLink
                    , linkItem [ href "/account/logout/", onClickPreventDefault LogOut ]
                        "Log Out"
                    ]

        cartLink =
            if cartItemCount > 0 then
                routeLink ("Cart (" ++ toString cartItemCount ++ ")") Cart
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
