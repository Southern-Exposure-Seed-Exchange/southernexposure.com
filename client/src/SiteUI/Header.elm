module SiteUI.Header exposing (view)

import Html exposing (Html, div, a, img, h1, br, ul, li, text, small)
import Html.Attributes exposing (id, class, href, src)
import Messages exposing (Msg)
import Routing exposing (Route(AdvancedSearch, PageDetails, Login, CreateAccount))
import SiteUI.Search as SiteSearch
import User exposing (AuthStatus(..))
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes)


view : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Html Msg
view searchTagger searchData authStatus =
    div [ class "container" ]
        [ div [ id "site-header", class "row clearfix" ]
            [ div [ class "col-sm-7 col-lg-6" ] [ logoAndName ]
            , div [ class "col-auto ml-auto d-none d-md-block text-right" ] <|
                linksAndSearch searchTagger searchData authStatus
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


linksAndSearch : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> List (Html Msg)
linksAndSearch searchTagger searchData authStatus =
    let
        authLinks =
            case authStatus of
                Anonymous ->
                    [ ( "Register", CreateAccount )
                    , ( "Log In", Login )
                    ]

                Authorized _ ->
                    [ ( "My Account", PageDetails "" )
                    , ( "Log Out", PageDetails "" )
                    ]

        quickLinks =
            ( "Quick Order", PageDetails "home" )
                :: authLinks
                |> List.map
                    (\( content, route ) ->
                        li [ class "d-inline-block ml-1" ]
                            [ a (class "p-2" :: routeLinkAttributes route) [ text content ] ]
                    )
                |> ul [ id "quick-links", class "list-unstyled" ]
    in
        [ quickLinks
        , SiteSearch.form searchTagger "primary" searchData
        , small []
            [ a (routeLinkAttributes AdvancedSearch)
                [ text "Advanced Search" ]
            ]
        ]
