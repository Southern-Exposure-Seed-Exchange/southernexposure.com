module SiteUI.Header exposing (adminView, view)

import Components.Button as Button exposing (defaultButton)
import Components.IconButton as IconButton
import Components.Svg exposing (shoppingCartSvg)
import Html exposing (Html, a, br, div, h1, img, li, small, span, text, ul)
import Html.Attributes exposing (alt, class, href, id, src, target)
import Html.Events.Extra exposing (onClickPreventDefault)
import Messages exposing (Msg(..))
import Routing exposing (AdminRoute(..), Route(..), reverse)
import SiteUI.Search as SiteSearch
import User exposing (AuthStatus(..))
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes)


view : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> Html Msg
view searchTagger searchData authStatus cartItemCount =
    div [ class "container" ]
        [ div [ id "site-header", class "tw:py-[30px] tw:flex tw:items-center tw:w-full" ]
            [ logoAndName Routing.homePage
            , div [ class "tw:grow" ] []
            , linksAndSearch searchTagger searchData authStatus cartItemCount
            ]
        ]


logoAndName : Route -> Html Msg
logoAndName linkRoute =
    let
        logoImage =
            div [ class "tw:w-[84px]" ]
                [ img
                    [ id "site-logo"
                    , class ""

                    -- , class "float-left mr-2 mx-lg-3"
                    , src <| Images.static "logos/sese.png"
                    , alt "SESE's Logo - Two Hands Supporting a Growing Flower"
                    ]
                    []
                ]

        titleLink =
            a (class "d-block tw:text-[#1E0C03]! tw:text-[29px]" :: routeLinkAttributes linkRoute)
                [ text "Southern Exposure"
                , br [ class "d-none d-md-block" ] []
                , text " Seed Exchange"
                ]
    in
    div [ class "tw:flex tw:gap-[16px]" ]
        [ a (class "my-auto" :: routeLinkAttributes linkRoute) [ logoImage ]
        , div [ id "site-title", class "media-body my-auto" ]
            [ h1 [ class "media-heading m-0" ] [ titleLink ]
            ]
        ]


cartIcon : Int -> Html Msg
cartIcon cartItemCount =
    a
        [ class "tw:relative tw:p-[6px]! tw:cursor-pointer tw:group", href (reverse Cart) ]
        [ if cartItemCount > 0 then
            div [ class "tw:absolute tw:top-[-6px] tw:right-[-6px]" ]
                [ div [ class "tw:w-[24px] tw:h-[24px] tw:bg-[#D62246] tw:rounded-full tw:flex tw:items-center tw:justify-center" ]
                    [ span [ class "tw:pt-[1px] tw:block tw:text-white tw:text-[12px]" ] [ text <| String.fromInt cartItemCount ]
                    ]
                ]

          else
            div [] []
        , IconButton.view shoppingCartSvg
        ]



-- TODO: update this to support when the user is logged in after the design is ready


linksAndSearch : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> Html Msg
linksAndSearch searchTagger searchData authStatus cartItemCount =
    let
        routeLink content route =
            linkItem (routeLinkAttributes route) content

        linkItem attrs content =
            li [ class "d-inline-block ml-1" ]
                [ a (class "p-2" :: attrs) [ text content ] ]
    in
    div [ class "tw:hidden tw:md:flex tw:md:gap-[16px] tw:flex tw:items-center" ] <|
        [ cartIcon cartItemCount
        , Button.view { defaultButton | label = "Quick order", type_ = Button.Link <| reverse QuickOrder, style = Button.Outline }
        ]
            ++ (case authStatus of
                    Anonymous ->
                        [ Button.view { defaultButton | label = "Log in", type_ = Button.Link <| reverse <| Login Nothing False }
                        ]

                    Authorized _ ->
                        [ routeLink "My Account" MyAccount
                        , linkItem [ href "/account/logout/", onClickPreventDefault LogOut, target "" ]
                            "Log Out"
                        ]
               )


linksAndSearchOld : (SiteSearch.Msg -> Msg) -> SiteSearch.Data -> AuthStatus -> Int -> List (Html Msg)
linksAndSearchOld searchTagger searchData authStatus cartItemCount =
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
