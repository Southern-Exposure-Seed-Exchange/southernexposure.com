module Components.SiteUI.Header exposing (adminView, view)

-- import Components.MobileNav as MobileNav

import Components.Button as Button exposing (defaultButton)
import Components.IconButton as IconButton
import Components.ProfileNavbar as ProfileNavbar
import Components.Shared exposing (cartIcon, logoAndName, searchIcon)
import Components.SiteUI.Search as SiteSearch
import Components.Svg exposing (..)
import Data.Category exposing (CategoryId)
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..), reverse)
import Data.Search as Search exposing (UniqueSearch(..))
import Data.SiteUI exposing (NavigationData)
import Data.User exposing (AuthStatus(..))
import Html exposing (Html, a, br, div, h1, img, li, small, span, text, ul)
import Html.Attributes exposing (alt, class, href, id, src, target)
import Html.Events.Extra exposing (onClickPreventDefault)
import RemoteData exposing (WebData)
import Utils.View exposing (routeLinkAttributes)


view :
    Model
    -> AuthStatus
    -> Int
    -> Route
    -> WebData NavigationData
    -> List CategoryId
    -> Html Msg
view model authStatus cartItemCount route navigationData activeCategoryIds =
    div [ class "se-container tw:hidden tw:lg:block" ]
        [ div [ id "site-header", class "tw:pt-[20px] tw:lg:pt-[30px] tw:pb-0 tw:lg:pb-[30px] tw:flex tw:items-center tw:w-full" ]
            [ logoAndName Routing.homePage
            , div [ class "tw:grow" ] []
            , linksAndSearch model authStatus cartItemCount route navigationData activeCategoryIds
            ]
        ]


linksAndSearch : Model -> AuthStatus -> Int -> Route -> WebData NavigationData -> List CategoryId -> Html Msg
linksAndSearch model authStatus cartItemCount route navigationData activeCategoryIds =
    let
        linkItem attrs content =
            li [ class "d-inline-block ml-1" ]
                [ a (class "p-2" :: attrs) [ text content ] ]
    in
    div [ class "tw:flex tw:gap-[16px] tw:flex tw:items-center" ] <|
        [ if Routing.showSearchbar model.route then
            text ""

          else
            searchIcon
        , cartIcon cartItemCount

        -- , MobileNav.view route authStatus navigationData activeCategoryIds searchData
        , Button.view { defaultButton | label = "Quick order", type_ = Button.Link <| reverse QuickOrder, style = Button.Outline, responsiveClass = "tw:hidden tw:lg:block" }
        ]
            ++ (case authStatus of
                    Anonymous ->
                        [ Button.view { defaultButton | label = "Log in", type_ = Button.Link <| reverse <| Login Nothing False, responsiveClass = "tw:hidden tw:lg:block" }
                        ]

                    Authorized _ ->
                        [ Html.map ProfileNavbarMsg <| ProfileNavbar.view model.profileNavbar
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
