module Components.Shared exposing (..)

import Components.IconButton as IconButton
import Components.Svg exposing (..)
import Data.Msg exposing (Msg(..))
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..), reverse)
import Data.Search exposing (UniqueSearch(..))
import Data.User exposing (AuthStatus(..))
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, id, src)
import Utils.Images as Images
import Utils.View exposing (routeLinkAttributes)


logoAndName : Route -> Html msg
logoAndName linkRoute =
    let
        logoImage =
            div [ class "tw:w-[48px] tw:lg:w-[84px]" ]
                [ img
                    [ id "site-logo"
                    , class ""

                    -- , class "float-left mr-2 mx-lg-3"
                    , src <| Images.static "logos/sese.png"
                    , alt "SESE's Logo - Two Hands Supporting a Growing Flower"
                    ]
                    []
                ]

        homeRoute =
            Routing.homePage

        titleLink =
            a (class "d-block tw:text-[#1E0C03]! tw:text-[18px] tw:lg:text-[29px]" :: routeLinkAttributes linkRoute)
                [ text "Southern Exposure"
                , br [ class "d-block" ] []
                , text " Seed Exchange"
                ]
    in
    div [ class "tw:flex tw:gap-[16px] tw:items-center" ]
        [ a (class "my-auto" :: routeLinkAttributes linkRoute) [ logoImage ]
        , div [ id "site-title", class "media-body tw:pt-[4px] tw:lg:pt-0" ]
            [ h1 [ class "media-heading m-0 " ] [ titleLink ]
            ]
        ]


searchIcon : Html Msg
searchIcon =
    a
        [ class "tw:relative tw:p-[6px]! tw:cursor-pointer tw:group", href "/all-products" ]
        [ searchSvgBig
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
