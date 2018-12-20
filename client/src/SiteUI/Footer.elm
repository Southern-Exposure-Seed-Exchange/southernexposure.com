module SiteUI.Footer exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, target, title)
import Messages exposing (Msg)
import Routing exposing (Route(..))
import Views.Utils exposing (routeLinkAttributes)


view : Html Msg
view =
    let
        break =
            br [] []

        staticPageLink slug title =
            li []
                [ a (routeLinkAttributes <| PageDetails slug) [ text title ]
                ]

        informationLinks =
            ul [ class "list-unstyled" ]
                [ staticPageLink "shipping-info" "Shipping & Returns"
                , staticPageLink "privacy" "Privacy Notice"
                , staticPageLink "conditions" "Conditions of Use"
                , staticPageLink "contact-us" "Contact Us"
                , staticPageLink "site-map" "Site Map"
                ]

        importantLinks =
            ul [ class "list-unstyled" ]
                [ staticPageLink "our-seed-growers" "Our Seed Growers"
                , staticPageLink "our-nongmo-policy" "Our Non-GMO Policy"
                , staticPageLink "quality-promise" "Quality Promise"
                , staticPageLink "growing-guides" "Growing Guides"
                ]

        contactAddress =
            address []
                [ strong [] [ text "Southern Exposure Seed Exchange" ]
                , break
                , text "P.O. Box 460"
                , break
                , text "Mineral, Virginia 23117"
                , break
                , a [ href "mailto:gardens@southernexposure.com?subject=SESE Website Contact", target "_blank" ]
                    [ text "gardens@southernexposure.com" ]
                , break
                , abbr [ title "Phone" ] [ text "P:" ]
                , text " (540) 894-9480"
                , break
                , abbr [ title "Fax" ] [ text "F:" ]
                , text " (540) 894-9481"
                ]
    in
    div [ id "footer", class "container" ]
        [ node "footer"
            []
            [ div [ class "row" ]
                [ div [ class "col-sm-4" ]
                    [ h4 [ class "mt-3" ] [ text "Information" ]
                    , informationLinks
                    ]
                , div [ class "col-sm-4" ]
                    [ h4 [ class "mt-3" ] [ text "Important Links" ]
                    , importantLinks
                    ]
                , div [ class "col-sm-4" ]
                    [ h4 [ class "mt-3" ] [ text "Contact Us" ]
                    , contactAddress
                    ]
                , div [ class "col-sm-12 text-center" ]
                    [ text "Copyright © 2017 Southern Exposure Seed Exchange" ]
                ]
            ]
        ]
