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
                , text " (540) 266-1021"
                ]

        footerBlock title class_ content =
            div [ class class_ ]
                [ h4 [ class "mt-3" ] [ text title ]
                , content
                ]
    in
    div [ id "footer", class "container" ]
        [ node "footer"
            []
            [ div [ class "row justify-content-around" ]
                [ footerBlock "Information" "col-10 col-sm-auto" informationLinks
                , footerBlock "Important Links" "col-10 col-sm-auto" importantLinks
                , footerBlock "Contact Us" "col-10 col-md-5 col-lg-auto" contactAddress
                , div [ class "col-12 text-center" ]
                    [ text "Copyright © 2019 Southern Exposure Seed Exchange" ]
                ]
            ]
        ]
