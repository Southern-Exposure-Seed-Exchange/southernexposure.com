module SiteUI.Footer exposing (view)

import Html exposing (Html, a, abbr, address, br, div, h4, li, node, span, strong, text, ul)
import Html.Attributes exposing (class, href, id, target, title)
import Messages exposing (Msg)
import Routing exposing (Route(..))
import Views.Microdata as Microdata
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
                , staticPageLink "seeds-for-schools-fundraisers" "Seeds for Schools (Fundraisers)"
                ]

        contactAddress =
            address []
                [ strong [ Microdata.name ] [ text "Southern Exposure Seed Exchange" ]
                , break
                , span (Microdata.address :: Microdata.postalAddress)
                    [ span [ Microdata.streetAddress ] [ text "P.O. Box 460" ]
                    , break
                    , span [ Microdata.addressLocality ] [ text "Mineral" ]
                    , text ", "
                    , span [ Microdata.addressRegion ] [ text "Virginia" ]
                    , text " "
                    , span [ Microdata.postalCode ] [ text "23117" ]
                    ]
                , break
                , a
                    [ Microdata.email
                    , href "mailto:gardens@southernexposure.com?subject=SESE Website Contact"
                    , target "_blank"
                    ]
                    [ text "gardens@southernexposure.com" ]
                , break
                , abbr [ title "Phone" ] [ text "P:" ]
                , span [ Microdata.telephone ] [ text " (540) 894-9480" ]
                , break
                , abbr [ title "Fax" ] [ text "F:" ]
                , span [ Microdata.faxNumber ] [ text " (540) 266-1021" ]
                ]

        contactBlock =
            div Microdata.organization
                [ Microdata.urlLink "https://www.southernexposure.com"
                , Microdata.logoLink "https://www.southernexposure.com/static/img/logos/sese.png"
                , Microdata.sameAsLink "https://www.facebook.com/SouthernExposureSeeds/"
                , Microdata.sameAsLink "https://www.instagram.com/southernexposureseed/"
                , Microdata.sameAsLink "https://github.com/Southern-Exposure-Seed-Exchange"
                , Microdata.sameAsLink "https://www.linkedin.com/company/southern-exposure-seed-exchange"
                , contactAddress
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
                [ footerBlock "Information" "col-10 col-sm-auto d-print-none" informationLinks
                , footerBlock "Important Links" "col-10 col-sm-auto d-print-none" importantLinks
                , footerBlock "Contact Us" "col-10 col-md-5 col-lg-auto" contactBlock
                , div [ class "col-12 text-center" ]
                    [ text "Copyright © 2019 Southern Exposure Seed Exchange" ]
                ]
            ]
        ]
