module Components.SiteUI.Footer exposing (view)

import Components.Microdata as Microdata
import Data.Msg exposing (Msg)
import Data.Routing.Routing as Routing exposing (Route(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils.Images as Images
import Utils.View exposing (routeLinkAttributes)


view : Html Msg
view =
    let
        break =
            br [] []

        staticPageLink slug title =
            div []
                [ a (routeLinkAttributes <| PageDetails slug Nothing) [ text title ]
                ]

        informationLinks =
            div [ class "tw:flex tw:flex-col tw:gap-[16px]" ]
                [ staticPageLink "shipping-info" "Shipping & Returns"
                , staticPageLink "contact-us" "Contact Us"
                ]

        importantLinks =
            div [ class "tw:flex tw:flex-col tw:gap-[16px]" ]
                [ staticPageLink "our-seed-growers" "Our Seed Growers"
                , staticPageLink "our-nongmo-policy" "Our Non-GMO Policy"
                , staticPageLink "quality-promise" "Quality Promise"
                , staticPageLink "growing-guides" "Growing Guides"
                , staticPageLink "fundraisers" "Fundraisers"
                ]

        contactAddress =
            address []
                [ span (Microdata.address :: Microdata.postalAddress)
                    [ break
                    , strong [] [ text "Address:" ]
                    , break
                    , span [ Microdata.streetAddress ] [ text "P.O. Box 460" ]
                    , text ", "
                    , span [ Microdata.addressLocality ] [ text "Mineral" ]
                    , text ", "
                    , span [ Microdata.addressRegion ] [ text "Virginia" ]
                    , text " "
                    , span [ Microdata.postalCode ] [ text "23117" ]
                    ]
                , break
                , break
                , strong [] [ text "Contact:" ]
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
                ]

        contactBlock =
            div Microdata.organization
                [ Microdata.legalNameMeta "Southern Exposure Seed Exchange"
                , Microdata.sloganMeta "Saving the Past for the Future"
                , Microdata.urlLink "https://www.southernexposure.com"
                , Microdata.logoLink "https://www.southernexposure.com/static/img/logos/sese.png"
                , Microdata.sameAsLink "https://www.facebook.com/SouthernExposureSeeds/"
                , Microdata.sameAsLink "https://www.instagram.com/southernexposureseed/"
                , Microdata.sameAsLink "https://github.com/Southern-Exposure-Seed-Exchange"
                , Microdata.sameAsLink "https://www.linkedin.com/company/southern-exposure-seed-exchange"
                , contactAddress
                ]

        footerBlock title class_ content =
            div [ class class_ ]
                [ p [ class "tw:font-semibold tw:pt-[8px] tw:pb-[16px]" ] [ text title ]
                , content
                ]

        contactUsBlock =
            div [ class "tw:grow" ]
                [ -- Logo
                  div [ class "tw:flex tw:items-start tw:gap-[12px]" ]
                    [ img
                        [ id "site-logo"
                        , class "tw:w-[48px] tw:shrink-0"
                        , src <| Images.static "logos/sese.png"
                        , alt "SESE's Logo - Two Hands Supporting a Growing Flower"
                        ]
                        []
                    , p [ Microdata.name, class "poor-richard text-[20px] leading-[24px] tw:w-[140px]" ]
                        [ text "Southern Exposure Seed Exchange"
                        ]
                    ]
                , contactBlock
                ]
    in
    div [ id "footer", class "tw:mt-[80px] tw:lg:mt-[136px] tw:px-[16px] tw:lg:px-[124px] tw:pt-[40px] tw:lg:pt-[60px] tw:pb-[40px] tw:bg-[rgba(30,12,3,0.03)]" ]
        [ node "footer"
            [ class "" ]
            [ div [ class "tw:flex tw:flex-col" ]
                [ div [ class "tw:flex tw:flex-col tw:lg:flex-row tw:gap-[40px]" ]
                    [ contactUsBlock
                    , div [ class "tw:flex tw:gap-[32px]" ]
                        [ footerBlock "Important Links" "" importantLinks
                        , footerBlock "Information" "" informationLinks
                        ]
                    ]
                , div [ class "tw:pt-[40px]" ]
                    [ hr [] []
                    , div [ class "tw:pt-[32px] tw:flex tw:flex-col tw:lg:flex-row tw:gap-[24px]" ]
                        [ div [ class "tw:grow" ] [ text "Copyright © 2020 Southern Exposure Seed Exchange" ]
                        , div [ class "tw:flex tw:gap-[24px]" ]
                            [ staticPageLink "privacy" "Privacy Notice"
                            , staticPageLink "conditions" "Conditions of Use"
                            ]
                        ]
                    ]
                ]
            ]
        ]
