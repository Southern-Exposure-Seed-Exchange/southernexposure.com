module SiteUI.Sidebar exposing (view)

import Components.Button as Button exposing (defaultButton)
import Components.Form as Form
import Html exposing (..)
import Html.Attributes as A exposing (alt, class, href, id, name, src, target, type_, value)
import Messages exposing (Msg)
import Products.Pagination as Pagination
import Routing exposing (Route(..))
import Search
import SeedAttribute
import Views.Aria as Aria
import Views.Images as Images
import Views.Utils exposing (icon, routeLinkAttributes)


view : Route -> Html Msg
view route =
    let
        pageLink linkRoute title =
            let
                activeClass =
                    if linkRoute == route then
                        " active"

                    else
                        ""
            in
            div [ class "tw:py-[12px] tw:px-[32px]" ]
                [ a
                    (class (" " ++ activeClass)
                        :: routeLinkAttributes linkRoute
                    )
                    [ text title ]
                ]

        staticPageLink slug =
            pageLink (PageDetails slug Nothing)

        specialSearchLink modifier =
            SearchResults (modifier Search.initial) Pagination.default
                |> pageLink

        pageLinks =
            div [ Aria.role "navigation" ]
                [ div [ class "tw:text-[16px] tw:leading-[20px] tw:py-[20px] tw:rounded-[16px] tw:bg-[rgba(254,245,233,0.6)]" ]
                    [ pageLink QuickOrder "Quick Order"
                    , staticPageLink "about-us" "About Us"
                    , staticPageLink "growing-guides" "Growing Guides"
                    , staticPageLink "retail-stores" "Retail Stores"
                    , staticPageLink "events" "Events"
                    , staticPageLink "faq" "FAQ"
                    , staticPageLink "links" "Links"
                    , div [ class "tw:py-[12px] tw:px-[32px]" ]
                        [ a [ href "/blog/", target "_blank", class "" ]
                            [ text "Blog" ]
                        ]
                    , staticPageLink "contact-us" "Contact Us"
                    , staticPageLink "fundraisers" "Fundraisers"
                    , specialSearchLink identity "All Products"
                    ]
                ]

        specialSearch modifier =
            routeLinkAttributes (SearchResults (modifier Search.initial) Pagination.default)

        attributeLink attribute title modifier =
            li [ class "media" ]
                [ a (class "px-2 w-100" :: specialSearch modifier)
                    [ div [ class "mr-auto d-flex align-items-center" ]
                        [ img
                            [ class "pl-1 my-2"
                            , src <| SeedAttribute.iconUrl attribute
                            , alt <| SeedAttribute.toDescription attribute
                            ]
                            []
                        , h6 [ class "pl-2 py-2 mb-0 font-weight-normal" ] [ text title ]
                        ]
                    ]
                ]

        attributesCard =
            div [ class "card mb-2 icon-legend" ]
                [ ul [ class "list-unstyled mb-0" ]
                    [ attributeLink SeedAttribute.Organic
                        "Certified Organic"
                        (\s -> { s | isOrganic = True })
                    , attributeLink SeedAttribute.Heirloom
                        "Heirloom"
                        (\s -> { s | isHeirloom = True })
                    , attributeLink SeedAttribute.Regional
                        "Especially well-suited to the South-East"
                        (\s -> { s | isRegional = True })
                    , attributeLink SeedAttribute.SmallGrower
                        "From Small Farms"
                        (\s -> { s | isSmallGrower = True })
                    ]
                ]

        newsletterCard =
            div [ class "tw:p-[20px] tw:rounded-[16px] tw:bg-[rgba(254,245,233,0.6)] tw:flex tw:flex-col tw:gap-[16px]" ]
                [ div [ class "tw:flex tw:flex-col tw:gap-[12px]" ]
                    [ div [ class "tw:text-[18px] tw:leading-[24px] tw:font-semibold" ] [ text "Newsletter" ]
                    , div [] [ text "Subscribe to receive our latest Garden Guides \u{2028}and event updates." ]
                    ]
                , form
                    [ A.action "https://sendy.southernexposure.com/subscribe"
                    , A.method "POST"
                    , A.acceptCharset "utf-8"
                    , target "_blank"
                    , class "tw:flex tw:flex-col tw:gap-[16px]"
                    ]
                    [ input [ type_ "hidden", name "list", value "EXGP5iaxXvU4tH7fWWopIQ" ] []
                    , Form.textView "email" "email" "Enter your email" "Email"
                    , Button.view { defaultButton | label = "Subscribe", type_ = Button.FormSubmit, icon = "envelope", padding = Button.Expand }
                    ]
                ]

        facebookCard =
            div [ class "tw:p-[20px] tw:rounded-[16px] tw:bg-[rgba(254,245,233,0.6)] tw:flex tw:flex-col tw:gap-[12px]" ]
                [ div [ class "tw:text-[18px] tw:leading-[24px] tw:font-semibold" ] [ text "Follow us" ]
                , a
                    [ target "_blank"
                    , href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts"
                    , Aria.label "Visit Our Facebook Page"
                    , noOpener
                    ]
                    [ img
                        [ class "tw:w-[140px]"
                        , src <| Images.static "logos/facebook-big-icon-2.svg"
                        , alt "Facebook Logo"
                        ]
                        []
                    ]
                ]

        logoCard =
            div [ class "tw:p-[20px] tw:rounded-[16px] tw:bg-[rgba(254,245,233,0.6)] tw:flex tw:flex-col tw:gap-[12px]" ]
                [ div [ class "tw:text-[18px] tw:leading-[24px] tw:font-semibold" ] [ text "Our Partners" ]
                , div [ class "tw:flex tw:flex-col tw:gap-[8px]" ]
                    [ a
                        [ target "_blank"
                        , href "http://www.smartgardener.com/"
                        , Aria.label "Visit Smart Gardener"
                        , noOpener
                        ]
                        [ img
                            [ class "tw:w-[169px]"
                            , src <| Images.static "logos/smart-gardener-2.png"
                            , alt "Smart Gardener - Simply Grow Great Food"
                            ]
                            []
                        ]
                    , a
                        [ target "_blank"
                        , href "http://www.localharvest.org/"
                        , Aria.label "Visit Local Harvest"
                        , noOpener
                        ]
                        [ img
                            [ class "tw:w-[169px]"
                            , src <| Images.static "logos/local-harvest-2.png"
                            , alt "Local Harvest - Real Food, Real Farmers, Real Community"
                            ]
                            []
                        ]
                    ]
                ]

        noOpener =
            A.attribute "rel" "noopener"
    in
    div [ id "sidebar", class "tw:lg:w-[220px] tw:shrink-0 tw:flex tw:flex-col tw:gap-[16px]" ]
        [ pageLinks

        -- TODO: update this
        , attributesCard
        , newsletterCard
        , facebookCard
        , logoCard
        ]
