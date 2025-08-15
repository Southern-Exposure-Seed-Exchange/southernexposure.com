module Components.SiteUI.Sidebar exposing (view)

import Components.Button as Button exposing (defaultButton)
import Components.Form as Form
import Html exposing (..)
import Html.Attributes as A exposing (alt, class, href, id, name, src, target, type_, value)
import Data.Msg exposing (Msg)
import Components.Products.Pagination as Pagination
import Data.Routing.Routing as Routing exposing (Route(..))
import Data.Search as Search
import Data.SeedAttribute as SeedAttribute
import Components.Aria as Aria
import Utils.Images as Images
import Utils.View exposing (icon, routeLinkAttributes)


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
                    , Button.view { defaultButton | label = "Subscribe", type_ = Button.FormSubmit, icon = Just (icon "envelope"), padding = Button.Expand }
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
        , newsletterCard
        , facebookCard
        , logoCard
        ]
