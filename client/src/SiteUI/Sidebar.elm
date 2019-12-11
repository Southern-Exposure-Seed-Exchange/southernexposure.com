module SiteUI.Sidebar exposing (view)

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
            li [ class "nav-item" ]
                [ a
                    (class ("py-1 d-block border-0 nav-link" ++ activeClass)
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
                [ ul [ class "nav nav-pills nav-fill flex-column text-center mb-2" ]
                    [ pageLink QuickOrder "Quick Order"
                    , staticPageLink "about-us" "About Us"
                    , staticPageLink "growing-guides" "Growing Guides"
                    , staticPageLink "retail-stores" "Retail Stores"
                    , staticPageLink "events" "Events"
                    , staticPageLink "faq" "FAQ"
                    , staticPageLink "links" "Links"
                    , li [ class "nav-item" ]
                        [ a [ href "/blog/", target "_blank", class "py-1 d-block nav-link" ]
                            [ text "Blog" ]
                        ]
                    , staticPageLink "contact-us" "Contact Us"
                    , staticPageLink "seeds-for-schools-fundraisers" "Fundraisers"
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
            div [ class "newsletter card mb-2" ]
                [ h5 [ class "card-header text-center" ] [ b [] [ text "Newsletter" ] ]
                , div [ class "card-body" ]
                    [ p [] [ text "Subscribe to receive our latest Garden Guides and event updates." ]
                    , form
                        [ A.action "https://sendy.southernexposure.com/subscribe"
                        , A.method "POST"
                        , A.acceptCharset "utf-8"
                        , target "_blank"
                        ]
                        [ input [ type_ "hidden", name "list", value "EXGP5iaxXvU4tH7fWWopIQ" ] []
                        , div [ class "form-group" ]
                            [ input
                                [ class "form-control form-control-sm"
                                , type_ "email"
                                , name "email"
                                , A.placeholder "Enter your email"
                                , Aria.label "Email"
                                ]
                                []
                            ]
                        , div [ class "form-group" ]
                            [ button
                                [ class "form-control btn btn-primary"
                                , type_ "submit"
                                , name "submit"
                                ]
                                [ icon "envelope"
                                , text " Subscribe"
                                ]
                            ]
                        ]
                    ]
                ]

        logoCard =
            div [ class "card mb-3" ]
                [ div [ class "card-body text-center" ]
                    [ a
                        [ target "_blank"
                        , href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts"
                        , Aria.label "Visit Our Facebook Page"
                        , noOpener
                        ]
                        [ img
                            [ class "img-fluid"
                            , src <| Images.static "logos/facebook-big-icon.png"
                            , alt "Facebook Logo"
                            ]
                            []
                        ]
                    , hr [] []
                    , div [ class "text-center font-weight-bold" ] [ text "Our Partners" ]
                    , a
                        [ target "_blank"
                        , href "http://www.smartgardener.com/"
                        , Aria.label "Visit Smart Gardener"
                        , noOpener
                        ]
                        [ img
                            [ class "mb-3 img-fluid"
                            , src <| Images.static "logos/smart-gardener.jpg"
                            , alt "Smart Gardener - Simply Grow Great Food"
                            ]
                            []
                        ]
                    , br [] []
                    , a
                        [ target "_blank"
                        , href "http://www.localharvest.org/"
                        , Aria.label "Visit Local Harvest"
                        , noOpener
                        ]
                        [ img
                            [ class "img-fluid"
                            , src <| Images.static "logos/local-harvest.jpg"
                            , alt "Local Harvest - Real Food, Real Farmers, Real Community"
                            ]
                            []
                        ]
                    ]
                ]

        noOpener =
            A.attribute "rel" "noopener"
    in
    div [ id "sidebar", class "col-12 col-md-3 col-lg-3 col-xl-2 order-md-1 d-print-none" ]
        [ pageLinks
        , attributesCard
        , newsletterCard
        , logoCard
        ]
