module SiteUI.Sidebar exposing (view)

import Html exposing (..)
import Html.Attributes as A exposing (class, href, id, name, src, target, type_, value)
import Messages exposing (Msg)
import Products.Pagination as Pagination
import Routing exposing (Route(..))
import Search
import SeedAttribute
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
            pageLink (PageDetails slug)

        specialSearchLink modifier =
            SearchResults (modifier Search.initial) Pagination.default
                |> pageLink

        pageLinks =
            ul [ class "nav nav-pills nav-fill flex-column text-center mb-2" ]
                [ pageLink QuickOrder "Quick Order"
                , staticPageLink "about-us" "About Us"
                , staticPageLink "growing-guides" "Growing Guides"
                , staticPageLink "retail-stores" "Retail Stores"
                , staticPageLink "events" "Events"
                , staticPageLink "faq" "FAQ"
                , staticPageLink "links" "Links"
                , a [ href "/blog/", target "_blank" ] [ text "Blog" ]
                , staticPageLink "contact-us" "Contact Us"
                , specialSearchLink identity "All Products"
                ]

        specialSearch modifier =
            routeLinkAttributes (SearchResults (modifier Search.initial) Pagination.default)

        attributeLink attribute title modifier =
            li [ class "media" ]
                [ a (class "px-2 w-100" :: specialSearch modifier)
                    [ div [ class "mr-auto d-flex align-items-center" ]
                        [ img [ class "pl-1 my-2", src <| SeedAttribute.iconUrl attribute ] []
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
                    [ a [ target "_blank", href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts" ]
                        [ img [ class "img-fluid", src <| Images.static "logos/facebook-big-icon.png" ] [] ]
                    , hr [] []
                    , div [ class "text-center font-weight-bold" ] [ text "Our Partners" ]
                    , a [ target "_blank", href "http://www.smartgardener.com/" ]
                        [ img [ class "mb-3 img-fluid", src <| Images.static "logos/smart-gardener.jpg" ] [] ]
                    , br [] []
                    , a [ target "_blank", href "http://www.localharvest.org/" ]
                        [ img [ class "img-fluid", src <| Images.static "logos/local-harvest.jpg" ] [] ]
                    ]
                ]
    in
    div [ id "sidebar", class "col-12 col-md-3 col-lg-3 col-xl-2 order-md-1" ]
        [ pageLinks
        , attributesCard
        , newsletterCard
        , logoCard
        ]
