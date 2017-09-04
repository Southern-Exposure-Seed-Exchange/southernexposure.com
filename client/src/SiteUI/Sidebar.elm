module SiteUI.Sidebar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id, class, href, target, src)
import Messages exposing (Msg)
import Routing exposing (Route(PageDetails))
import Views.Images as Images
import Views.Utils exposing (routeLinkAttributes)


view : Html Msg
view =
    let
        staticPageLink slug title =
            li []
                [ a (class "py-2 d-block" :: (routeLinkAttributes <| PageDetails slug))
                    [ text title ]
                ]

        pageLinks =
            ul [ class "nav nav-pills nav-fill flex-column text-center mb-2" ]
                [ staticPageLink "about-us" "About Us"
                , staticPageLink "growing-guides" "Growing Guides"
                , staticPageLink "retail-stores" "Retail Stores"
                , staticPageLink "events" "Events"
                , staticPageLink "faq" "FAQ"
                , staticPageLink "links" "Links"
                , a [ href "/blog/", target "_blank" ] [ text "Blog" ]
                , staticPageLink "contact-us" "Contact Us"
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
            , logoCard
            ]
