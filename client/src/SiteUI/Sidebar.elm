module SiteUI.Sidebar exposing (view)

import Html exposing (Html, div, a, img, hr, br, text)
import Html.Attributes exposing (id, class, href, target, src)
import Views.Utils exposing (staticImage)


view : Html msg
view =
    div [ id "sidebar", class "col-12 col-md-3 col-lg-3 col-xl-2 order-md-1" ]
        [ div [ class "card mb-3" ]
            [ div [ class "card-body text-center" ]
                [ a [ target "_blank", href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts" ]
                    [ img [ class "img-fluid", src <| staticImage "logos/facebook-big-icon.png" ] [] ]
                , hr [] []
                , div [ class "text-center font-weight-bold" ] [ text "Our Partners" ]
                , a [ target "_blank", href "http://www.smartgardener.com/" ]
                    [ img [ class "mb-3 img-fluid", src <| staticImage "logos/smart-gardener.jpg" ] [] ]
                , br [] []
                , a [ target "_blank", href "http://www.localharvest.org/" ]
                    [ img [ class "img-fluid", src <| staticImage "logos/local-harvest.jpg" ] [] ]
                ]
            ]
        ]
