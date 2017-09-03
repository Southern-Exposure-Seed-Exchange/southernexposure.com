module SiteUI.Footer exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id, class, href, target, title)


view : Html msg
view =
    let
        break =
            br [] []

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
                    [ div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Information" ] ]
                    , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Important Links" ] ]
                    , div [ class "col-sm-4" ]
                        [ h4 [ class "mt-3" ] [ text "Contact Us" ]
                        , contactAddress
                        ]
                    , div [ class "col-sm-12 text-center" ]
                        [ text "Copyright © 2017 Southern Exposure Seed Exchange" ]
                    ]
                ]
            ]
