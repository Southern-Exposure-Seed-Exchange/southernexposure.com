module SiteUI.Footer exposing (view)

import Html exposing (Html, h4, div, text, node)
import Html.Attributes exposing (id, class)


view : Html msg
view =
    div [ id "footer", class "container" ]
        [ node "footer"
            []
            [ div [ class "row" ]
                [ div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Information" ] ]
                , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Important Links" ] ]
                , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Contact Us" ] ]
                , div [ class "col-sm-12 text-center" ]
                    [ text "Copyright © 2017 Southern Exposure Seed Exchange" ]
                ]
            ]
        ]
