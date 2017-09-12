module Auth.MyAccount exposing (view)

import Html exposing (..)
import Messages exposing (Msg)
import Views.Utils exposing (routeLinkAttributes)
import Routing exposing (Route(EditLogin))


view : List (Html Msg)
view =
    let
        accountLinks =
            [ li []
                [ a (routeLinkAttributes EditLogin)
                    [ text "Change Login Details" ]
                ]
            , li []
                [ a (routeLinkAttributes EditLogin)
                    [ text "Change Contact Information" ]
                ]
            ]
    in
        [ h1 [] [ text "My Account" ]
        , hr [] []
        , ul [] accountLinks
        ]
