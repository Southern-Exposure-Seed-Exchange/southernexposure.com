module Auth.Login
    exposing
        ( view
        )

import Html exposing (..)


view : List (Html msg)
view =
    [ h1 [] [ text "Please Sign In" ]
    , hr [] []
    ]
