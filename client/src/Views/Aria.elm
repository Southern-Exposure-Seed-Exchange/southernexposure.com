module Views.Aria exposing
    ( label, labelledby, role, current
    , haspopup, expanded
    , controls, disabled, live
    , boolToString
    )

{-| This module defines `aria-*` attributes we use for marking up elements to improve accessibility.

@docs label, labelledby, role, current

@docs haspopup, expanded

@docs controls, disabled, live

-}

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


label : String -> Attribute msg
label =
    attribute "aria-label"


labelledby : String -> Attribute msg
labelledby =
    attribute "aria-labelledby"


role : String -> Attribute msg
role =
    attribute "role"


current : String -> Attribute msg
current =
    attribute "aria-current"


haspopup : Bool -> Attribute msg
haspopup =
    attribute "aria-haspopup" << boolToString


expanded : Bool -> Attribute msg
expanded =
    attribute "aria-expanded" << boolToString


controls : String -> Attribute msg
controls =
    attribute "aria-controls"


disabled : Bool -> Attribute msg
disabled =
    attribute "aria-disabled" << boolToString


live : String -> Attribute msg
live =
    attribute "aria-live"



-- Utils


boolToString : Bool -> String
boolToString isTrue =
    if isTrue then
        "true"

    else
        "false"
