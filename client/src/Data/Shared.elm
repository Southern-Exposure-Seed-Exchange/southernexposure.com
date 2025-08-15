module Data.Shared exposing (..)

import Components.Tooltip as Tooltip
-- import Messages exposing (Msg)



-- data shared across all component


type alias Shared =
    { tooltips : Tooltip.Model
    -- , tooltipsToParentMsg : Tooltip.Msg -> Msg
    }