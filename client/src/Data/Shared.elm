module Data.Shared exposing (..)

import Components.Tooltip as Tooltip



-- Shared Model meant to be used across all `Component`


type alias Shared =
    { tooltips : Tooltip.Model
    }


initShared : Shared
initShared =
    { tooltips = Tooltip.init
    }
