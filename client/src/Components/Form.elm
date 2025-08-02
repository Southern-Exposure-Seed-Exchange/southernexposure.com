module Components.Form exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Aria as Aria


textView : String -> String -> String -> String -> Html msg
textView typeParam nameParam placeholderParam ariaLabel =
    div [ class "tw:w-full tw:py-[8px] tw:px-[12px] bg-white tw:rounded-[8px] tw:border tw:border-[rgba(232,231,230,1)]" ]
        [ input
            [ class "tw:block tw:w-full tw:placeholder:text-[rgba(187,182,179,1)]"
            , type_ typeParam
            , name nameParam
            , placeholder placeholderParam
            , Aria.label ariaLabel
            ]
            []
        ]
