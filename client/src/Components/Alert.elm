module Components.Alert exposing (..)

import Components.Svg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type Style
    = Success
    | Warning
    | Danger


type alias Config msg =
    { content : Html msg
    , style : Style
    , icon : Maybe (Html msg)
    }


defaultAlert : Config msg
defaultAlert =
    { content = text "Something went wrong"
    , style = Success
    , icon = Nothing
    }


defaultDangerIcon : Html msg
defaultDangerIcon =
    div [ class "tw:pt-[3px]" ]
        [ warningSvg
        ]

defaultSuccessIcon : Html msg
defaultSuccessIcon =
    div [ class "tw:pt-[3px]" ]
        [ checkSvg
        ]


view : Config msg -> Html msg
view config =
    let
        class_ =
            case config.style of
                Success ->
                    "tw:border-[rgba(77,170,154,0.4)] tw:text-[rgba(77,170,154,1)] tw:bg-[rgba(77,170,154,0.1)]"

                Warning ->
                    "tw:border-[rgba(255,197,61,1)] tw:bg-[rgba(255,197,61,0.15)]"

                Danger ->
                    "tw:border-[rgba(214,34,70,0.4)] tw:text-[rgba(214,34,70,1)] tw:bg-[rgba(214,34,70,0.1)]"
    in
    div [ class <| class_ ++ " tw:p-[16px] tw:border tw:rounded-[16px] tw:flex tw:gap-[8px] tw:items-start tw:w-full" ]
        [ case config.icon of
            Just customIcon ->
                customIcon

            Nothing ->
                text ""

        , config.content
        ]
