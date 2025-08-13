module Components.Alert exposing (..)

import Components.Svg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


type Style
    = Success
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


view : Config msg -> Html msg
view config =
    let
        defaultIcon =
            case config.style of
                Success ->
                    checkSvg

                Danger ->
                    warningSvg

        class_ =
            case config.style of
                Success ->
                    "tw:border-[rgba(77,170,154,0.4)] tw:text-[rgba(77,170,154,1)] tw:bg-[rgba(77,170,154,0.1)]"

                Danger ->
                    "tw:border-[rgba(214,34,70,0.4)] tw:text-[rgba(214,34,70,1)] tw:bg-[rgba(214,34,70,0.1)]"
    in
    div [ class <| class_ ++ " tw:p-[16px] tw:border tw:rounded-[16px] tw:flex tw:gap-[8px] tw:items-start" ]
        [ case config.icon of
            Just customIcon ->
                customIcon

            Nothing ->
                div [ class "tw:pt-[3px]" ]
                    [ defaultIcon
                    ]
        , span [ class "" ] [ config.content ]
        ]
