module Views.Utils
    exposing
        ( routeLinkAttributes
        , onIntInput
        , htmlOrBlank
        , icon
        )

import Html exposing (Html, Attribute, i, text)
import Html.Attributes exposing (href, class)
import Html.Events exposing (on)
import Html.Events.Extra exposing (onClickPreventDefault, targetValueInt)
import Json.Decode as Decode
import Messages exposing (Msg(NavigateTo))
import Routing exposing (Route, reverse)


routeLinkAttributes : Route -> List (Attribute Msg)
routeLinkAttributes route =
    [ onClickPreventDefault <| NavigateTo route
    , href <| reverse route
    ]


onIntInput : (Int -> msg) -> Attribute msg
onIntInput msg =
    targetValueInt |> Decode.map msg |> on "input"


htmlOrBlank : (a -> Html msg) -> Maybe a -> Html msg
htmlOrBlank renderFunction =
    Maybe.map renderFunction
        >> Maybe.withDefault (text "")


icon : String -> Html msg
icon faClass =
    i [ class <| "fa fa-" ++ faClass ] []
