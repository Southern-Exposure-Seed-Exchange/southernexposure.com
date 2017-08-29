module Views.Utils
    exposing
        ( routeLinkAttributes
        , htmlOrBlank
        )

import Html exposing (Html, Attribute, text)
import Html.Attributes exposing (href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Messages exposing (Msg(NavigateTo))
import Routing exposing (Route, reverse)


routeLinkAttributes : Route -> List (Attribute Msg)
routeLinkAttributes route =
    [ onClickPreventDefault <| NavigateTo route
    , href <| reverse route
    ]


htmlOrBlank : (a -> Html msg) -> Maybe a -> Html msg
htmlOrBlank renderFunction =
    Maybe.map renderFunction
        >> Maybe.withDefault (text "")
