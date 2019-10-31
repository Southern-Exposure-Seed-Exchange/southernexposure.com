module Views.Utils exposing
    ( autocomplete
    , decimalInput
    , emailInput
    , htmlOrBlank
    , icon
    , inputMode
    , numericInput
    , onIntInput
    , rawHtml
    , routeLinkAttributes
    , selectImageFile
    )

import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, i, text)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (on)
import Html.Events.Extra exposing (targetValueInt)
import Json.Decode as Decode
import Markdown exposing (defaultOptions)
import Routing exposing (Route, reverse)


routeLinkAttributes : Route -> List (Attribute msg)
routeLinkAttributes route =
    [ href <| reverse route
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


{-| Convert a string containing markdown & HTML into an Html node.
-}
rawHtml : String -> Html msg
rawHtml =
    Markdown.toHtmlWith { defaultOptions | sanitize = False, smartypants = True }
        []


{-| Set the `inputmode` attribute
-}
inputMode : String -> Attribute msg
inputMode =
    attribute "inputmode"


{-| Show a numeric virtual keyboard
-}
numericInput : Attribute msg
numericInput =
    inputMode "numeric"


{-| Show a virtual keyboard for email addresses
-}
emailInput : Attribute msg
emailInput =
    inputMode "email"


{-| Show a decimal virtual keyboard
-}
decimalInput : Attribute msg
decimalInput =
    inputMode "decimal"


autocomplete : String -> Attribute msg
autocomplete =
    attribute "autocomplete"


{-| Run a command to show the User a file selector. Limits files to the image mimetypes supported by the backend.

Note: Must be used in a response to some user action(e.g., an onClick message handler).

-}
selectImageFile : (File -> msg) -> Cmd msg
selectImageFile =
    Select.file
        [ "image/bmp"
        , "image/gif"
        , "image/png"
        , "image/jpeg"
        , "image/jpg"
        ]
