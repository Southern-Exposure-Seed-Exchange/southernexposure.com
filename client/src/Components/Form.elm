module Components.Form exposing (..)

import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode
import Views.Aria as Aria
import Views.Utils exposing (numericInput, onIntInput)


textView : String -> String -> String -> String -> Html msg
textView typeParam nameParam placeholderParam ariaLabel =
    div [ class "tw:w-full" ]
        [ input
            [ class "tw:block tw:w-full tw:placeholder:text-[rgba(187,182,179,1)] form-control"
            , type_ typeParam
            , name nameParam
            , placeholder placeholderParam
            , Aria.label ariaLabel
            ]
            []
        ]


numberView : String -> Int -> (Int -> msg) -> Html msg
numberView class_ currentVal onChange =
    input
        [ type_ "number"
        , class class_
        , A.min "1"
        , A.step "1"
        , value <| String.fromInt currentVal
        , onIntInput <| onChange
        , numericInput
        , Aria.label "Quantity"
        ]
        []


selectView :
    { tagId : String
    , ariaLabel : String
    , onSelectHandler : fieldValue -> msg
    , valueDecoder : String -> Decode.Decoder fieldValue
    , values : List value
    , view : value -> Html msg
    }
    -> Html msg
selectView { tagId, ariaLabel, onSelectHandler, values, valueDecoder, view } =
    let
        onSelectInt msg =
            targetValue
                |> Decode.andThen valueDecoder
                |> Decode.map msg
                |> on "change"
    in
    select
        [ id tagId
        , class "variant-select form-control"
        , title "Choose a Size"
        , onSelectInt <| onSelectHandler
        , Aria.label ariaLabel
        ]
        (List.map view values)

