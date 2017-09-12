module Views.HorizontalForm
    exposing
        ( inputRow
        , withLabel
        )

import Dict
import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, required)
import Html.Events exposing (onInput)
import Api


{-| TODO: Have users define a Field type w/ functions to convert to required
arguments, then pass these functions to a config function that returns a
version of this function that just takes a Field type?
-}
inputRow : Api.FormErrors -> (String -> msg) -> Bool -> String -> String -> String -> Html msg
inputRow errors inputMsg isRequired labelText errorField inputType =
    let
        inputId =
            String.filter (\c -> c /= ' ') labelText

        fieldErrors =
            Dict.get errorField errors
                |> Maybe.withDefault []

        inputClass =
            if List.isEmpty fieldErrors && not (Dict.isEmpty errors) then
                "form-control is-valid"
            else if List.isEmpty fieldErrors then
                "form-control"
            else
                "form-control is-invalid"

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""
            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
        input
            [ id <| "input" ++ inputId
            , class inputClass
            , type_ inputType
            , required isRequired
            , onInput inputMsg
            ]
            []
            |> (\i -> [ i, errorHtml ])
            |> withLabel labelText isRequired


withLabel : String -> Bool -> List (Html msg) -> Html msg
withLabel labelText isRequired input =
    let
        inputId =
            String.filter (\c -> c /= ' ') labelText

        requiredHtml isRequired =
            if isRequired then
                span [ class "text-danger" ] [ text "*" ]
            else
                text ""
    in
        div [ class "form-group form-row align-items-center" ]
            [ label
                [ class "col-sm-3 col-form-label text-right font-weight-bold"
                , for <| "input" ++ inputId
                ]
                [ requiredHtml isRequired, text " ", text <| labelText ++ ":" ]
            , div [ class "col" ] input
            ]
