module Views.HorizontalForm exposing
    ( checkboxRow
    , dateRow
    , genericErrorText
    , inputRow
    , selectElement
    , selectRow
    , selectCol
    , textareaRow
    , withLabel
    )

import Api
import Components.Alert as Alert exposing (defaultAlert)
import Dict
import Html exposing (..)
import Html.Attributes exposing (checked, class, for, id, name, placeholder, required, rows, type_, value)
import Html.Events exposing (on, onCheck, onInput, targetValue)
import Json.Decode as Decode
import Views.Utils exposing (autocomplete, disableGrammarly, labelView)


genericErrorText : Bool -> Html msg
genericErrorText hasErrors =
    if hasErrors then
        div [ class "tw:pb-[16px]" ]
            [ Alert.view
                { defaultAlert
                    | style = Alert.Danger
                    , content =
                        text <|
                            "There were issues processing your request, "
                                ++ "please correct any errors highlighted below "
                                ++ "& resubmit the form."
                }
            ]

    else
        text ""




{-| TODO: Have users define a Field type w/ functions to convert to required
arguments, then pass these functions to a config function that returns a
version of this function that just takes a Field type?
-}
inputRow : Api.FormErrors -> String -> (String -> msg) -> Bool -> String -> String -> String -> String -> Html msg
inputRow errors inputValue inputMsg isRequired labelText errorField inputType autocompleteType =
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
        , name inputId
        , placeholder labelText
        , class inputClass
        , type_ inputType
        , value inputValue
        , required isRequired
        , onInput inputMsg
        , autocomplete autocompleteType
        ]
        []
        |> (\i -> [ i, errorHtml ])
        |> withLabelCol labelText isRequired


textareaRow : Api.FormErrors -> String -> (String -> msg) -> Bool -> String -> String -> Int -> Html msg
textareaRow errors inputValue inputMsg isRequired labelText errorField rowCount =
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
    textarea
        [ id <| "textarea" ++ inputId
        , name inputId
        , class inputClass
        , required isRequired
        , onInput inputMsg
        , rows rowCount
        , disableGrammarly
        ]
        [ text inputValue ]
        |> (\i -> [ i, errorHtml ])
        |> withLabel labelText isRequired


checkboxRow : Bool -> (Bool -> msg) -> String -> String -> Html msg
checkboxRow value msg labelText name_ =
    div [ class "form-group form-row align-items-center" ]
        [ div [ class "col-sm-3 col-form-label" ] []
        , div [ class "col" ]
            [ div [ class "form-check" ]
                [ input
                    [ id <| "input" ++ name_
                    , name name_
                    , class "form-check-input"
                    , type_ "checkbox"
                    , onCheck msg
                    , checked value
                    ]
                    []
                , label
                    [ class "form-check-label w-100"
                    , for <| "input" ++ name_
                    ]
                    [ text labelText ]
                ]
            ]
        ]


selectRow : (String -> Result String a) -> (a -> msg) -> String -> Bool -> List (Html msg) -> Html msg
selectRow parser msg labelText isRequired options =
    selectElement labelText "" parser msg options
        |> List.singleton
        |> withLabel labelText isRequired

selectCol : (String -> Result String a) -> (a -> msg) -> String -> Bool -> List (Html msg) -> Html msg
selectCol parser msg labelText isRequired options =
    selectElement labelText "" parser msg options
        |> List.singleton
        |> withLabelCol labelText isRequired


{-| Make a `select` element without embedding it in row or label. Used for form
rows with multiple inputs.
-}
selectElement : String -> String -> (String -> Result String a) -> (a -> msg) -> List (Html msg) -> Html msg
selectElement name_ classes parser msg options =
    let
        inputId =
            "input" ++ String.filter (\c -> c /= ' ') name_

        onSelect =
            targetValue
                |> Decode.andThen decoder
                |> Decode.map msg
                |> on "change"

        decoder str =
            case parser str of
                Ok x ->
                    Decode.succeed x

                Err e ->
                    Decode.fail e
    in
    select
        [ id inputId
        , name name_
        , class <| "form-control " ++ classes
        , onSelect
        ]
        options


dateRow : Api.FormErrors -> String -> (String -> msg) -> Bool -> String -> String -> Html msg
dateRow errors date msg isRequired labelText errorField =
    let
        dateId =
            String.filter (\c -> c /= ' ') labelText

        fieldErrors =
            Dict.get errorField errors |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
    withLabel labelText
        isRequired
        [ input
            [ id <| "input" ++ dateId
            , name dateId
            , required isRequired
            , value date
            , type_ "date"
            , onInput msg
            , class "form-control"
            ]
            []
        , errorHtml
        ]


withLabel : String -> Bool -> List (Html msg) -> Html msg
withLabel labelText isRequired input =
    let
        inputId =
            String.filter (\c -> c /= ' ') labelText

        optionalHtml =
            if not isRequired then
                small [ class "horizontal-optional-text d-block text-muted mr-1" ]
                    [ text "(optional)" ]

            else
                text ""
    in
    div [ class "form-group form-row align-items-center" ]
        [ label
            [ class "col-sm-3 col-form-label text-sm-right font-weight-bold"
            , for <| "input" ++ inputId
            ]
            [ text <| labelText ++ ":", optionalHtml ]
        , div [ class "col" ] input
        ]


withLabelCol : String -> Bool -> List (Html msg) -> Html msg
withLabelCol labelText isRequired input =
    let
        inputId =
            String.filter (\c -> c /= ' ') labelText

        optionalHtml =
            if not isRequired then
                small [ class "horizontal-optional-text d-block text-muted mr-1 tw:pb-[6px]" ]
                    [ text "(optional)" ]

            else
                text ""
    in
    div [ class "tw:flex tw:flex-col" ]
        [ div [ class "tw:flex tw:items-center tw:gap-[4px]" ]
            [ labelView ("input" ++ inputId) labelText
            , optionalHtml
            ]
        , div [ class "" ] input
        ]
