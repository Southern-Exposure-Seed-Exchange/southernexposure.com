module QuickOrder
    exposing
        ( Forms
        , initial
        , Msg
        , update
        , view
        )

import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes as A exposing (id, class, type_, value, name, required)
import Html.Events exposing (on, onInput, onSubmit, onClick)
import Html.Events.Extra exposing (targetValueInt)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)
import Api
import Ports
import User exposing (AuthStatus, User)


-- Model


type alias Forms =
    { forms : Array Form
    , errors : Api.FormErrors
    }


initial : Forms
initial =
    { forms = Array.initialize 14 initialForm
    , errors = Api.initialErrors
    }


type alias Form =
    { sku : String
    , quantity : Int
    }


initialForm : Int -> Form
initialForm index =
    { sku = ""
    , quantity = 1
    }



-- Update


type Msg
    = Sku Int String
    | Quantity Int Int
    | AddRows
    | Submit
    | SubmitResponse Int (WebData (Result Api.FormErrors String))


update : Msg -> Forms -> AuthStatus -> Maybe String -> ( Forms, Maybe ( Int, String ), Cmd Msg )
update msg ({ forms } as model) authStatus maybeSessionToken =
    case msg of
        Sku formIndex sku ->
            updateForm model
                formIndex
                (\f -> { f | sku = sku })

        Quantity formIndex quantity ->
            updateForm model
                formIndex
                (\f -> { f | quantity = quantity })

        AddRows ->
            let
                formCount =
                    Array.length forms
            in
                ( { model
                    | forms =
                        Array.append forms <|
                            Array.initialize 14 (\i -> initialForm <| i + formCount)
                  }
                , Nothing
                , Cmd.none
                )

        Submit ->
            let
                ( quantityToAdd, _, changedForms ) =
                    Array.foldl
                        (\f ( q, i, acc ) ->
                            if not (String.isEmpty f.sku) then
                                ( q + f.quantity, i + 1, ( i, f ) :: acc )
                            else
                                ( q, i + 1, acc )
                        )
                        ( 0, 0, [] )
                        forms

                encodedForms =
                    Encode.object
                        [ ( "items", Encode.list encodeForm changedForms )
                        , ( "sessionToken"
                          , Maybe.map Encode.string maybeSessionToken
                                |> Maybe.withDefault Encode.null
                          )
                        ]

                encodeForm : ( Int, Form ) -> Value
                encodeForm ( index, { sku, quantity } ) =
                    Encode.object
                        [ ( "sku", Encode.string sku )
                        , ( "quantity", Encode.int quantity )
                        , ( "index", Encode.int index )
                        ]
            in
                if List.isEmpty changedForms then
                    ( model, Nothing, Cmd.none )
                else
                    case authStatus of
                        User.Authorized { authToken } ->
                            ( model
                            , Nothing
                            , addItemsToCustomerCart authToken encodedForms quantityToAdd
                            )

                        User.Anonymous ->
                            ( model
                            , Nothing
                            , addItemsToAnonymousCart encodedForms quantityToAdd
                            )

        SubmitResponse quantityToAdd response ->
            case response of
                RemoteData.Success (Ok newToken) ->
                    ( initial, Just ( quantityToAdd, newToken ), Cmd.none )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors }, Nothing, Ports.scrollToID "quick-order-form" )

                _ ->
                    ( model, Nothing, Cmd.none )


updateForm : Forms -> Int -> (Form -> Form) -> ( Forms, Maybe a, Cmd msg )
updateForm ({ forms } as model) formIndex updater =
    Array.get formIndex forms
        |> Maybe.map (\f -> Array.set formIndex (updater f) forms)
        |> Maybe.withDefault forms
        |> (\fs -> ( { model | forms = fs }, Nothing, Cmd.none ))


addItemsToCustomerCart : String -> Value -> Int -> Cmd Msg
addItemsToCustomerCart token encodedForms quantityToAdd =
    Api.post Api.CartQuickOrderCustomer
        |> Api.withToken token
        |> Api.withJsonBody encodedForms
        |> Api.withErrorHandler (Decode.succeed token)
        |> Api.sendRequest (SubmitResponse quantityToAdd)


addItemsToAnonymousCart : Value -> Int -> Cmd Msg
addItemsToAnonymousCart encodedForms quantityToAdd =
    Api.post Api.CartQuickOrderAnonymous
        |> Api.withJsonBody encodedForms
        |> Api.withStringErrorHandler
        |> Api.sendRequest (SubmitResponse quantityToAdd)



-- View


view : Forms -> List (Html Msg)
view model =
    let
        generalErrorHtml =
            if Dict.isEmpty model.errors then
                text ""
            else
                div [ class "alert alert-danger" ]
                    [ p []
                        [ text <|
                            "There were some issues with your Quick Order "
                                ++ "Form. The items have not yet been added to "
                                ++ "your Shopping Cart - please fix the "
                                ++ "highlighted errors below and re-submit the form."
                        ]
                    ]

        buildTableRows index item acc =
            case acc of
                ( [], rows ) ->
                    ( renderForm model.errors index item, rows )

                ( firstFormInRow, rows ) ->
                    ( [], rows ++ [ tr [] (firstFormInRow ++ renderForm model.errors index item) ] )

        applyOddRow ( unappliedForm, rows ) =
            case unappliedForm of
                [] ->
                    rows

                formColumns ->
                    rows ++ [ tr [] (formColumns ++ [ td [] [], td [] [] ]) ]
    in
        [ h1 [] [ text "Quick Order" ]
        , hr [] []
        , p [] [ h5 [] [ text "Instructions" ], instructions ]
        , form [ id "quick-order-form", onSubmit Submit ]
            [ generalErrorHtml
            , table [ class "table table-striped table-sm quick-order-table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Item Number" ]
                        , th [] [ text "Quantity" ]
                        , th [] [ text "Item Number" ]
                        , th [] [ text "Quantity" ]
                        ]
                    ]
                , indexedFoldl buildTableRows ( [], [] ) model.forms
                    |> applyOddRow
                    |> tbody []
                ]
            , div [ class "form-group" ]
                [ button [ class "btn btn-primary", type_ "submit" ]
                    [ text "Add to Cart" ]
                , button [ class "btn btn-secondary ml-3", type_ "button", onClick AddRows ]
                    [ text "Add Rows" ]
                ]
            ]
        ]



-- | Return the 2 Table Columns for a form row


renderForm : Api.FormErrors -> Int -> Form -> List (Html Msg)
renderForm errors index model =
    let
        itemNumberInput =
            input
                [ id <| "item-number-" ++ String.fromInt index
                , class itemNumberClass
                , name <| "item-number-" ++ String.fromInt index
                , type_ "text"
                , value model.sku
                , onInput <| Sku index
                ]
                []

        errorHtml =
            if List.isEmpty itemNumberErrors then
                text ""
            else
                itemNumberErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

        itemNumberClass =
            if List.isEmpty itemNumberErrors && not (Dict.isEmpty errors || String.isEmpty model.sku) then
                "form-control is-valid"
            else if List.isEmpty itemNumberErrors then
                "form-control"
            else
                "form-control is-invalid"

        hasErrors =
            not <| List.isEmpty itemNumberErrors

        itemNumberErrors =
            Dict.get (String.fromInt index) errors
                |> Maybe.withDefault []

        onIntInput msg =
            targetValueInt |> Decode.map msg |> on "input"
    in
        [ td [ class "item-number" ] [ itemNumberInput, errorHtml ]
        , td [ class "quantity" ]
            [ input
                [ id <| "quantity-" ++ String.fromInt index
                , class "form-control"
                , name <| "quantity-" ++ String.fromInt index
                , type_ "number"
                , A.min "1"
                , A.step "1"
                , A.size 5
                , onIntInput <| Quantity index
                , value <| String.fromInt model.quantity
                , required <| not <| String.isEmpty model.sku
                ]
                []
            ]
        ]


instructions : Html msg
instructions =
    ul []
        [ li []
            [ text <|
                String.join " "
                    [ "5-digit Item Numbers can be found at the end of each"
                    , "item description in our catalog, or under the price on"
                    , "our website. A letter at the end of an item number"
                    , "indicates a bulk size. Include the letter if you would"
                    , "like to purchase the bulk size."
                    ]
            ]
        , li []
            [ text "Press "
            , code [] [ text "Tab" ]
            , text " to move between fields."
            ]
        , li []
            [ text "To add the items to your Shopping Cart, press "
            , code [] [ text "Enter" ]
            , text " or click the "
            , code [] [ text "Add to Cart" ]
            , text <|
                " button at the bottom of the page. You can review the "
                    ++ "items and quantities once they have been moved to your Cart."
            ]
        , li []
            [ text "To add more rows, click the "
            , code [] [ text "Add Rows" ]
            , text " button at the bottom of the page."
            ]
        ]



-- Utils


indexedFoldl : (Int -> a -> b -> b) -> b -> Array a -> b
indexedFoldl reducer initialValue =
    Array.foldl
        (\item ( index, acc ) -> ( index + 1, reducer index item acc ))
        ( 0, initialValue )
        >> Tuple.second
