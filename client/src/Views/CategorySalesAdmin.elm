module Views.CategorySalesAdmin exposing (NewForm, NewMsg, initialNewForm, list, new, updateNewForm)

import Api
import Array exposing (Array)
import Category exposing (CategoryId(..))
import Dict
import Html exposing (Html, a, br, div, form, input, option, table, tbody, td, text, th, thead, tr)
import Html.Attributes as A exposing (class, id, name, required, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import PageData exposing (AdminCategorySaleListData, SaleType(..), saleTypeEncoder)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Posix, Zone)
import Update.Utils exposing (noCommand, removeIndex, updateArray)
import Validation as V exposing (formValidation)
import Views.Admin as Admin
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Utils exposing (routeLinkAttributes)



-- LIST


list : Zone -> AdminCategorySaleListData -> List (Html msg)
list zone { sales, categories } =
    let
        renderSale ({ name, saleType, start, end } as sale) =
            tr []
                [ td [] [ text name ]
                , td [] [ renderCategories sale.categories ]
                , td [ class "text-right" ] [ text <| renderSaleType saleType ]
                , td [ class "text-center" ] [ text <| Format.date zone start ]
                , td [ class "text-center" ] [ text <| Format.date zone end ]

                -- TODO: Add Links to Edit pages
                , td [] [ a [] [ text "Edit" ] ]
                ]

        renderCategories =
            List.filterMap (\(CategoryId i) -> Dict.get i categories)
                >> List.sort
                >> List.map text
                >> List.intersperse (br [] [])
                >> div []

        renderSaleType t =
            case t of
                FlatSale c ->
                    Format.cents c

                PercentSale p ->
                    String.fromInt p ++ "%"
    in
    [ div [ class "form-group mb-4" ]
        [ a (class "btn btn-primary" :: routeLinkAttributes (Admin CategorySaleNew))
            [ text "New Category Sale" ]
        ]
    , table [ class "table table-sm table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [ class "text-right" ] [ text "Discount" ]
                , th [ class "text-center" ] [ text "Start Date" ]
                , th [ class "text-center" ] [ text "End Date" ]
                , th [] []
                ]
            ]
        , tbody [] <| List.map renderSale sales
        ]
    ]



-- NEW


type alias NewForm =
    { name : String
    , saleType : SaleTypeTag
    , amount : String
    , start : String
    , end : String
    , categories : Array CategoryId
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialNewForm : NewForm
initialNewForm =
    { name = ""
    , saleType = Flat
    , amount = ""
    , start = ""
    , end = ""
    , categories = Array.initialize 1 (always <| CategoryId 0)
    , errors = Api.initialErrors
    , isSaving = False
    }


type SaleTypeTag
    = Flat
    | Percent


tagToString : SaleTypeTag -> String
tagToString t =
    case t of
        Flat ->
            "Flat Discount"

        Percent ->
            "Percent Discount"


tagToValue : SaleTypeTag -> String
tagToValue t =
    case t of
        Flat ->
            "flat"

        Percent ->
            "percent"


tagParser : String -> Result String SaleTypeTag
tagParser val =
    case val of
        "flat" ->
            Ok Flat

        "percent" ->
            Ok Percent

        _ ->
            Err <| "Could not parse SaleTypeTag: " ++ val


type alias ValidNewForm =
    { name : String
    , saleType : SaleType
    , start : Posix
    , end : Posix
    , categories : Array CategoryId
    }


validateNewForm : NewForm -> Result Api.FormErrors ValidNewForm
validateNewForm model =
    formValidation
        (\type_ start end ->
            { name = model.name
            , saleType = type_
            , start = start
            , end = end
            , categories = model.categories
            }
        )
        |> V.apply "type" (validateSaleType model.saleType model.amount)
        |> V.apply "start" (V.date model.start)
        |> V.apply "end" (V.date model.end)


validateSaleType : SaleTypeTag -> String -> V.Validation SaleType
validateSaleType tag amount =
    let
        validatePercentage =
            V.int amount |> V.map PercentSale

        validateFlat =
            V.cents amount |> V.map FlatSale
    in
    case tag of
        Flat ->
            validateFlat

        Percent ->
            validatePercentage


encodeNewForm : ValidNewForm -> Value
encodeNewForm { name, saleType, start, end, categories } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "type", saleTypeEncoder saleType )
        , ( "start", Iso8601.encode start )
        , ( "end", Iso8601.encode end )
        , ( "categories", Encode.array (\(CategoryId i) -> Encode.int i) categories )
        ]


type NewMsg
    = NewInputName String
    | NewSelectType SaleTypeTag
    | NewInputAmount String
    | NewInputStart String
    | NewInputEnd String
    | NewSelectCategory Int CategoryId
    | NewRemoveCategory Int
    | NewAddCategry
    | NewSubmit
    | NewSubmitResponse (WebData (Result Api.FormErrors Int))


updateNewForm : NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm msg model =
    case msg of
        NewInputName v ->
            noCommand { model | name = v }

        NewSelectType v ->
            noCommand { model | saleType = v }

        NewInputAmount v ->
            noCommand { model | amount = v }

        NewInputStart v ->
            noCommand { model | start = v }

        NewInputEnd v ->
            noCommand { model | end = v }

        NewSelectCategory index v ->
            noCommand
                { model | categories = updateArray index (always v) model.categories }

        NewRemoveCategory index ->
            noCommand
                { model | categories = removeIndex index model.categories }

        NewAddCategry ->
            noCommand
                { model | categories = Array.push (CategoryId 0) model.categories }

        NewSubmit ->
            case validateNewForm model of
                Err e ->
                    ( { model | errors = e }
                    , Ports.scrollToErrorMessage
                    )

                Ok validModel ->
                    ( { model | isSaving = True }
                    , Api.post Api.AdminNewCategorySale
                        |> Api.withJsonBody (encodeNewForm validModel)
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest NewSubmitResponse
                    )

        NewSubmitResponse resp ->
            -- TODO: Refactor this into function - repeated in lots of Admin
            -- views with different initalModels & redirect pages.
            case resp of
                RemoteData.Success (Ok _) ->
                    -- TODO: Redirect to edit page
                    ( initialNewForm
                    , Cmd.none
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | isSaving = False, errors = errors }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure err ->
                    ( { model | isSaving = False, errors = Api.apiFailureToError err }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    ( { model | isSaving = False }, Cmd.none )


new : NewForm -> PageData.AdminNewCategorySaleData -> List (Html NewMsg)
new model { categories } =
    let
        inputRow =
            Form.inputRow model.errors
    in
    [ form [ class <| Admin.formSavingClass model, onSubmit NewSubmit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow model.name NewInputName True "Name" "name" "text" "off"
        , saleTypeRow model.errors model.saleType model.amount NewSelectType NewInputAmount
        , Admin.categorySelects True NewSelectCategory NewAddCategry NewRemoveCategory model categories
        , Form.dateRow model.errors model.start NewInputStart True "Start Date" "start"
        , Form.dateRow model.errors model.end NewInputEnd True "End Date" "end"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Add Category Sale" ]
        ]
    ]


saleTypeRow : Api.FormErrors -> SaleTypeTag -> String -> (SaleTypeTag -> msg) -> (String -> msg) -> Html msg
saleTypeRow errors selectedType enteredAmount selectMsg inputMsg =
    let
        inputId =
            "SaleAmount"

        selectId =
            "SaleType"

        inputAttrs =
            case selectedType of
                Flat ->
                    [ type_ "number", A.min "0.01", A.step "0.01" ]

                Percent ->
                    [ type_ "number", A.min "1", A.max "100", A.step "1" ]

        options =
            [ Flat, Percent ]
                |> List.map
                    (\t ->
                        option [ value <| tagToValue t, selected <| t == selectedType ]
                            [ text <| tagToString t ]
                    )

        fieldErrors =
            Dict.get "type" errors |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
    Form.withLabel "Sale Type"
        True
        [ Form.selectElement selectId "w-25 d-inline-block" tagParser selectMsg options
        , input
            ([ id <| "input" ++ inputId
             , name inputId
             , required True
             , value enteredAmount
             , onInput inputMsg
             , class "form-control w-50 d-inline-block ml-4"
             ]
                ++ inputAttrs
            )
            []
        , errorHtml
        ]
