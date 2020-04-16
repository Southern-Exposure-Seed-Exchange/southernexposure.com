module Views.CategorySalesAdmin exposing
    ( EditForm
    , EditMsg
    , NewForm
    , NewMsg
    , edit
    , initialEditForm
    , initialNewForm
    , list
    , new
    , updateEditForm
    , updateNewForm
    )

import Api
import Array exposing (Array)
import Category exposing (CategoryId(..))
import Dict
import Html exposing (Html, a, br, div, form, table, tbody, td, text, th, thead, tr)
import Html.Attributes as A exposing (class, type_)
import Html.Events exposing (onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Models.Fields exposing (centsToString)
import Models.Utils exposing (posixToDateString)
import PageData exposing (AdminCategorySaleListData, AdminEditCategorySaleData, SaleType(..), saleTypeEncoder)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Posix, Zone)
import Update.Utils exposing (noCommand, removeIndex, updateArray)
import Validation as V exposing (formValidation)
import Views.Admin as Admin exposing (updateEditField)
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Utils exposing (routeLinkAttributes)



-- LIST


list : Zone -> AdminCategorySaleListData -> List (Html msg)
list zone { sales, categories } =
    let
        renderSale ({ id, name, saleType, start, end } as sale) =
            tr []
                [ td [] [ text name ]
                , td [] [ renderCategories sale.categories ]
                , td [ class "text-right" ] [ text <| renderSaleType saleType ]
                , td [ class "text-center" ] [ text <| Format.date zone start ]
                , td [ class "text-center" ] [ text <| Format.date zone end ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| CategorySaleEdit id)
                        [ text "Edit" ]
                    ]
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


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
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
                RemoteData.Success (Ok id) ->
                    ( initialNewForm
                    , Routing.newUrl key <| Admin <| CategorySaleEdit id
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
    Admin.selectInputRow
        { label = "Sale Type"
        , isRequired = True
        , selectMsg = selectMsg
        , inputMsg = inputMsg
        , selectedValue = selectedType
        , selectId = "SaleType"
        , selectOptions = [ Flat, Percent ]
        , selectToValue = tagToValue
        , selectToString = tagToString
        , selectValueParser = tagParser
        , inputValue = enteredAmount
        , inputId = "SaleAmount"
        , inputAttributes =
            \t ->
                case t of
                    Flat ->
                        [ type_ "number", A.min "0.01", A.step "0.01" ]

                    Percent ->
                        [ type_ "number", A.min "1", A.max "100", A.step "1" ]
        , errors = errors
        , errorField = "type"
        }



-- EDIT


type alias EditForm =
    { name : Maybe String
    , saleType : Maybe SaleTypeTag
    , amount : Maybe String
    , start : Maybe String
    , end : Maybe String
    , categories : Maybe (Array CategoryId)
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialEditForm : EditForm
initialEditForm =
    { name = Nothing
    , saleType = Nothing
    , amount = Nothing
    , start = Nothing
    , end = Nothing
    , categories = Nothing
    , errors = Api.initialErrors
    , isSaving = False
    }


type alias ValidEditForm =
    { id : Int
    , name : Maybe String
    , saleType : Maybe SaleType
    , start : Maybe Posix
    , end : Maybe Posix
    , categories : Maybe (Array CategoryId)
    }


valiateEditForm : Int -> EditForm -> Result Api.FormErrors ValidEditForm
valiateEditForm saleId model =
    formValidation
        (\type_ start end ->
            { id = saleId
            , name = model.name
            , saleType = type_
            , start = start
            , end = end
            , categories = model.categories
            }
        )
        |> V.applyMaybe "type"
            (\( t, a ) -> validateSaleType t a)
            (Maybe.map2 Tuple.pair model.saleType model.amount)
        |> V.applyMaybe "start" V.date model.start
        |> V.applyMaybe "end" V.date model.end


encodeEditForm : ValidEditForm -> Value
encodeEditForm model =
    let
        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "name", encodeMaybe Encode.string model.name )
        , ( "type", encodeMaybe saleTypeEncoder model.saleType )
        , ( "start", encodeMaybe Iso8601.encode model.start )
        , ( "end", encodeMaybe Iso8601.encode model.end )
        , ( "categories", encodeMaybe (Encode.array (\(CategoryId i) -> Encode.int i)) model.categories )
        ]


type EditMsg
    = EditInputName String
    | EditSelectType SaleTypeTag
    | EditInputAmount String
    | EditInputStart String
    | EditInputEnd String
    | EditSelectCategory Int CategoryId
    | EditRemoveCategory Int
    | EditAddCategory
    | EditSubmit
    | EditSubmitResponse (WebData (Result Api.FormErrors ()))


updateEditForm : Routing.Key -> WebData AdminEditCategorySaleData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    case msg of
        EditInputName val ->
            noCommand <|
                updateEditField val original (.sale >> .name) <|
                    \v -> { model | name = v }

        EditSelectType val ->
            let
                newAmount =
                    case model.amount of
                        Just _ ->
                            model.amount

                        Nothing ->
                            RemoteData.map (.sale >> .saleType) original
                                |> RemoteData.toMaybe
                                |> Maybe.map saleTypeToAmount
            in
            noCommand { model | saleType = Just val, amount = newAmount }

        EditInputAmount val ->
            let
                newSaleType =
                    case model.saleType of
                        Just _ ->
                            model.saleType

                        Nothing ->
                            RemoteData.map (.sale >> .saleType) original
                                |> RemoteData.toMaybe
                                |> Maybe.map saleTypeToTag
            in
            noCommand <|
                updateEditField val original (.sale >> .saleType >> saleTypeToAmount) <|
                    \v -> { model | amount = v, saleType = newSaleType }

        EditInputStart val ->
            noCommand <|
                updateEditField val original (.sale >> .start >> posixToDateString) <|
                    \v -> { model | start = v }

        EditInputEnd val ->
            noCommand <|
                updateEditField val original (.sale >> .end >> posixToDateString) <|
                    \v -> { model | end = v }

        EditSelectCategory index val ->
            updateCategories model original <| updateArray index (always val)

        EditRemoveCategory index ->
            updateCategories model original <| removeIndex index

        EditAddCategory ->
            updateCategories model original <| Array.push (CategoryId 0)

        EditSubmit ->
            case RemoteData.map (.sale >> .id) original |> RemoteData.toMaybe of
                Nothing ->
                    noCommand model

                Just saleId ->
                    case valiateEditForm saleId model of
                        Err e ->
                            ( { model | errors = e }
                            , Ports.scrollToErrorMessage
                            )

                        Ok validModel ->
                            ( { model | isSaving = True }
                            , Api.patch Api.AdminEditCategorySale
                                |> Api.withJsonBody (encodeEditForm validModel)
                                |> Api.withErrorHandler (Decode.succeed ())
                                |> Api.sendRequest EditSubmitResponse
                            )

        EditSubmitResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin CategorySaleList
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand { model | isSaving = False }


updateCategories :
    EditForm
    -> WebData AdminEditCategorySaleData
    -> (Array CategoryId -> Array CategoryId)
    -> ( EditForm, Cmd msg )
updateCategories model original updater =
    noCommand <|
        case ( model.categories, original ) of
            ( Just cs, _ ) ->
                { model | categories = Just <| updater cs }

            ( _, RemoteData.Success { sale } ) ->
                { model | categories = Just <| updater <| Array.fromList sale.categories }

            _ ->
                model


edit : EditForm -> AdminEditCategorySaleData -> List (Html EditMsg)
edit model { sale, categories } =
    let
        valueWithFallback s1 s2 =
            s1 model |> Maybe.withDefault (s2 sale)

        inputRow s1 s2 =
            Form.inputRow model.errors (valueWithFallback s1 s2)
    in
    [ form [ class <| Admin.formSavingClass model, onSubmit EditSubmit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow .name .name EditInputName True "Name" "name" "text" "off"
        , saleTypeRow model.errors
            (valueWithFallback .saleType (.saleType >> saleTypeToTag))
            (valueWithFallback .amount (.saleType >> saleTypeToAmount))
            EditSelectType
            EditInputAmount
        , Admin.categorySelects True
            EditSelectCategory
            EditAddCategory
            EditRemoveCategory
            { errors = model.errors
            , categories = model.categories |> Maybe.withDefault (Array.fromList sale.categories)
            }
            categories
        , Form.dateRow model.errors (valueWithFallback .start (.start >> posixToDateString)) EditInputStart True "Start Date" "start"
        , Form.dateRow model.errors
            (valueWithFallback .end (.end >> posixToDateString))
            EditInputEnd
            True
            "End Date"
            "end"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Update Category Sale" ]
        ]
    ]


saleTypeToTag : SaleType -> SaleTypeTag
saleTypeToTag t =
    case t of
        FlatSale _ ->
            Flat

        PercentSale _ ->
            Percent


saleTypeToAmount : SaleType -> String
saleTypeToAmount t =
    case t of
        FlatSale cents ->
            centsToString cents

        PercentSale percent ->
            String.fromInt percent
