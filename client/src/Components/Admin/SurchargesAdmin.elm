module Components.Admin.SurchargesAdmin exposing
    ( Form
    , Msg(..)
    , getSurcharges
    , initialForm
    , updateForm
    , view
    )

import Data.Api as Api
import Array exposing (Array)
import Data.Category as Category exposing (CategoryId(..))
import Dict
import Html exposing (Html, button, div, form, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Data.Fields exposing (Cents, centsDecoder, centsEncoder, centsToString)
import Data.PageData as PageData
import Ports
import RemoteData exposing (WebData)
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Utils.Update exposing (noCommand, removeIndex, updateArray)
import Data.Validation as Validation exposing (FormValidation)
import Components.Admin.Admin as Admin
import Components.HorizontalForm as Form


type alias Form =
    { surcharges : Array SurchargeForm
    , categories : List PageData.AdminCategorySelect
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialForm : Form
initialForm =
    { surcharges = Array.fromList [ initialSurchargeForm ]
    , categories = []
    , errors = Api.initialErrors
    , isSaving = False
    }


formDecoder : Decoder Form
formDecoder =
    Decode.map4 Form
        (Decode.field "surcharges" <| Decode.array surchargeDecoder)
        (Decode.field "categories" <| Decode.list PageData.adminCategorySelectDecoder)
        (Decode.succeed Api.initialErrors)
        (Decode.succeed False)


type alias SurchargeForm =
    { id : Maybe Int
    , description : String
    , singleFee : String
    , multipleFee : String
    , categories : Array CategoryId
    , isActive : Bool
    }


initialSurchargeForm : SurchargeForm
initialSurchargeForm =
    { id = Nothing
    , description = ""
    , singleFee = ""
    , multipleFee = ""
    , categories = Array.fromList [ CategoryId 0 ]
    , isActive = True
    }


surchargeDecoder : Decoder SurchargeForm
surchargeDecoder =
    Decode.map6 SurchargeForm
        (Decode.field "id" <| Decode.map Just Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "singleFee" <| Decode.map centsToString centsDecoder)
        (Decode.field "multipleFee" <| Decode.map centsToString centsDecoder)
        (Decode.field "categories" <| Decode.array Category.idDecoder)
        (Decode.field "isActive" Decode.bool)


type alias ValidForm =
    List ValidSurchargeForm


validFormEncoder : ValidForm -> Value
validFormEncoder model =
    Encode.object
        [ ( "surcharges", Encode.list validSurchargeEncoder model )
        ]


validateForm : Form -> Result Api.FormErrors ValidForm
validateForm model =
    Array.toList model.surcharges
        |> Validation.indexedValidation "surcharges" validateSurcharge


validateSurcharge : SurchargeForm -> FormValidation ValidSurchargeForm
validateSurcharge model =
    Validation.formValidation
        (\singleFee multipleFee ->
            { id = model.id
            , description = model.description
            , singleFee = singleFee
            , multipleFee = multipleFee
            , categories = model.categories
            , isActive = model.isActive
            }
        )
        |> Validation.apply "singleFee" (Validation.cents model.singleFee)
        |> Validation.apply "multipleFee" (Validation.cents model.multipleFee)


type alias ValidSurchargeForm =
    { id : Maybe Int
    , description : String
    , singleFee : Cents
    , multipleFee : Cents
    , categories : Array CategoryId
    , isActive : Bool
    }


validSurchargeEncoder : ValidSurchargeForm -> Value
validSurchargeEncoder model =
    Encode.object
        [ ( "id", Maybe.withDefault Encode.null <| Maybe.map Encode.int model.id )
        , ( "description", Encode.string model.description )
        , ( "singleFee", centsEncoder model.singleFee )
        , ( "multipleFee", centsEncoder model.multipleFee )
        , ( "categories", Encode.array Category.idEncoder model.categories )
        , ( "isActive", Encode.bool model.isActive )
        ]


type Msg
    = FormMsg Int SurchargeMsg
    | AddSurcharge
    | GetSurchargesData (WebData Form)
    | SubmitForm
    | SubmitFormResponse (WebData (Result Api.FormErrors ()))


updateForm : Routing.Key -> Msg -> Form -> ( Form, Cmd Msg )
updateForm key msg model =
    case msg of
        FormMsg index RemoveSurcharge ->
            ( { model | surcharges = removeIndex index model.surcharges }
            , Cmd.none
            )

        FormMsg index subMsg ->
            ( { model
                | surcharges = updateArray index (updateSurchargeForm subMsg) model.surcharges
              }
            , Cmd.none
            )

        AddSurcharge ->
            ( { model | surcharges = Array.push initialSurchargeForm model.surcharges }
            , Cmd.none
            )

        GetSurchargesData resp ->
            case resp of
                RemoteData.Success f ->
                    ( f, Cmd.none )

                _ ->
                    ( { model | errors = Api.addError "" "Failed to fetch Surcharges from server." model.errors }
                    , Cmd.none
                    )

        SubmitForm ->
            case validateForm model of
                Ok validForm ->
                    ( { model | isSaving = True }
                    , Api.post Api.AdminSurcharges
                        |> Api.withJsonBody (validFormEncoder validForm)
                        |> Api.withErrorHandler (Decode.succeed ())
                        |> Api.sendRequest SubmitFormResponse
                    )

                Err errors ->
                    ( { model | errors = errors }
                    , Ports.scrollToErrorMessage
                    )

        SubmitFormResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( { model | isSaving = False }
                    , Routing.newUrl key <| Admin Surcharges
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


getSurcharges : Cmd Msg
getSurcharges =
    Api.get Api.AdminSurchargesData
        |> Api.withJsonResponse formDecoder
        |> Api.sendRequest GetSurchargesData


type SurchargeMsg
    = InputDescription String
    | InputSingleFee String
    | InputMultipleFee String
    | SelectCategory Int CategoryId
    | RemoveCategory Int
    | AddCategory
    | ToggleIsActive Bool
    | RemoveSurcharge


updateSurchargeForm : SurchargeMsg -> SurchargeForm -> SurchargeForm
updateSurchargeForm msg model =
    case msg of
        InputDescription val ->
            { model | description = val }

        InputSingleFee val ->
            { model | singleFee = val }

        InputMultipleFee val ->
            { model | multipleFee = val }

        SelectCategory index categoryId ->
            { model | categories = updateArray index (always categoryId) model.categories }

        RemoveCategory index ->
            { model | categories = removeIndex index model.categories }

        AddCategory ->
            { model | categories = Array.push (CategoryId 0) model.categories }

        ToggleIsActive val ->
            { model | isActive = val }

        RemoveSurcharge ->
            model


view : Form -> List (Html Msg)
view model =
    [ form [ class <| Admin.formSavingClass model, onSubmit SubmitForm ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , div []
            (Array.indexedMap
                (\i f ->
                    surchargeForm model.errors model.categories i f
                        |> Html.map (FormMsg i)
                )
                model.surcharges
                |> Array.toList
            )
        , div [ class "form-group mb-4" ]
            [ Admin.submitOrSavingButton model "Update Surcharges"
            , button
                [ class "ml-3 btn btn-secondary"
                , type_ "button"
                , onClick AddSurcharge
                ]
                [ text "Add Surcharge" ]
            ]
        ]
    ]


surchargeForm : Api.FormErrors -> List PageData.AdminCategorySelect -> Int -> SurchargeForm -> Html SurchargeMsg
surchargeForm errors categories index model =
    let
        inputRow v m r l n =
            Form.inputRow errors v m r l ("surcharges-" ++ String.fromInt index ++ "-" ++ n)
    in
    Html.fieldset [ class "mb-3" ]
        [ inputRow model.description InputDescription True "Description" "description" "text" "none"
        , inputRow model.singleFee InputSingleFee True "Single-Item Fee" "singleFee" "text" "none"
        , inputRow model.multipleFee InputMultipleFee True "Multiple-Item Fee" "multipleFee" "text" "none"
        , Admin.categorySelects True
            SelectCategory
            AddCategory
            RemoveCategory
            { errors = errors, categories = model.categories }
            categories
        , Form.checkboxRow model.isActive ToggleIsActive "Is Active" "is-active"
        , div [ class "form-group" ]
            [ button [ class "ml-3 btn btn-danger", type_ "button", onClick RemoveSurcharge ]
                [ text "Delete Surcharge" ]
            ]
        ]
