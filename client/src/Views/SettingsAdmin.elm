module Views.SettingsAdmin exposing (Form, Msg, getSettings, initialForm, update, view)

import Api
import Dict
import Html exposing (Html, div, fieldset, form, legend, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Update.Utils exposing (noCommand)
import Views.Admin as Admin exposing (formSavingClass)
import Views.HorizontalForm as Form



-- MODEL


type alias Form =
    { disableCheckout : Bool
    , disabledCheckoutMessage : String
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialForm : Form
initialForm =
    { disableCheckout = False
    , disabledCheckoutMessage = ""
    , errors = Api.initialErrors
    , isSaving = False
    }


formDecoder : Decoder Form
formDecoder =
    Decode.map4 Form
        (Decode.field "disableCheckout" Decode.bool)
        (Decode.field "disabledCheckoutMessage" Decode.string)
        (Decode.succeed Api.initialErrors)
        (Decode.succeed False)


formEncoder : Form -> Value
formEncoder model =
    Encode.object
        [ ( "disableCheckout", Encode.bool model.disableCheckout )
        , ( "disabledCheckoutMessage", Encode.string model.disabledCheckoutMessage )
        ]



-- UPDATE


type Msg
    = ToggleDisableCheckout Bool
    | InputDisableCheckoutMessage String
    | GetSettingsData (WebData Form)
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors ()))


update : Routing.Key -> Msg -> Form -> ( Form, Cmd Msg )
update key msg model =
    case msg of
        ToggleDisableCheckout v ->
            { model | disableCheckout = v } |> noCommand

        InputDisableCheckoutMessage v ->
            { model | disabledCheckoutMessage = v } |> noCommand

        GetSettingsData resp ->
            case resp of
                RemoteData.Success f ->
                    noCommand f

                _ ->
                    ( { model | errors = Api.addError "" "Failed to fetch Settigns from server." model.errors }
                    , Ports.scrollToErrorMessage
                    )

        Submit ->
            ( { model | isSaving = True, errors = Api.initialErrors }
            , Api.post Api.AdminSettings
                |> Api.withJsonBody (formEncoder model)
                |> Api.withErrorHandler (Decode.succeed ())
                |> Api.sendRequest SubmitResponse
            )

        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( { model | isSaving = False }
                    , Cmd.batch [ Routing.newUrl key <| Admin Settings, Ports.scrollToTop ]
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


getSettings : Cmd Msg
getSettings =
    Api.get Api.AdminSettingsData
        |> Api.withJsonResponse formDecoder
        |> Api.sendRequest GetSettingsData



-- VIEW


view : Form -> List (Html Msg)
view model =
    [ form [ class <| formSavingClass model, onSubmit Submit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , fieldset [ class "form-group" ] <| checkoutSettings model
        , div [ class "form-group mb-4" ]
            [ Admin.submitOrSavingButton model "Update Settings"
            ]
        ]
    ]


checkoutSettings : Form -> List (Html Msg)
checkoutSettings model =
    [ legend [] [ text "Checkout" ]
    , Form.checkboxRow model.disableCheckout
        ToggleDisableCheckout
        "Disable Checkout"
        "disable-checkout"
    , Form.textareaRow model.errors
        model.disabledCheckoutMessage
        InputDisableCheckoutMessage
        False
        "Disabled Checkout Message"
        "disabled-checkout-message"
        12
    ]
