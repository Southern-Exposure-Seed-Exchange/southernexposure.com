module Pages.VerificationRequired exposing
    ( Form
    , Msg
    , initial
    , update
    , view
    )

import Api
import Components.Alert as Alert exposing (defaultAlert)
import Components.Button as Button exposing (defaultButton)
import Decode.Utils as Decode
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Views.HorizontalForm as Form
import Views.Utils exposing (icon, pageTitleView)


type alias Form =
    { mailStatus : MailStatus
    , errors : Api.FormErrors
    }


type MailStatus
    = DidNotSendYet
    | Sending
    | MailWasSentSuccessfully
    | AnErrorOccured


initial : Form
initial =
    { mailStatus = DidNotSendYet
    , errors = Api.initialErrors
    }


type Msg
    = ResponseToVerificationRequest (WebData (Result Api.FormErrors ()))
    | RequestVerification Int


update : Msg -> Form -> ( Form, Cmd Msg )
update msg form =
    case msg of
        RequestVerification customerId ->
            ( { form | mailStatus = Sending }
            , Api.post (Api.CustomerRequestVerification customerId)
                |> Api.withErrorHandler Decode.unit
                |> Api.sendRequest ResponseToVerificationRequest
            )

        ResponseToVerificationRequest response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( { form | mailStatus = MailWasSentSuccessfully }
                    , Cmd.batch
                        [ Ports.scrollToTop
                        ]
                    )

                RemoteData.Success (Err errors) ->
                    ( { form | mailStatus = AnErrorOccured, errors = errors }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { form | mailStatus = AnErrorOccured, errors = Api.apiFailureToError error }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    ( form, Cmd.none )


view : Int -> Form -> (Msg -> msg) -> List (Html msg)
view customerId model tagger =
    let
        requestButton =
            case model.mailStatus of
                DidNotSendYet ->
                    Button.view { defaultButton | label = "Request a new email", type_ = Button.TriggerMsg (tagger (RequestVerification customerId)) }

                Sending ->
                    Button.view
                        { defaultButton
                            | label = "Sending..."
                            , type_ = Button.Disabled
                        }

                _ ->
                    text ""

        andvertisementText =
            case model.mailStatus of
                MailWasSentSuccessfully ->
                    text "Success! Please, check your mailbox for a new verification email."

                AnErrorOccured ->
                    text "Something is wrong on our side, sorry..."

                _ ->
                    text "Unable to find your verification email? Check your spam folder or request a new one!"
    in
    [ pageTitleView "Confirm your email to log in"
    , Form.genericErrorText (not <| Dict.isEmpty model.errors)
    , Api.getErrorHtml "" model.errors
    , p []
        [ Alert.view
            { defaultAlert
                | content = text "You can't log into your account before you verify your email"
                , style = Alert.Danger
                , icon = Just Alert.defaultDangerIcon
            }
        , p [ class "tw:p-[16px]" ] [ andvertisementText ]
        , div [ class "tw:flex" ]
            [ requestButton
            ]
        ]
    ]
