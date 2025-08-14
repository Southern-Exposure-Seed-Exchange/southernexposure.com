module Pages.VerifyEmail exposing
    ( Form
    , Msg
    , initial
    , update
    , verifyEmail
    , view
    )

import Api
import Components.Alert as Alert exposing (defaultAlert)
import Components.Button as Button exposing (defaultButton)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.HorizontalForm as Form
import Views.Utils exposing (pageTitleView)


type alias Form =
    { status : FormStatus
    , errors : Api.FormErrors
    }


type FormStatus
    = Waiting
    | Success
    | Expired


initial : Form
initial =
    { status = Waiting
    , errors = Api.initialErrors
    }


type Msg
    = SuccessfulVerification (WebData (Result Api.FormErrors VerifyStatus))
    | WaitingForResponse


type VerifyStatus
    = VerificationCompleted AuthStatus
    | VerificationExpired


verifyDecoder : Decoder VerifyStatus
verifyDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                if status == "expired" then
                    Decode.succeed VerificationExpired

                else
                    Decode.map VerificationCompleted
                        (Decode.field "data" User.decoder)
            )


verifyEmail : String -> Cmd Msg
verifyEmail uuid =
    Api.post (Api.CustomerVerifyEmail uuid)
        |> Api.withErrorHandler verifyDecoder
        |> Api.sendRequest SuccessfulVerification


update : Msg -> Form -> ( Form, Maybe AuthStatus, Cmd Msg )
update msg form =
    case msg of
        WaitingForResponse ->
            nothingAndNoCommand form

        SuccessfulVerification response ->
            case response of
                RemoteData.Success (Ok (VerificationCompleted authStatus)) ->
                    ( { form | status = Success }
                    , Just authStatus
                    , Cmd.batch
                        [ Ports.scrollToTop
                        , User.storeDetails authStatus
                        ]
                    )

                RemoteData.Success (Ok VerificationExpired) ->
                    ( { form | status = Expired }
                    , Nothing
                    , Cmd.none
                    )

                RemoteData.Success (Err errors) ->
                    ( { form | errors = errors }
                    , Nothing
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { form | errors = Api.apiFailureToError error }
                    , Nothing
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    form |> nothingAndNoCommand


view : Form -> msg -> List (Html msg)
view model logoutMsg =
    [ pageTitleView "Verify an email"
    , div [ class "" ]
        [ Form.genericErrorText (not <| Dict.isEmpty model.errors)
        , div [ class "tw:px-[16px]" ]
            [ Api.getErrorHtml "" model.errors
            ]
        , if Dict.isEmpty model.errors then
            p [ class "tw:pb-[16px]" ]
                [ case model.status of
                    Waiting ->
                        text "Waiting for server responce..."

                    Success ->
                        Alert.view
                            { defaultAlert
                                | content = text "Email was successfully verified!"
                                , style = Alert.Success
                                , icon = Just Alert.defaultSuccessIcon
                            }

                    Expired ->
                        Alert.view
                            { defaultAlert
                                | content = text "Your verification link already expired. Please, check your mailbox for a new one."
                                , style = Alert.Danger
                                , icon = Just Alert.defaultDangerIcon
                            }
                , if model.status == Success then
                    div [ class "tw:p-[16px]" ]
                        [ text <|
                            "You have been automatically logged into "
                                ++ "your new account. If you are using a public computer, "
                                ++ "please log out to keep your account details private."
                        , div [ class "tw:pt-[16px] tw:flex" ]
                            [ Button.view
                                { defaultButton
                                    | label = "Log out"
                                    , style = Button.Outline
                                    , type_ = Button.TriggerMsg logoutMsg
                                }
                            ]
                        ]

                  else
                    text ""
                ]

          else
            text ""
        ]
    ]
