module Auth.VerifyEmail exposing
  ( Form,
    Msg,
    verifyEmail,
    initial,
    update,
    view
  )


import Api
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.HorizontalForm as Form
import Json.Decode as Decode exposing (Decoder)

type alias Form =
    { status : FormStatus
    , errors : Api.FormErrors
    }

type FormStatus = Waiting | Success | Expired

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
    |> Decode.andThen (\status ->
      if status == "expired"
        then Decode.succeed VerificationExpired
        else Decode.map VerificationCompleted
                (Decode.field "data" User.decoder))

verifyEmail : String -> Cmd Msg
verifyEmail uuid =
  Api.post (Api.CustomerVerifyEmail uuid)
        |> Api.withErrorHandler verifyDecoder
        |> Api.sendRequest SuccessfulVerification


update : Msg -> Form -> ( Form, Maybe AuthStatus, Cmd Msg )
update msg form = case msg of
  WaitingForResponse -> nothingAndNoCommand form
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
    [ h1 [] [ text "Verify an email" ]
    , hr [] []
    , Form.genericErrorText (not <| Dict.isEmpty model.errors)
    , Api.getErrorHtml "" model.errors
    , if Dict.isEmpty model.errors
      then p []
        [ text <| case model.status of
            Waiting -> "Waiting for server responce..."
            Success -> "Email was successfully verified!"
            Expired -> "Your verification link already expired. Please, check your mailbox for a new one."
        , if model.status == Success
          then div [ class "alert alert-info clearfix" ]
              [ button [ class "btn btn-primary float-right ml-3", onClick logoutMsg ]
                  [ text "Log Out" ]
              , text <|
                  "You have been automatically logged into "
                      ++ "your new account. If you are using a public computer, "
                      ++ "please log out to keep your account details private."
              ]
          else text ""
        ]
      else text ""
    ]
