module Auth.VerificationRequired exposing
  ( Msg,
    Form,
    initial,
    update,
    view
  )

import Api
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Views.HorizontalForm as Form
import Decode.Utils as Decode
import Views.Utils exposing (icon)

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


update : Msg -> Form -> (Form, Cmd Msg)
update msg  form = case msg of
  RequestVerification customerId ->
      ( {form | mailStatus = Sending }
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
          (form, Cmd.none)


view : Int -> Form -> (Msg -> msg) -> List (Html msg)
view customerId model tagger =
    let
        requestButton = case model.mailStatus of
            DidNotSendYet ->
                button [ class "btn btn-primary float-right ml-3",
                          onClick (tagger (RequestVerification customerId)) ]
                    [ text "Request a new email" ]
            Sending ->
                button [ class "btn btn-primary float-right ml-3", disabled True ]
                    [ text "Sending...", icon "spinner fa-spin ml-2" ]
            _ ->
                text ""

        andvertisementText = case model.mailStatus of
            MailWasSentSuccessfully -> text "Success! Please, check your mailbox for a new verification email."
            AnErrorOccured -> text "Something is wrong on our side, sorry..."
            _ -> text "Unable to find your verification email? Check your spam folder or request a new one!"

    in [ h1 [] [ text "Verify your email" ]
    , hr [] []
    , Form.genericErrorText (not <| Dict.isEmpty model.errors)
    , Api.getErrorHtml "" model.errors
    , p []
        [ div [ class "p-3 mb-2 bg-warning text-dark" ]
            [ text "You can't log into your account before you verify your email" ]
        , div [ class "alert alert-info clearfix" ]
              [ requestButton
              , andvertisementText
              ]
        ]
    ]
