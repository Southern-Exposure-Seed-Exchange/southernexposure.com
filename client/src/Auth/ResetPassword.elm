module Auth.ResetPassword
    exposing
        ( Form
        , Msg
        , initial
        , update
        , view
        )

import Api
import Dict
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, for, href, id, name, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)


-- Requesting a Password Reset link


type alias Form =
    { email : String
    , password : String
    , passwordConfirm : String
    , requestSuccess : Maybe Bool
    , changeErrors : Api.FormErrors
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , passwordConfirm = ""
    , requestSuccess = Nothing
    , changeErrors = Api.initialErrors
    }


type Msg
    = Email String
    | Password String
    | PasswordConfirm String
    | SubmitRequest
    | SubmitRequestResponse (WebData ())
    | SubmitChange String
    | SubmitChangeResponse (WebData (Result Api.FormErrors AuthStatus))


update : Routing.Key -> Msg -> Form -> Maybe String -> ( Form, Maybe AuthStatus, Cmd Msg )
update key msg model maybeSessionToken =
    case msg of
        Email email ->
            { model | email = email }
                |> nothingAndNoCommand

        Password password ->
            { model | password = password }
                |> nothingAndNoCommand

        PasswordConfirm passwordConfirm ->
            { model | passwordConfirm = passwordConfirm }
                |> nothingAndNoCommand

        SubmitRequest ->
            ( { model | requestSuccess = Nothing }
            , Nothing
            , requestResetEmail model.email
            )

        SubmitRequestResponse response ->
            case response of
                RemoteData.Success () ->
                    { model | requestSuccess = Just True }
                        |> nothingAndNoCommand

                RemoteData.Failure _ ->
                    { model | requestSuccess = Just False }
                        |> nothingAndNoCommand

                _ ->
                    model |> nothingAndNoCommand

        SubmitChange code ->
            ( { model | changeErrors = Api.initialErrors }
            , Nothing
            , changePassword maybeSessionToken code model.password
            )

        SubmitChangeResponse response ->
            case response of
                RemoteData.Success (Ok authStatus) ->
                    ( initial
                    , Just authStatus
                    , Cmd.batch
                        [ Routing.newUrl key MyAccount
                        , Ports.scrollToTop
                        , User.storeDetails authStatus
                        ]
                    )

                RemoteData.Success (Err errors) ->
                    { model | changeErrors = errors }
                        |> nothingAndNoCommand

                _ ->
                    model |> nothingAndNoCommand


requestResetEmail : String -> Cmd Msg
requestResetEmail email =
    Api.post Api.CustomerResetRequest
        |> Api.withJsonBody (Encode.object [ ( "email", Encode.string email ) ])
        |> Api.withJsonResponse (Decode.succeed ())
        |> Api.sendRequest SubmitRequestResponse


changePassword : Maybe String -> String -> String -> Cmd Msg
changePassword maybeSessionToken code password =
    Api.post Api.CustomerPasswordReset
        |> Api.withJsonBody
            (Encode.object
                [ ( "resetCode", Encode.string code )
                , ( "password", Encode.string password )
                , ( "sessionToken"
                  , Maybe.map Encode.string maybeSessionToken
                        |> Maybe.withDefault Encode.null
                  )
                ]
            )
        |> Api.withErrorHandler User.decoder
        |> Api.sendRequest SubmitChangeResponse


view : (Msg -> msg) -> Form -> Maybe String -> List (Html msg)
view tagger model maybeCode =
    case maybeCode of
        Nothing ->
            requestView tagger model

        Just code ->
            changeView tagger model code


requestView : (Msg -> msg) -> Form -> List (Html msg)
requestView tagger model =
    let
        infoHtml =
            case model.requestSuccess of
                Just True ->
                    p [ class "text-success font-weight-bold" ]
                        [ text <|
                            "Your reset request was successful, please "
                                ++ "check your email for a password reset link."
                        ]

                Just False ->
                    p [ class "text-danger font-weight-bold" ]
                        [ text <|
                            "We encountered an error when trying to process your "
                                ++ "request. Please try again or "
                        , a [ href "mailto:gardens@southernexposure.com" ]
                            [ text "contact us" ]
                        , text " for support."
                        ]

                Nothing ->
                    text ""
    in
        [ h1 [] [ text "Reset Password" ]
        , hr [] []
        , p []
            [ text <|
                "Enter your email address below & we'll send you an email with a link "
                    ++ "where you can change your password."
            ]
        , form [ onSubmit <| tagger SubmitRequest ]
            [ infoHtml
            , div [ class "form-group" ]
                [ label [ class "font-weight-bold", for "inputEmail" ]
                    [ text "Email Address:" ]
                , input
                    [ id "inputEmail"
                    , class "form-control"
                    , type_ "email"
                    , name "email"
                    , onInput <| tagger << Email
                    , value model.email
                    , required True
                    , autofocus True
                    ]
                    []
                ]
            , div [ class "form-group clearfix" ]
                [ button
                    [ class "btn btn-primary float-right"
                    , type_ "submit"
                    ]
                    [ text "Reset Password" ]
                ]
            ]
        ]


inputRow : Api.FormErrors -> (String -> msg) -> String -> String -> String -> String -> String -> Bool -> Bool -> Html msg
inputRow errors msg inputValue inputId labelText errorField inputType isRequired isAutofocus =
    let
        fieldErrors =
            Dict.get errorField errors |> Maybe.withDefault []

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
        div [ class "form-group" ]
            [ label [ class "font-weight-bold", for <| "input" ++ inputId ]
                [ text <| labelText ++ ":" ]
            , input
                [ id <| "input" ++ inputId
                , class inputClass
                , type_ inputType
                , name inputId
                , onInput msg
                , value inputValue
                , required isRequired
                , autofocus isAutofocus
                ]
                []
            , errorHtml
            ]


changeView : (Msg -> msg) -> Form -> String -> List (Html msg)
changeView tagger model code =
    let
        errorHtml =
            case Dict.get "" model.changeErrors of
                Nothing ->
                    text ""

                Just errors ->
                    List.map text errors
                        |> List.intersperse (br [] [])
                        |> p [ class "text-danger font-weight-bold" ]
    in
        [ h1 [] [ text "Change Password" ]
        , hr [] []
        , form [ onSubmit <| tagger <| SubmitChange code ]
            [ errorHtml
            , inputRow model.changeErrors
                (tagger << Password)
                model.password
                "password"
                "Password"
                "password"
                "password"
                True
                True
            , inputRow model.changeErrors
                (tagger << PasswordConfirm)
                model.passwordConfirm
                "passwordConfirm"
                "Confirm Password"
                "password"
                "password"
                True
                False
            , div [ class "form-group clearfix" ]
                [ button [ class "btn btn-primary float-right", type_ "submit" ]
                    [ text "Update Password" ]
                ]
            ]
        ]
