module Pages.ResetPassword exposing
    ( Form
    , Msg
    , initial
    , update
    , view
    )

import Data.Api as Api
import Components.Button as Button exposing (defaultButton)
import Dict
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, for, href, id, name, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import RemoteData exposing (WebData)
import Data.Routing.Routing as Routing exposing (Route(..))
import Utils.Update exposing (nothingAndNoCommand)
import Data.User as User exposing (AuthStatus)
import Utils.View exposing (autocomplete, emailInput, labelView, pageTitleView)



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
                        , Ports.removeCartSessionToken ()
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
                    p [ class "tw:pt-[16px]! text-success font-weight-bold" ]
                        [ text <|
                            "Your reset request was successful, please "
                                ++ "check your email for a password reset link."
                        ]

                Just False ->
                    p [ class "tw:pt-[16px]! text-danger font-weight-bold" ]
                        [ text <|
                            "We encountered an error when trying to process your "
                                ++ "request. Please try again or "
                        , a [ href "mailto:gardens@southernexposure.com" ]
                            [ text "contact us" ]
                        , text " for support."
                        ]

                Nothing ->
                    text ""

        resetForm =
            div []
                [ p [ class "tw:pl-[16px] tw:pb-[16px]" ]
                    [ text <|
                        "Enter your email address below & we'll send you an email with a link "
                            ++ "where you can change your password."
                    ]
                , form
                    [ onSubmit <| tagger SubmitRequest
                    ]
                    [ div
                        [ class "tw:p-[16px]! tw:rounded-[16px] tw:bg-[rgba(30,12,3,0.03)]"
                        ]
                        [ labelView "inputEmail" "Email Address" True
                        , input
                            [ id "inputEmail"
                            , class "form-control"
                            , type_ "email"
                            , placeholder "Email Address"
                            , name "email"
                            , onInput <| tagger << Email
                            , value model.email
                            , required True
                            , autofocus True
                            , emailInput
                            , autocomplete "email"
                            ]
                            []
                        ]
                    , infoHtml
                    , div [ class "tw:pt-[16px]" ]
                        [ Button.view { defaultButton | label = "Reset Password", type_ = Button.FormSubmit }
                        ]
                    ]
                ]
    in
    [ pageTitleView "Reset Password"
    , div [ class "tw:flex tw:grid tw:grid-cols-2" ]
        [ resetForm
        ]
    ]


inputRow : Api.FormErrors -> (String -> msg) -> String -> String -> String -> String -> String -> String -> Bool -> Bool -> Html msg
inputRow errors msg inputValue inputId labelText errorField inputType completionType isRequired isAutofocus =
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
        [ labelView ("input" ++ inputId) labelText True
        , input
            [ id <| "input" ++ inputId
            , class inputClass
            , type_ inputType
            , name inputId
            , placeholder labelText
            , onInput msg
            , value inputValue
            , required isRequired
            , autofocus isAutofocus
            , autocomplete completionType
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
    [ pageTitleView "Change Password"
    , div [ class "tw:grid tw:grid-cols-2" ]
        [ form [ onSubmit <| tagger <| SubmitChange code ]
            [ div [ class "tw:p-[16px] tw:rounded-[16px] tw:bg-[rgba(30,12,3,0.03)]" ]
                [ inputRow model.changeErrors
                    (tagger << Password)
                    model.password
                    "password"
                    "Password"
                    "password"
                    "password"
                    "new-password"
                    True
                    True
                , inputRow model.changeErrors
                    (tagger << PasswordConfirm)
                    model.passwordConfirm
                    "passwordConfirm"
                    "Confirm Password"
                    "password"
                    "password"
                    "new-password"
                    True
                    False
                ]
            , errorHtml
            , div [ class "tw:pt-[16px]" ]
                [ Button.view { defaultButton | label = "Update Password", type_ = Button.FormSubmit }
                ]
            ]
        ]
    ]
