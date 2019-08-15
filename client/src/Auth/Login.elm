module Auth.Login exposing
    ( Form
    , Msg
    , initial
    , update
    , view
    )

import Api
import Dict
import Html exposing (..)
import Html.Attributes exposing (autofocus, checked, class, for, href, id, required, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..), reverse)
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus, UserId(..))
import Views.Utils exposing (autocomplete, emailInput, routeLinkAttributes)



-- MODEL


type alias Form =
    { email : String
    , password : String
    , remember : Bool
    , errors : Api.FormErrors
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , remember = True
    , errors = Api.initialErrors
    }


encode : Form -> Maybe String -> Value
encode { email, password } maybeSessionToken =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        , ( "sessionToken"
          , Maybe.map Encode.string maybeSessionToken
                |> Maybe.withDefault Encode.null
          )
        ]



-- UPDATE


type Msg
    = Email String
    | Password String
    | Remember Bool
    | CreateAccountPage
    | ResetPasswordPage
    | SubmitForm
    | SubmitResponse (WebData (Result Api.FormErrors AuthStatus))


update : Routing.Key -> Msg -> Form -> Maybe String -> ( Form, Maybe AuthStatus, Cmd Msg )
update key msg model maybeSessionToken =
    case msg of
        Email email ->
            { model | email = email }
                |> nothingAndNoCommand

        Password password ->
            { model | password = password }
                |> nothingAndNoCommand

        Remember remember ->
            { model | remember = remember }
                |> nothingAndNoCommand

        CreateAccountPage ->
            ( model
            , Nothing
            , Cmd.batch
                [ Routing.newUrl key CreateAccount
                , Ports.scrollToTop
                ]
            )

        ResetPasswordPage ->
            ( model
            , Nothing
            , Cmd.batch
                [ Routing.newUrl key <| ResetPassword <| Just ""
                , Ports.scrollToTop
                ]
            )

        SubmitForm ->
            ( { model | errors = Api.initialErrors }
            , Nothing
            , login model maybeSessionToken
            )

        -- TODO: Better error case handling/feedback
        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok authStatus) ->
                    ( initial
                    , Just authStatus
                    , Cmd.batch
                        [ Routing.newUrl key <| PageDetails "home"
                        , rememberAuth model.remember authStatus
                        , Ports.removeCartSessionToken ()
                        ]
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors }, Nothing, Ports.scrollToTop )

                _ ->
                    debugResponse response model


rememberAuth : Bool -> AuthStatus -> Cmd msg
rememberAuth remember authStatus =
    if remember then
        User.storeDetails authStatus

    else
        Cmd.none


{-| TODO: This no longer works since we use the --optimize flag. Show an error instead.
-}
debugResponse : c -> a -> ( a, Maybe b, Cmd msg )
debugResponse response model =
    --let
    --    _ =
    --        Debug.log "Bad Response" response
    --in
    model |> nothingAndNoCommand


login : Form -> Maybe String -> Cmd Msg
login form maybeSessionToken =
    Api.post Api.CustomerLogin
        |> Api.withJsonBody (encode form maybeSessionToken)
        |> Api.withErrorHandler User.decoder
        |> Api.sendRequest SubmitResponse



-- VIEW


view : (Msg -> msg) -> Form -> List (Html msg)
view tagger model =
    let
        loginForm =
            form [ onSubmit <| tagger SubmitForm ]
                [ fieldset []
                    [ legend [] [ text "Returning Customers" ]
                    , errorHtml
                    , loginInputs
                    , rememberCheckbox
                    , div [ class "form-group" ]
                        [ button
                            [ class "btn btn-primary"
                            , type_ "submit"
                            ]
                            [ text "Log In" ]
                        ]
                    , div []
                        [ a
                            (routeLinkAttributes <| ResetPassword <| Just "")
                            [ text "Forgot your password?" ]
                        ]
                    ]
                ]

        errorHtml =
            case Dict.get "" model.errors of
                Nothing ->
                    text ""

                Just errors ->
                    List.map text errors
                        |> List.intersperse (br [] [])
                        |> p [ class "text-danger font-weight-bold" ]

        loginInputs =
            [ div [ class "form-group" ]
                [ label [ class "font-weight-bold", for "emailInput" ]
                    [ text "Email Address:" ]
                , input
                    [ id "emailInput"
                    , class "form-control"
                    , type_ "email"
                    , onInput <| tagger << Email
                    , value model.email
                    , required True
                    , autofocus True
                    , emailInput
                    , autocomplete "email"
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [ class "font-weight-bold", for "passwordInput" ]
                    [ text "Password:" ]
                , input
                    [ id "passwordInput"
                    , class "form-control"
                    , type_ "password"
                    , onInput <| tagger << Password
                    , value model.password
                    , required True
                    , autocomplete "current-password"
                    ]
                    []
                ]
            ]
                |> div []

        rememberCheckbox =
            div [ class "form-check" ]
                [ label [ class "form-check-label" ]
                    [ input
                        [ class "form-check-input"
                        , type_ "checkbox"
                        , onCheck <| tagger << Remember
                        , checked model.remember
                        ]
                        []
                    , text "Stay Signed In"
                    ]
                ]

        createAccountSection =
            fieldset []
                [ legend [] [ text "New Customers" ]
                , p [ class "px-1" ]
                    [ text "If you do not currently have a "
                    , b [] [ text "Southern Exposure Seed Exchange" ]
                    , text " account, you can create one to checkout "
                    , text "faster, review your order details, & take advantage "
                    , text "of our other membership benefits."
                    ]
                , a
                    [ class "btn btn-primary ml-auto mb-2"
                    , href <| reverse CreateAccount
                    ]
                    [ text "Create an Account" ]
                ]
    in
    [ h1 [] [ text "Please Sign In" ]
    , hr [] []
    , div [ class "row" ]
        [ div [ class "col-sm-6" ] [ loginForm ]
        , div [ class "col-sm-6" ] [ createAccountSection ]
        ]
    ]
