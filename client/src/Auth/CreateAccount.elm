module Auth.CreateAccount exposing
    ( Form
    , Msg
    , initial
    , successView
    , update
    , view
    )

-- TODO: Refactor module as a composition of the EditLogin & EditContact module forms.

import Api
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, for, id, required, selected, type_, value)
import Html.Events exposing (on, onInput, onSubmit, targetValue)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Routing exposing (Route(..), reverse)
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.HorizontalForm as Form



-- MODEL


type alias Form =
    { email : String
    , password : String
    , passwordConfirm : String
    , errors : Api.FormErrors
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , passwordConfirm = ""
    , errors = Api.initialErrors
    }


encode : Form -> Maybe String -> Value
encode model maybeSessionToken =
    let
        encodedToken =
            maybeSessionToken
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
                |> Tuple.pair "sessionToken"
    in
    [ ( "email", model.email )
    , ( "password", model.password )
    ]
        |> List.map (Tuple.mapSecond Encode.string)
        |> (::) encodedToken
        |> Encode.object



-- UPDATE


type Msg
    = Email String
    | Password String
    | PasswordConfirm String
    | SubmitForm
    | SubmitResponse (WebData (Result Api.FormErrors AuthStatus))


update : Routing.Key -> Msg -> Form -> Maybe String -> ( Form, Maybe AuthStatus, Cmd Msg )
update key msg form maybeSessionToken =
    case msg of
        Email str ->
            { form | email = str }
                |> nothingAndNoCommand

        Password str ->
            { form | password = str }
                |> nothingAndNoCommand

        PasswordConfirm str ->
            { form | passwordConfirm = str }
                |> nothingAndNoCommand

        SubmitForm ->
            if form.password /= form.passwordConfirm then
                ( { form
                    | errors =
                        Api.initialErrors
                            |> Api.addError "passwordConfirm" "Passwords do not match."
                            |> Api.addError "password" "Passwords do not match."
                  }
                , Nothing
                , Ports.scrollToID "form-errors-text"
                )

            else
                ( { form | errors = Api.initialErrors }
                , Nothing
                , createNewAccount form maybeSessionToken
                )

        -- TODO: Better error case handling/feedback
        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok authStatus) ->
                    ( form
                    , Just authStatus
                    , Cmd.batch
                        [ Routing.newUrl key CreateAccountSuccess
                        , Ports.scrollToTop
                        , User.storeDetails authStatus
                        , Ports.removeCartSessionToken ()
                        ]
                    )

                RemoteData.Success (Err errors) ->
                    ( { form | errors = errors }
                    , Nothing
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    form |> nothingAndNoCommand


createNewAccount : Form -> Maybe String -> Cmd Msg
createNewAccount form maybeSessionToken =
    Api.post Api.CustomerRegister
        |> Api.withJsonBody (encode form maybeSessionToken)
        |> Api.withErrorHandler User.decoder
        |> Api.sendRequest SubmitResponse



-- VIEW


{-| TODO: This was the first form validation so it's pretty ad-hoc, refactor,
maybe with a custom type for each Fields & functions to pull proper data out
of each field? Maybe wait til we have multiple validation forms and pull out
commonalities? Brainstorm a bit.
-}
view : (Msg -> msg) -> Form -> List (Html msg)
view tagger model =
    let
        requiredField s msg =
            inputField s msg True

        inputField selector msg =
            Form.inputRow model.errors (selector model) (tagger << msg)
    in
    [ h1 [] [ text "Create an Account" ]
    , hr [] []
    , Form.genericErrorText (not <| Dict.isEmpty model.errors)
    , form [ onSubmit <| tagger SubmitForm ]
        [ fieldset []
            [ legend [] [ text "Login Information" ]
            , requiredField .email Email "Email" "email" "email" "email"
            , requiredField .password Password "Password" "password" "password" "new-password"
            , requiredField .passwordConfirm PasswordConfirm "Confirm Password" "passwordConfirm" "password" "new-password"
            ]
        , Form.submitButton "Register"
        ]
    ]


successView : List (Html msg)
successView =
    [ h1 [] [ text "Account Successfully Created" ]
    , hr [] []
    , p []
        [ text <|
            String.join " "
                [ "Congratulations, your new account has been successfully created!"
                , "A confirmation has been sent to your email address."
                ]
        ]
    ]
