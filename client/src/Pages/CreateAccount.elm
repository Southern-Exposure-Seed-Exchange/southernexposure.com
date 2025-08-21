module Pages.CreateAccount exposing
    ( Form
    , Msg
    , initial
    , successView
    , update
    , view
    )

-- TODO: Refactor module as a composition of the EditLogin & EditContact module forms.

import Components.Button as Button exposing (defaultButton)
import Components.HorizontalForm as Form
import Data.Api as Api
import Data.Routing.Routing as Routing exposing (Route(..), reverse)
import Data.User as User exposing (AuthStatus)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Utils.Decode as Decode
import Utils.Update exposing (nothingAndNoCommand)
import Utils.View exposing (legendView, pageTitleView)



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
    | SubmitResponse (WebData (Result Api.FormErrors ()))


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
                , Ports.scrollToErrorMessage
                )

            else
                ( { form | errors = Api.initialErrors }
                , Nothing
                , createNewAccount form maybeSessionToken
                )

        -- TODO: Better error case handling/feedback
        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( form
                    , Nothing
                    , Cmd.batch
                        [ Routing.newUrl key CreateAccountSuccess
                        , Ports.scrollToTop
                        , Ports.removeCartSessionToken ()
                        ]
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


createNewAccount : Form -> Maybe String -> Cmd Msg
createNewAccount form maybeSessionToken =
    Api.post Api.CustomerRegister
        |> Api.withJsonBody (encode form maybeSessionToken)
        |> Api.withErrorHandler Decode.unit
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

        signupForm =
            form [ onSubmit <| tagger SubmitForm ]
                [ fieldset []
                    [ legendView "New Customers"
                    , signupInputs
                    , Form.genericErrorText (not <| Dict.isEmpty model.errors)
                    , Api.getErrorHtml "" model.errors
                    , div [ class "tw:pb-[16px]" ] [ Button.view { defaultButton | label = "Register", type_ = Button.FormSubmit, padding = Button.Width "tw:w-full tw:lg:w-[160px]" } ]
                    ]
                ]

        signupInputs =
            div [ class "tw:p-[16px] tw:bg-[rgba(30,12,3,0.03)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px] tw:mb-[16px]" ]
                [ requiredField .email Email "Email" "email" "email" "email"
                , requiredField .password Password "Password" "password" "password" "new-password"
                , requiredField .passwordConfirm PasswordConfirm "Confirm Password" "passwordConfirm" "password" "new-password"
                ]

        welcomeBackSection =
            fieldset []
                [ legendView "Returning Customers"
                , div [ class "tw:pb-[16px]" ]
                    [ p [ class "tw:p-[16px] tw:rounded-[16px] tw:bg-[rgba(167,215,197,0.1)]" ]
                        [ text "Welcome back! Log in to continue shopping, view your past orders, and manage your saved preferences with ease."
                        ]
                    ]
                , div [ class "tw:flex" ]
                    [ Button.view { defaultButton | label = "Log in", style = Button.Outline, type_ = Button.Link <| reverse <| Login Nothing False, padding = Button.Width "tw:w-full tw:lg:px-[16px] tw:lg:w-auto" }
                    ]
                ]
    in
    [ pageTitleView "Create an Account"
    , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[40px] tw:pb-[60px]" ]
        [ signupForm
        , welcomeBackSection
        ]
    ]


successView : List (Html msg)
successView =
    [ pageTitleView "Verify your email address"
    , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2" ]
        [ p [ class "tw:p-[16px] tw:bg-[rgba(167,215,197,0.1)] tw:rounded-[16px]" ]
            [ text <|
                String.join " "
                    [ "One last step to complete your registration:"
                    , "please check your email inbox and follow the verification link we've sent you."
                    , "You can safely close this page now."
                    ]
            ]
        ]
    ]
