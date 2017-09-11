module Auth.Login
    exposing
        ( Form
        , initial
        , Msg
        , update
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, href, required, autofocus, checked, value)
import Html.Events exposing (onInput, onCheck, onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http exposing (Error(BadStatus))
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)
import Auth.Utils exposing (noCommandOrStatus)
import Ports
import Routing exposing (Route(CreateAccount, PageDetails), reverse)
import User exposing (AuthStatus, UserId(..))


-- MODEL


type alias Form =
    { email : String
    , password : String
    , remember : Bool
    , error : String
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , remember = True
    , error = ""
    }


encode : Form -> Value
encode { email, password } =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]



-- UPDATE


type Msg
    = Email String
    | Password String
    | Remember Bool
    | CreateAccountPage
    | SubmitForm
    | SubmitResponse (WebData AuthStatus)


update : Msg -> Form -> ( Form, Maybe AuthStatus, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            { model | email = email }
                |> noCommandOrStatus

        Password password ->
            { model | password = password }
                |> noCommandOrStatus

        Remember remember ->
            { model | remember = remember }
                |> noCommandOrStatus

        CreateAccountPage ->
            ( model
            , Nothing
            , Cmd.batch
                [ Routing.newUrl CreateAccount
                , Ports.scrollToTop
                ]
            )

        SubmitForm ->
            ( { model | error = "" }, Nothing, login model )

        -- TODO: Better error case handling/feedback
        SubmitResponse response ->
            case response of
                RemoteData.Success authStatus ->
                    ( initial
                    , Just authStatus
                    , Cmd.batch
                        [ Routing.newUrl <| PageDetails "home"
                        , rememberAuth model.remember authStatus
                        ]
                    )

                RemoteData.Failure (BadStatus rawResponse) ->
                    if rawResponse.status.code == 422 then
                        case Decode.decodeString Decode.string rawResponse.body of
                            Ok error ->
                                ( { model | error = error }, Nothing, Ports.scrollToTop )

                            Err _ ->
                                debugResponse response model
                    else
                        debugResponse response model

                _ ->
                    debugResponse response model


rememberAuth : Bool -> AuthStatus -> Cmd msg
rememberAuth remember authStatus =
    case ( remember, authStatus ) of
        ( True, User.Authorized user ) ->
            let
                (UserId id) =
                    user.id
            in
                Ports.storeAuthDetails ( user.authToken, id )

        _ ->
            Cmd.none


debugResponse : c -> a -> ( a, Maybe b, Cmd msg )
debugResponse response model =
    let
        _ =
            Debug.log "Bad Response" response
    in
        model |> noCommandOrStatus


login : Form -> Cmd Msg
login form =
    Http.post "/api/customers/login/"
        (encode form |> Http.jsonBody)
        User.decoder
        |> RemoteData.sendRequest
        |> Cmd.map SubmitResponse



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
                    ]
                ]

        errorHtml =
            if String.isEmpty model.error then
                text ""
            else
                p [ class "text-danger font-weight-bold" ] [ text model.error ]

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
                    , onClickPreventDefault <| tagger CreateAccountPage
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
