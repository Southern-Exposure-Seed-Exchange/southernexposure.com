module Auth.Login
    exposing
        ( Form
        , initial
        , Msg
        , update
        , view
        )

import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, href, required)
import Html.Events exposing (onInput, onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http exposing (Error(BadStatus))
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)
import Auth.Utils exposing (noCommandOrStatus)
import Ports
import Routing exposing (Route(CreateAccount, PageDetails), reverse)
import User exposing (AuthStatus)


-- MODEL


type alias Form =
    { email : String
    , password : String
    , error : String
    }


initial : Form
initial =
    { email = ""
    , password = ""
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
                    ( initial, Just authStatus, Routing.newUrl <| PageDetails "home" )

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
                    , required True
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
                    , required True
                    ]
                    []
                ]
            ]
                |> div []

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
