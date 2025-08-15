module Pages.Login exposing
    ( Form
    , LoginStatus(..)
    , Msg
    , initial
    , loginDecoder
    , update
    , view
    )

import Data.Api as Api
import Browser.Navigation
import Components.Button as Button exposing (defaultButton)
import Components.Svg exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (autofocus, checked, class, for, href, id, required, target, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Data.Routing.Routing as Routing exposing (Route(..), reverse)
import Utils.Update exposing (nothingAndNoCommand)
import Data.User as User exposing (AuthStatus, UserId(..))
import Utils.View exposing (..)
import Html.Attributes exposing (placeholder)



-- MODEL


type alias Form =
    { email : String
    , password : String
    , remember : Bool
    , errors : Api.FormErrors
    , redirectTo : Maybe String
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , remember = True
    , errors = Api.initialErrors
    , redirectTo = Nothing
    }


encode : Form -> Maybe String -> Value
encode { email, password, remember } maybeSessionToken =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        , ( "sessionToken"
          , Maybe.map Encode.string maybeSessionToken
                |> Maybe.withDefault Encode.null
          )
        , ( "remember", Encode.bool remember )
        ]



-- UPDATE


type Msg
    = Email String
    | Password String
    | Remember Bool
    | SubmitForm (Maybe String) Bool
    | SubmitResponse (WebData (Result Api.FormErrors (LoginStatus AuthStatus)))


type LoginStatus a
    = LoginCompleted a
    | ErrVerificationRequired Int


loginDecoder : Decoder a -> Decoder (LoginStatus a)
loginDecoder innerDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                if status == "verification-required" then
                    Decode.map ErrVerificationRequired
                        (Decode.field "data" Decode.int)

                else
                    Decode.map LoginCompleted
                        (Decode.field "data" innerDecoder)
            )


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

        SubmitForm redirectTo clearCart ->
            ( { model | errors = Api.initialErrors, redirectTo = redirectTo }
            , Nothing
            , login model maybeSessionToken clearCart
            )

        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok (LoginCompleted authStatus)) ->
                    let
                        redirectCmd =
                            case model.redirectTo of
                                Nothing ->
                                    Routing.newUrl key Routing.homePage

                                Just "" ->
                                    Routing.newUrl key Routing.homePage

                                Just url ->
                                    Browser.Navigation.pushUrl key url
                    in
                    ( initial
                    , Just authStatus
                    , Cmd.batch
                        [ redirectCmd
                        , rememberAuth model.remember authStatus
                        , Ports.removeCartSessionToken ()
                        ]
                    )

                RemoteData.Success (Ok (ErrVerificationRequired customerId)) ->
                    ( initial
                    , Nothing
                    , Routing.newUrl key (VerificationRequired customerId)
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors }, Nothing, Ports.scrollToTop )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error }
                    , Nothing
                    , Ports.scrollToTop
                    )

                _ ->
                    nothingAndNoCommand model


rememberAuth : Bool -> AuthStatus -> Cmd msg
rememberAuth remember authStatus =
    if remember then
        User.storeDetails authStatus

    else
        Cmd.none


login : Form -> Maybe String -> Bool -> Cmd Msg
login form maybeSessionToken clearCart =
    Api.post (Api.CustomerLogin clearCart)
        |> Api.withJsonBody (encode form maybeSessionToken)
        |> Api.withErrorHandler (loginDecoder User.decoder)
        |> Api.sendRequest SubmitResponse



-- VIEW


view : (Msg -> msg) -> Form -> Maybe String -> Bool -> List (Html msg)
view tagger model redirectTo clearCart =
    let
        loginForm =
            form [ onSubmit <| tagger <| SubmitForm redirectTo clearCart ]
                [ fieldset [ class "" ]
                    [ legendView "Returning Customers"
                    , loginInputs
                    , errorHtml
                    , rememberCheckbox
                    , div [ class "tw:pb-[16px]" ] [ Button.view { defaultButton | label = "Login", type_ = Button.FormSubmit, padding = Button.Width "tw:w-[160px]" } ]
                    , div []
                        [ a
                            ([ class "tw:text-[14px] tw:opacity-60" ] ++ (routeLinkAttributes <| ResetPassword <| Just ""))
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
            div [ class "tw:p-[16px] tw:bg-[rgba(30,12,3,0.03)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px] tw:mb-[16px]" ]
                [ div [ class "" ]
                    [ labelView "emailInput" "Email Address" True
                    , input
                        [ id "emailInput"
                        , class "form-control"
                        , type_ "email"
                        , placeholder "Email Address"
                        , onInput <| tagger << Email
                        , value model.email
                        , required True
                        , autofocus True
                        , emailInput
                        , autocomplete "email"
                        ]
                        []
                    ]
                , div [ class "" ]
                    [ labelView "passwordInput" "Password" True
                    , input
                        [ id "passwordInput"
                        , class "form-control"
                        , type_ "password"
                        , placeholder "Password"
                        , onInput <| tagger << Password
                        , value model.password
                        , required True
                        , autocomplete "current-password"
                        ]
                        []
                    ]
                ]

        rememberCheckbox =
            div [ class "form-check tw:pb-[16px]" ]
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
                [ legendView "New Customers"
                , div [ class "tw:pb-[16px]" ]
                    [ p [ class "tw:p-[16px] tw:rounded-[16px] tw:bg-[rgba(167,215,197,0.1)]" ]
                        [ text "If you do not currently have a "
                        , b [] [ text "Southern Exposure Seed Exchange" ]
                        , text " account, you can create one to checkout "
                        , text "faster, review your order details, & take advantage "
                        , text "of our other membership benefits."
                        ]
                    ]
                , div [ class "tw:flex" ]
                    [ Button.view { defaultButton | label = "Create an account", style = Button.Outline, type_ = Button.Link <| reverse CreateAccount }
                    ]
                ]

        seedracksSection =
            p [ class "tw:mb-0! tw:whitespace-pre-line" ]
                [ text "Looking to resell our packets?\nPlease login to our Seed Racks/Wholesale webstore at "
                , a [ target "_blank", href "https://seedracks.southernexposure.com" ]
                    [ text "seedracks.southernexposure.com" ]
                , text "."
                ]
    in
    [ pageTitleView "Sign in"
    , div [ class "tw:flex tw:grid tw:grid-cols-2 tw:gap-[40px] tw:pb-[60px]" ]
        [ loginForm
        , createAccountSection
        ]
    , div [ class "tw:bg-[rgba(167,215,197,0.1)] tw:rounded-[16px] tw:p-[16px] static-page tw:flex tw:gap-[12px]" ]
        [ resellSvg
        , seedracksSection
        ]
    ]
