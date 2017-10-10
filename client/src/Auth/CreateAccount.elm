module Auth.CreateAccount
    exposing
        ( Form
        , initial
        , Msg
        , update
        , view
        , successView
        )

-- TODO: Refactor module as a composition of the EditLogin & EditContact module forms.

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, required, value, selected)
import Html.Events exposing (onInput, onSubmit, on, targetValue)
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)
import Api
import Auth.Utils exposing (noCommandOrStatus)
import Locations exposing (AddressLocations)
import Ports
import User exposing (AuthStatus)
import Views.HorizontalForm as Form
import Routing exposing (Route(CreateAccountSuccess), reverse)


-- MODEL


type alias Form =
    { email : String
    , password : String
    , passwordConfirm : String
    , firstName : String
    , lastName : String
    , street : String
    , addressTwo : String
    , city : String
    , state : String
    , zipCode : String
    , country : String
    , phoneNumber : String
    , errors : Api.FormErrors
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , passwordConfirm = ""
    , firstName = ""
    , lastName = ""
    , street = ""
    , addressTwo = ""
    , city = ""
    , state = "AL"
    , zipCode = ""
    , country = "US"
    , phoneNumber = ""
    , errors = Api.initialErrors
    }


encode : Form -> Maybe String -> Value
encode model maybeSessionToken =
    let
        encodedState =
            (,) "state" <|
                case model.country of
                    "US" ->
                        if List.member model.state Locations.armedForcesCodes then
                            stateWithKey "armedForces"
                        else
                            stateWithKey "state"

                    "CA" ->
                        stateWithKey "province"

                    _ ->
                        stateWithKey "custom"

        encodedToken =
            maybeSessionToken
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
                |> ((,) "sessionToken")

        stateWithKey key =
            Encode.object [ ( key, Encode.string model.state ) ]
    in
        [ ( "email", model.email )
        , ( "password", model.password )
        , ( "firstName", model.firstName )
        , ( "lastName", model.lastName )
        , ( "addressOne", model.street )
        , ( "addressTwo", model.addressTwo )
        , ( "city", model.city )
        , ( "zipCode", model.zipCode )
        , ( "country", model.country )
        , ( "telephone", model.phoneNumber )
        ]
            |> List.map (Tuple.mapSecond Encode.string)
            |> ((::) encodedState)
            |> ((::) encodedToken)
            |> Encode.object



-- UPDATE


type Msg
    = Email String
    | Password String
    | PasswordConfirm String
    | FirstName String
    | LastName String
    | Street String
    | AddressTwo String
    | City String
    | State String
    | ZipCode String
    | Country String
    | PhoneNumber String
    | SubmitForm
    | SubmitResponse (WebData (Result Api.FormErrors AuthStatus))


update : Msg -> Form -> Maybe String -> ( Form, Maybe AuthStatus, Cmd Msg )
update msg form maybeSessionToken =
    case msg of
        Email str ->
            { form | email = str }
                |> noCommandOrStatus

        Password str ->
            { form | password = str }
                |> noCommandOrStatus

        PasswordConfirm str ->
            { form | passwordConfirm = str }
                |> noCommandOrStatus

        FirstName str ->
            { form | firstName = str }
                |> noCommandOrStatus

        LastName str ->
            { form | lastName = str }
                |> noCommandOrStatus

        Street str ->
            { form | street = str }
                |> noCommandOrStatus

        AddressTwo str ->
            { form | addressTwo = str }
                |> noCommandOrStatus

        City str ->
            { form | city = str }
                |> noCommandOrStatus

        State str ->
            { form | state = str }
                |> noCommandOrStatus

        ZipCode str ->
            { form | zipCode = str }
                |> noCommandOrStatus

        Country str ->
            { form | country = str }
                |> noCommandOrStatus

        PhoneNumber str ->
            { form | phoneNumber = str }
                |> noCommandOrStatus

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
                        [ Routing.newUrl CreateAccountSuccess
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
                    form |> noCommandOrStatus


createNewAccount : Form -> Maybe String -> Cmd Msg
createNewAccount form maybeSessionToken =
    Api.post Api.CustomerRegister
        |> Api.withJsonBody (encode form maybeSessionToken)
        |> Api.withJsonResponse User.decoder
        |> Api.withErrorHandler SubmitResponse



-- VIEW
-- TODO: This was the first form validation so it's pretty ad-hoc, refactor,
-- maybe with a custom type for each Fields & functions to pull proper data out
-- of each field? Maybe wait til we have multiple validation forms and pull out
-- commonalities? Brainstorm a bit.


view : (Msg -> msg) -> Form -> AddressLocations -> List (Html msg)
view tagger model locations =
    let
        requiredField s msg =
            inputField s msg True

        optionalField s msg =
            inputField s msg False

        inputField selector msg =
            Form.inputRow model.errors (selector model) (tagger << msg)

        selectRow msg =
            Form.selectRow Ok (tagger << msg)

        countrySelect =
            List.map (locationToOption .country) locations.countries
                |> selectRow Country "Country" True

        locationToOption selector { code, name } =
            option [ value code, selected <| selector model == code ]
                [ text name ]

        regionField =
            case model.country of
                "US" ->
                    regionSelect "State" (locations.states ++ locations.armedForces)

                "CA" ->
                    regionSelect "Province" locations.provinces

                _ ->
                    requiredField .state State "State / Province" "state" "state"

        regionSelect labelText =
            List.map (locationToOption .state) >> selectRow State labelText True
    in
        [ h1 [] [ text "Create an Account" ]
        , hr [] []
        , Form.genericErrorText (not <| Dict.isEmpty model.errors)
        , form [ onSubmit <| tagger SubmitForm ]
            [ fieldset []
                [ legend [] [ text "Login Information" ]
                , requiredField .email Email "Email" "email" "email"
                , requiredField .password Password "Password" "password" "password"
                , requiredField .passwordConfirm PasswordConfirm "Confirm Password" "passwordConfirm" "password"
                ]
            , fieldset []
                [ legend [] [ text "Contact Information" ]
                , requiredField .firstName FirstName "First Name" "firstName" "text"
                , requiredField .lastName LastName "Last Name" "lastName" "text"
                , requiredField .street Street "Street Address" "addressOne" "text"
                , optionalField .addressTwo AddressTwo "Address Line 2" "addressTwo" "text"
                , requiredField .city City "City" "city" "text"
                , regionField
                , requiredField .zipCode ZipCode "Zip Code" "zipCode" "text"
                , countrySelect
                , requiredField .phoneNumber PhoneNumber "Phone Number" "telephone" "tel"
                ]
            , Form.submitButton "Register" True
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
