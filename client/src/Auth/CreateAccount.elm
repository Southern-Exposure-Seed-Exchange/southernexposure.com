module Auth.CreateAccount
    exposing
        ( Form
        , initial
        , Msg
        , update
        , view
        , successView
        )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, required, value, selected)
import Html.Events exposing (onInput, onSubmit, on, targetValue)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Navigation
import RemoteData exposing (WebData)
import PageData
import Ports
import User exposing (AuthStatus)
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
    , errors : Dict String (List String)
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
    , state = ""
    , zipCode = ""
    , country = "US"
    , phoneNumber = ""
    , errors = Dict.empty
    }


encode : Form -> Value
encode model =
    let
        encodedState =
            (,) "state" <|
                case model.country of
                    "US" ->
                        if List.member model.state [ "AA", "AE", "AP" ] then
                            stateWithKey "armedForces"
                        else
                            stateWithKey "state"

                    "CA" ->
                        stateWithKey "province"

                    _ ->
                        stateWithKey "custom"

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
    | SubmitResponse (WebData AuthStatus)


update : Msg -> Form -> ( Form, Maybe AuthStatus, Cmd Msg )
update msg form =
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
                        addError "passwordConfirm" "Passwords do not match." Dict.empty
                            |> addError "password" "Passwords do not match."
                  }
                , Nothing
                , Ports.scrollToID "form-errors-text"
                )
            else
                ( { form | errors = Dict.empty }, Nothing, createNewAccount form )

        -- TODO: Better error case handling/feedback
        SubmitResponse response ->
            case response of
                RemoteData.Success authStatus ->
                    ( form
                    , Just authStatus
                    , Navigation.newUrl <| reverse CreateAccountSuccess
                    )

                RemoteData.Failure (Http.BadStatus rawResponse) ->
                    if rawResponse.status.code == 422 then
                        case Decode.decodeString errorDecoder rawResponse.body of
                            Ok errors ->
                                ( { form | errors = errors }
                                , Nothing
                                , Ports.scrollToID "form-errors-text"
                                )

                            Err _ ->
                                form |> noCommandOrStatus
                    else
                        form |> noCommandOrStatus

                _ ->
                    form |> noCommandOrStatus


addError : String -> String -> Dict String (List String) -> Dict String (List String)
addError key error errors =
    flip (Dict.update key) errors <|
        \val ->
            case val of
                Nothing ->
                    Just [ error ]

                Just errs ->
                    Just (error :: errs)


errorDecoder : Decoder (Dict String (List String))
errorDecoder =
    Decode.dict <| Decode.list Decode.string


noCommandOrStatus : a -> ( a, Maybe b, Cmd msg )
noCommandOrStatus form =
    ( form, Nothing, Cmd.none )


createNewAccount : Form -> Cmd Msg
createNewAccount form =
    Http.post "/api/customers/register/"
        (encode form |> Http.jsonBody)
        User.decoder
        |> RemoteData.sendRequest
        |> Cmd.map SubmitResponse



-- VIEW
-- TODO: This was the first form validation so it's pretty ad-hoc, refactor,
-- maybe with a custom type for each Fields & functions to pull proper data out
-- of each field? Maybe wait til we have multiple validation forms and pull out
-- commonalities? Brainstorm a bit.


view : (Msg -> msg) -> Form -> PageData.LocationData -> List (Html msg)
view tagger model locations =
    let
        errorText =
            if Dict.isEmpty model.errors then
                text ""
            else
                div [ id "form-errors-text", class "alert alert-danger" ]
                    [ text <|
                        "There were issues processing your information, "
                            ++ "please correct any errors detailed below "
                            ++ "& resubmit the form."
                    ]

        requiredField =
            inputField True

        optionalField =
            inputField False

        inputField isRequired labelText inputType errorField msg =
            let
                inputId =
                    String.filter (\c -> c /= ' ') labelText

                fieldErrors =
                    Dict.get errorField model.errors
                        |> Maybe.withDefault []

                inputClass =
                    if List.isEmpty fieldErrors && not (Dict.isEmpty model.errors) then
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
                input
                    [ id <| "input" ++ inputId
                    , class inputClass
                    , type_ inputType
                    , required isRequired
                    , onInput <| tagger << msg
                    ]
                    []
                    |> (\i -> [ i, errorHtml ])
                    |> withLabel labelText inputId isRequired

        requiredHtml isRequired =
            if isRequired then
                span [ class "text-danger" ] [ text "*" ]
            else
                text ""

        withLabel labelText inputId isRequired input =
            div [ class "form-group form-row align-items-center" ]
                [ label
                    [ class "col-sm-3 col-form-label text-right font-weight-bold"
                    , for <| "input" ++ inputId
                    ]
                    [ requiredHtml isRequired, text " ", text <| labelText ++ ":" ]
                , div [ class "col" ] input
                ]

        countrySelect =
            List.map (locationToOption .country) locations.countries
                |> select
                    [ id "Country"
                    , class "form-control"
                    , onSelect <| tagger << Country
                    ]
                |> List.singleton
                |> withLabel "Country" "Country" True

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
                    input
                        [ id "Region"
                        , class "form-control"
                        , type_ "text"
                        , onInput <| tagger << State
                        ]
                        []
                        |> List.singleton
                        |> withLabel "State / Province" "Region" True

        regionSelect labelText =
            List.map (locationToOption .state)
                >> select
                    [ id labelText
                    , class "form-control"
                    , onSelect <| tagger << State
                    ]
                >> List.singleton
                >> withLabel labelText labelText True

        onSelect msg =
            targetValue
                |> Decode.map msg
                |> on "change"
    in
        [ h1 [] [ text "Create an Account" ]
        , hr [] []
        , errorText
        , form [ onSubmit <| tagger SubmitForm ]
            [ fieldset []
                [ legend [] [ text "Login Information" ]
                , requiredField "Email" "email" "email" Email
                , requiredField "Password" "password" "password" Password
                , requiredField "Confirm Password" "password" "passwordConfirm" PasswordConfirm
                ]
            , fieldset []
                [ legend [] [ text "Contact Information" ]
                , requiredField "First Name" "text" "firstName" FirstName
                , requiredField "Last Name" "text" "lastName" LastName
                , requiredField "Street Address" "text" "addressOne" Street
                , optionalField "Address Line 2" "text" "" AddressTwo
                , requiredField "City" "text" "city" City
                , regionField
                , requiredField "Zip Code" "text" "zipCode" ZipCode
                , countrySelect
                , requiredField "Phone Number" "tel" "telephone" PhoneNumber
                ]
            , div [ class "form-group clearfix" ]
                [ small [ class "font-weight-bold text-danger" ]
                    [ text "* Required Fields" ]
                , button [ class "btn btn-primary float-right", type_ "submit" ]
                    [ text "Register" ]
                ]
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
