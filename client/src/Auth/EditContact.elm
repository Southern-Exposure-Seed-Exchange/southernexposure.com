module Auth.EditContact
    exposing
        ( Form
        , initial
        , fromContactDetails
        , Msg
        , update
        , view
        )

import Dict
import Html exposing (..)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Encode exposing (encode)
import RemoteData exposing (WebData)
import Api
import Views.HorizontalForm as Form
import PageData exposing (ContactDetails, Region(..))
import Ports
import Routing exposing (Route(MyAccount))
import Update.Utils exposing (noCommand)
import User exposing (AuthStatus(..))


-- MODEL


type alias Form =
    { details : PageData.ContactDetails
    , errors : Api.FormErrors
    }


initial : Form
initial =
    let
        initialDetails =
            { firstName = ""
            , lastName = ""
            , street = ""
            , addressTwo = ""
            , city = ""
            , state = USState "AL"
            , zipCode = ""
            , country = "US"
            , phoneNumber = ""
            }
    in
        fromContactDetails initialDetails


fromContactDetails : ContactDetails -> Form
fromContactDetails contact =
    { details = contact
    , errors = Api.initialErrors
    }



-- UPDATE


type FieldMsg
    = FirstName String
    | LastName String
    | Street String
    | AddressTwo String
    | City String
    | State Region
    | ZipCode String
    | Country String
    | PhoneNumber String


type Msg
    = Field FieldMsg
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors ()))


updateDetails : FieldMsg -> PageData.ContactDetails -> PageData.ContactDetails
updateDetails msg model =
    case msg of
        FirstName str ->
            { model | firstName = str }

        LastName str ->
            { model | lastName = str }

        Street str ->
            { model | street = str }

        AddressTwo str ->
            { model | addressTwo = str }

        City str ->
            { model | city = str }

        State region ->
            { model | state = region }

        ZipCode str ->
            { model | zipCode = str }

        Country str ->
            { model | country = str }

        PhoneNumber str ->
            { model | phoneNumber = str }


update : Msg -> Form -> AuthStatus -> ( Form, Cmd Msg )
update msg model authStatus =
    case msg of
        Field subMsg ->
            { model | details = updateDetails subMsg model.details }
                |> noCommand

        Submit ->
            ( model, updateContactDetails model.details authStatus )

        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok _) ->
                    ( initial
                    , Cmd.batch [ Routing.newUrl <| MyAccount, Ports.scrollToTop ]
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors }
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    model |> noCommand


updateContactDetails : PageData.ContactDetails -> AuthStatus -> Cmd Msg
updateContactDetails details authStatus =
    case authStatus of
        Authorized user ->
            Api.post Api.CustomerEditContact
                |> Api.withToken user.authToken
                |> Api.withJsonBody (PageData.contactDetailsEncoder details)
                |> Api.withJsonResponse (Decode.succeed ())
                |> Api.withErrorHandler SubmitResponse

        Anonymous ->
            Cmd.none



-- VIEW


view : (Msg -> msg) -> Form -> PageData.LocationData -> List (Html msg)
view tagger { details, errors } locations =
    let
        inputRow selector msg =
            Form.inputRow errors (selector details) (tagger << Field << msg)

        selectRow parser msg =
            Form.selectRow parser (tagger << Field << msg)

        countryField =
            List.map countryOption locations.countries
                |> selectRow Ok Country "Country" True

        countryOption { code, name } =
            option [ value code, selected <| code == details.country ] [ text name ]

        regionField =
            case details.country of
                "US" ->
                    regionSelect "State" <| locations.states ++ locations.armedForces

                "CA" ->
                    regionSelect "Province" locations.provinces

                _ ->
                    inputRow (.state >> regionToCode)
                        (Custom >> State)
                        True
                        "State / Province"
                        "text"
                        "state"

        regionSelect labelText =
            List.map regionOption
                >> selectRow (Decode.decodeString PageData.regionDecoder)
                    State
                    labelText
                    True

        regionOption { code, name } =
            option
                [ value
                    << encode 0
                    << PageData.regionEncoder
                  <|
                    locationCodeToRegion code
                , selected <| code == regionToCode details.state
                ]
                [ text name ]

        locationCodeToRegion code =
            case details.country of
                "US" ->
                    if List.member code [ "AA", "AE", "AP" ] then
                        ArmedForces code
                    else
                        USState code

                "CA" ->
                    CAProvince code

                _ ->
                    Custom code

        regionToCode region =
            case region of
                USState c ->
                    c

                ArmedForces c ->
                    c

                CAProvince c ->
                    c

                Custom c ->
                    c
    in
        [ h1 []
            [ text
                "Edit Contact Details"
            ]
        , hr [] []
        , form [ onSubmit <| tagger Submit ]
            [ Form.genericErrorText (not <| Dict.isEmpty errors)
            , inputRow .firstName FirstName True "First Name" "firstName" "text"
            , inputRow .lastName LastName True "Last Name" "lastName" "text"
            , inputRow .street Street True "Street" "addressOne" "text"
            , inputRow .addressTwo AddressTwo False "Address Line 2" "addressTwo" "text"
            , inputRow .city City True "City" "city" "text"
            , regionField
            , inputRow .zipCode ZipCode True "Zip Code" "zipCode" "text"
            , countryField
            , inputRow .phoneNumber PhoneNumber True "Phone Number" "phoneNumber" "text"
            , Form.submitButton "Update" True
            ]
        ]
