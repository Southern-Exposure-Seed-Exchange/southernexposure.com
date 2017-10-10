module Address
    exposing
        ( Model
        , initial
        , decode
        , Form
        , initialForm
        , encode
        , Msg
        , update
        , card
        , form
        )

import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, id, class, for, type_, required, value, name, selected)
import Html.Events exposing (onInput, on, targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Api
import Locations exposing (Region(..), regionDecoder, regionEncoder, Location, AddressLocations)


-- Model


type alias Model =
    { firstName : String
    , lastName : String
    , companyName : String
    , street : String
    , addressTwo : String
    , city : String
    , state : Region
    , zipCode : String
    , country : String
    }


initial : Model
initial =
    { firstName = ""
    , lastName = ""
    , companyName = ""
    , street = ""
    , addressTwo = ""
    , city = ""
    , state = USState "AL"
    , zipCode = ""
    , country = "US"
    }


decode : Decoder Model
decode =
    Decode.map8 Model
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "companyName" Decode.string)
        (Decode.field "addressOne" Decode.string)
        (Decode.field "addressTwo" Decode.string)
        (Decode.field "city" Decode.string)
        (Decode.field "state" regionDecoder)
        (Decode.field "zipCode" Decode.string)
        |> Decode.andThen
            (\constr ->
                Decode.map constr
                    (Decode.field "country" Decode.string)
            )


type alias Form =
    { model : Model
    , errors : Api.FormErrors
    }


initialForm : Form
initialForm =
    { model = initial
    , errors = Api.initialErrors
    }


encode : Form -> Value
encode { model } =
    let
        encodedState =
            regionEncoder model.state
    in
        [ ( "firstName", model.firstName )
        , ( "lastName", model.lastName )
        , ( "companyName", model.companyName )
        , ( "addressOne", model.street )
        , ( "addressTwo", model.addressTwo )
        , ( "city", model.city )
        , ( "zipCode", model.zipCode )
        , ( "country", model.country )
        ]
            |> List.map (Tuple.mapSecond Encode.string)
            |> ((::) ( "state", encodedState ))
            |> Encode.object



-- Update


type Msg
    = FirstName String
    | LastName String
    | CompanyName String
    | Street String
    | AddressTwo String
    | City String
    | State String
    | ZipCode String
    | Country String


update : Msg -> Form -> Form
update msg ({ model } as f) =
    { f | model = updateModel msg model }


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        FirstName str ->
            { model | firstName = str }

        LastName str ->
            { model | lastName = str }

        CompanyName str ->
            { model | companyName = str }

        Street str ->
            { model | street = str }

        AddressTwo str ->
            { model | addressTwo = str }

        City str ->
            { model | city = str }

        State str ->
            case model.country of
                "US" ->
                    if List.member str Locations.armedForcesCodes then
                        { model | state = ArmedForces str }
                    else
                        { model | state = USState str }

                "CA" ->
                    { model | state = CAProvince str }

                _ ->
                    { model | state = Custom str }

        ZipCode str ->
            { model | zipCode = str }

        Country str ->
            let
                newRegion =
                    if str == "US" then
                        USState "AL"
                    else if str == "CA" then
                        CAProvince "AB"
                    else if model.country == "CA" || model.country == "US" then
                        Custom ""
                    else
                        model.state
            in
                if model.country /= str then
                    { model | country = str, state = newRegion }
                else
                    model



-- View


card : Model -> AddressLocations -> Html msg
card address locations =
    let
        notBlank str =
            if not (String.isEmpty str) then
                Just <| text str
            else
                Nothing

        stateString =
            Maybe.withDefault "" <|
                case address.state of
                    USState code ->
                        findLocation code locations.states

                    ArmedForces code ->
                        findLocation code locations.armedForces

                    CAProvince code ->
                        findLocation code locations.provinces

                    Custom str ->
                        Just str

        countryHtml =
            if address.country == "US" then
                Nothing
            else
                findLocation address.country locations.countries
                    |> Maybe.map text

        findLocation code ls =
            case ls of
                [] ->
                    Nothing

                x :: xs ->
                    if x.code == code then
                        Just x.name
                    else
                        findLocation code xs
    in
        [ Just <| b [] [ text <| address.firstName ++ " " ++ address.lastName ]
        , notBlank address.companyName
        , Just <| text address.street
        , notBlank address.addressTwo
        , Just <| text <| address.city ++ ", " ++ stateString ++ " " ++ address.zipCode
        , countryHtml
        ]
            |> List.filterMap identity
            |> List.intersperse (br [] [])
            |> Html.address []


form : Form -> String -> AddressLocations -> Html Msg
form { model, errors } inputPrefix locations =
    let
        field selector =
            inputField errors inputPrefix (selector model)

        selectField selector =
            locationSelect inputPrefix (selector model)

        regionField =
            case model.country of
                "US" ->
                    selectField stateCode
                        State
                        (locations.states ++ locations.armedForces)
                        "State"
                        "state"

                "CA" ->
                    selectField stateCode State locations.provinces "Province" "state"

                _ ->
                    field stateCode State "State / Province" "state" "address-level1" True

        stateCode =
            .state >> Locations.fromRegion
    in
        div []
            [ field .firstName FirstName "First Name" "firstName" "given-name" True
            , field .lastName LastName "Last Name" "lastName" "family-name" True
            , field .companyName CompanyName "Company Name" "companyName" "organization" False
            , field .street Street "Street Address" "addressOne" "street-address" True
            , field .addressTwo AddressTwo "Address Line 2" "addressTwo" "address-line2" False
            , field .city City "City" "address-level2" "on" True
            , regionField
            , field .zipCode ZipCode "Zip Code" "zipCode" "postal-code" True
            , selectField .country Country locations.countries "Country" "country"
            ]


{-| TODO: Refactor into separate Views.Form module
-}
inputField : Api.FormErrors -> String -> String -> (String -> msg) -> String -> String -> String -> Bool -> Html msg
inputField errors prefix inputValue inputMsg labelText inputName autocompleteType isRequired =
    let
        fieldLabel =
            label
                [ class "mb-0"
                , for <| prefix ++ "-" ++ inputName ++ "Input"
                ]
                [ text labelText
                , if not isRequired then
                    small [] [ text " (optional)" ]
                  else
                    text ""
                ]

        autocomplete str =
            attribute "autocomplete" <| prefix ++ " " ++ str

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""
            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

        fieldErrors =
            Dict.get inputName errors |> Maybe.withDefault []
    in
        div [ class "form-group mb-2" ]
            [ fieldLabel
            , input
                [ id <| prefix ++ "-" ++ inputName ++ "Input"
                , class "form-control"
                , name inputName
                , type_ "text"
                , required isRequired
                , onInput inputMsg
                , value inputValue
                , autocomplete autocompleteType
                ]
                []
            , errorHtml
            ]


{-| TODO: Refactor into Locations module? Or Views.Form if can't combine w/ HorizontalForm.
-}
locationSelect : String -> String -> (String -> msg) -> List Location -> String -> String -> Html msg
locationSelect prefix selectedValue selectMsg locations labelText inputName =
    let
        fieldLabel =
            label [ class "mb-0", for inputId ]
                [ text labelText ]

        inputId =
            prefix ++ "-" ++ inputName ++ "Input"

        toOption { code, name } =
            option [ value code, selected <| selectedValue == code ] [ text name ]

        onSelect =
            on "change" (targetValue |> Decode.map selectMsg)
    in
        div [ class "form-group mb-2" ]
            [ fieldLabel
            , List.map toOption locations
                |> select [ id inputId, class "form-control", onSelect ]
            ]
