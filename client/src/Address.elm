module Address exposing
    ( AddressId(..)
    , Form
    , Model
    , Msg
    , card
    , decoder
    , encode
    , form
    , fromModel
    , horizontalForm
    , initial
    , initialForm
    , select
    , update
    )

import Api
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, classList, for, id, name, required, selected, type_, value)
import Html.Events exposing (on, onInput, targetValue)
import Html.Events.Extra exposing (onChange)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Locations exposing (AddressLocations, Location, Region(..), regionDecoder, regionEncoder)
import Views.Aria as Aria
import Views.HorizontalForm as Form
import Views.Utils exposing (autocomplete)
import Views.Utils exposing (labelView)



-- Model


type AddressId
    = AddressId Int


type alias Model =
    { id : Maybe AddressId
    , firstName : String
    , lastName : String
    , companyName : String
    , street : String
    , addressTwo : String
    , city : String
    , state : Maybe Region
    , zipCode : String
    , country : String
    , phoneNumber : String
    , isDefault : Bool
    }


initial : Model
initial =
    { id = Nothing
    , firstName = ""
    , lastName = ""
    , companyName = ""
    , street = ""
    , addressTwo = ""
    , city = ""
    , state = Nothing
    , zipCode = ""
    , country = "US"
    , phoneNumber = ""
    , isDefault = True
    }


decoder : Decoder Model
decoder =
    Decode.map8 Model
        (Decode.field "id" <| Decode.map (Just << AddressId) Decode.int)
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.field "companyName" Decode.string)
        (Decode.field "addressOne" Decode.string)
        (Decode.field "addressTwo" Decode.string)
        (Decode.field "city" Decode.string)
        (Decode.field "state" <| Decode.map Just regionDecoder)
        |> Decode.andThen
            (\constr ->
                Decode.map4 constr
                    (Decode.field "zipCode" Decode.string)
                    (Decode.field "country" Decode.string)
                    (Decode.field "phoneNumber" Decode.string)
                    (Decode.field "isDefault" Decode.bool)
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


fromModel : Model -> Form
fromModel model =
    { initialForm | model = model }


encode : Form -> Value
encode { model } =
    let
        encodedState =
            regionEncoder <|
                case model.state of
                    Nothing ->
                        case model.country of
                            "US" ->
                                USState "AL"

                            "CA" ->
                                USState "AB"

                            _ ->
                                Custom ""

                    Just region ->
                        region
    in
    [ ( "firstName", model.firstName )
    , ( "lastName", model.lastName )
    , ( "companyName", model.companyName )
    , ( "addressOne", model.street )
    , ( "addressTwo", model.addressTwo )
    , ( "city", model.city )
    , ( "zipCode", model.zipCode )
    , ( "country", model.country )
    , ( "phoneNumber", model.phoneNumber )
    ]
        |> List.map (Tuple.mapSecond Encode.string)
        |> (::)
            ( "id"
            , Maybe.withDefault Encode.null <|
                Maybe.map (\(AddressId i) -> Encode.int i) model.id
            )
        |> (::) ( "state", encodedState )
        |> (::) ( "isDefault", Encode.bool model.isDefault )
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
    | PhoneNumber String


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
                        { model | state = Just <| ArmedForces str }

                    else
                        { model | state = Just <| USState str }

                "CA" ->
                    { model | state = Just <| CAProvince str }

                _ ->
                    { model | state = Just <| Custom str }

        ZipCode str ->
            { model | zipCode = str }

        Country str ->
            if model.country /= str then
                { model | country = str, state = Nothing }

            else
                model

        PhoneNumber str ->
            { model | phoneNumber = str }



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
            address.state
                |> Maybe.andThen (Locations.regionName locations)
                |> Maybe.withDefault ""

        countryHtml =
            if address.country == "US" then
                Nothing

            else
                Locations.findName locations.countries address.country
                    |> Maybe.map text
    in
    [ Just <| b [] [ text <| address.firstName ++ " " ++ address.lastName ]
    , notBlank address.companyName
    , Just <| text address.street
    , notBlank address.addressTwo
    , Just <| text <| address.city ++ ", " ++ stateString ++ " " ++ address.zipCode
    , countryHtml
    , Just <| text address.phoneNumber
    ]
        |> List.filterMap identity
        |> List.intersperse (br [] [])
        |> Html.address [class ""]


{-| TODO: Add Region to description
-}
select : (Int -> msg) -> Maybe AddressId -> List Model -> Bool -> Html msg
select selectMsg maybeAddressId addresses newAddressOption =
    let
        isSelected addr =
            selected <| addr.id == maybeAddressId

        fromAddressId a =
            case a of
                Just (AddressId i) ->
                    i

                _ ->
                    0

        addressDescription addr =
            [ b [] [ text <| addr.firstName ++ " " ++ addr.lastName ]
            , text " - "
            , text <|
                String.join ", " <|
                    List.filter (not << String.isEmpty) <|
                        [ addr.companyName
                        , addr.street
                        , addr.addressTwo
                        , addr.city
                        , addr.zipCode ++ " " ++ addr.country
                        ]
            ]

        onSelectInt msg =
            targetValue
                |> Decode.andThen
                    (String.toInt
                        >> Maybe.map Decode.succeed
                        >> Maybe.withDefault (Decode.fail "")
                    )
                |> Decode.map msg
                |> on "change"

        addNewOption options =
            if newAddressOption then
                option [ selected (Just (AddressId 0) == maybeAddressId), value "0" ]
                    [ text "Add a New Address..." ]
                    :: options

            else
                option [ selected <| Nothing == maybeAddressId, value "0" ]
                    [ text "Select an Address..." ]
                    :: options
    in
    addresses
        |> List.map
            (\a ->
                option
                    [ value <| String.fromInt <| fromAddressId a.id
                    , isSelected a
                    ]
                    (addressDescription a)
            )
        |> addNewOption
        |> Html.select [ class "form-control", onSelectInt selectMsg, Aria.label "Select an Existing Address" ]


form : Form -> String -> AddressLocations -> Html Msg
form { model, errors } inputPrefix locations =
    let
        generalErrors =
            Api.getErrorHtml "" errors

        field selector =
            inputField errors inputPrefix (selector model)

        selectField selector =
            locationSelect errors inputPrefix (selector model)

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
                    field stateCode (ChangeMsg State) "State / Province" "state" "address-level1" True

        stateCode =
            .state >> Maybe.map Locations.fromRegion >> Maybe.withDefault ""
    in
    div []
        [ generalErrors
        , field .firstName (InputMsg FirstName) "First Name" "firstName" "given-name" True
        , field .lastName (InputMsg LastName) "Last Name" "lastName" "family-name" True
        , field .companyName (InputMsg CompanyName) "Company Name" "companyName" "organization" False
        , field .street (ChangeMsg Street) "Street Address" "addressOne" "address-line1" True
        , field .addressTwo (InputMsg AddressTwo) "Address Line 2" "addressTwo" "address-line2" False
        , field .city (InputMsg City) "City" "city" "address-level2" True
        , regionField
        , field .zipCode (ChangeMsg ZipCode) "Zip Code" "zipCode" "postal-code" True
        , selectField .country Country locations.countries "Country" "country"
        , field .phoneNumber (InputMsg PhoneNumber) "Phone Number" "phoneNumber" "tel" True
        ]


horizontalForm : Form -> AddressLocations -> List (Html Msg)
horizontalForm { model, errors } locations =
    let
        requiredField s msg =
            textField s msg True

        optionalField s msg =
            textField s msg False

        textField selector =
            Form.inputRow errors (selector model)

        selectCol =
            Form.selectCol Ok

        countrySelect =
            List.map (locationToOption (Just << .country)) locations.countries
                |> selectCol Country "Country" True

        locationToOption selector { code, name } =
            option [ value code, selected <| selector model == Just code ]
                [ text name ]

        regionField =
            case model.country of
                "US" ->
                    regionSelect "State" (locations.states ++ locations.armedForces)

                "CA" ->
                    regionSelect "Province" locations.provinces

                _ ->
                    requiredField (stateCode >> Maybe.withDefault "") State "State / Province" "state" "state" "address-level1"

        regionSelect labelText =
            List.map (locationToOption stateCode) >> selectCol State labelText True

        stateCode =
            .state >> Maybe.map Locations.fromRegion
    in
    [ Api.getErrorHtml "" errors
    , div [ class "tw:grid tw:grid-cols-2 tw:gap-[16px]" ]
        [ requiredField .firstName FirstName "First Name" "firstName" "text" "given-name"
        , requiredField .lastName LastName "Last Name" "lastName" "text" "family-name"
        ]
    , requiredField .street Street "Street Address" "addressOne" "text" "address-line1"
    , optionalField .addressTwo AddressTwo "Address Line 2" "addressTwo" "text" "address-line2"
    , div [ class "tw:grid tw:grid-cols-2 tw:gap-[16px]" ]
        [ countrySelect
        , regionField
        ]
    , div [ class "tw:grid tw:grid-cols-2 tw:gap-[16px]" ]
        [ requiredField .city City "City" "city" "text" "address-level2"
        , requiredField .zipCode ZipCode "Zip Code" "zipCode" "text" "postal-code"
        ]
    , requiredField .phoneNumber PhoneNumber "Phone Number" "phoneNumber" "tel" "tel"
    ]


type FieldMsg msg
    = InputMsg (String -> msg)
    | ChangeMsg (String -> msg)


{-| TODO: Refactor into separate Views.Form module. Turn parameters into a Config type.
-}
inputField : Api.FormErrors -> String -> String -> FieldMsg msg -> String -> String -> String -> Bool -> Html msg
inputField errors prefix inputValue fieldMsg labelText inputName autocompleteType isRequired =
    let
        fieldLabel = labelView (prefix ++ "-" ++ inputName ++ "Input") labelText isRequired

        addressAutocomplete str =
            autocomplete <| prefix ++ " " ++ str

        errorHtml =
            if hasErrors then
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

            else
                text ""

        hasErrors =
            not (Dict.isEmpty errors) && not (List.isEmpty fieldErrors)

        isValid =
            not (Dict.isEmpty errors) && List.isEmpty fieldErrors

        fieldErrors =
            Dict.get inputName errors |> Maybe.withDefault []

        msgAttribute =
            case fieldMsg of
                InputMsg msg ->
                    onInput msg

                ChangeMsg msg ->
                    onChange msg
    in
    div [ class "form-group mb-2" ]
        [ fieldLabel
        , input
            [ id <| prefix ++ "-" ++ inputName ++ "Input"
            , classList
                [ ( "form-control", True )
                , ( "is-invalid", hasErrors )
                , ( "is-valid", isValid )
                ]
            , name inputName
            , type_ "text"
            , required isRequired
            , msgAttribute
            , value inputValue
            , addressAutocomplete autocompleteType
            ]
            []
        , errorHtml
        ]


{-| TODO: Refactor into Locations module? Or Views.Form if can't combine w/ HorizontalForm.
-}
locationSelect : Api.FormErrors -> String -> String -> (String -> msg) -> List Location -> String -> String -> Html msg
locationSelect errors prefix selectedValue selectMsg locations labelText inputName =
    let
        fieldLabel = labelView inputId labelText True

        toOption { code, name } =
            option [ value code, selected <| selectedValue == code ] [ text name ]

        inputId =
            prefix ++ "-" ++ inputName ++ "Input"

        onSelect =
            on "change" (targetValue |> Decode.map selectMsg)

        blankOption =
            if selectedValue == "" then
                [ { code = "", name = "" } ]

            else
                []

        errorHtml =
            if fieldHasErrors then
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

            else
                text ""

        fieldHasErrors =
            not <| List.isEmpty fieldErrors

        fieldErrors =
            Dict.get inputName errors |> Maybe.withDefault []

        formHasErrors =
            not <| Dict.isEmpty errors
    in
    div [ class "form-group mb-2" ]
        [ fieldLabel
        , List.map toOption (blankOption ++ locations)
            |> Html.select
                [ id inputId
                , classList
                    [ ( "form-control", True )
                    , ( "is-invalid", fieldHasErrors )
                    , ( "is-valid", formHasErrors && not fieldHasErrors )
                    ]
                , onSelect
                ]
        , errorHtml
        ]
