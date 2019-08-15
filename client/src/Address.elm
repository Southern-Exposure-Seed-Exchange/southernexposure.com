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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Locations exposing (AddressLocations, Location, Region(..), regionDecoder, regionEncoder)
import Views.HorizontalForm as Form
import Views.Utils exposing (autocomplete)



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
    , state : Region
    , zipCode : String
    , country : String
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
    , state = USState "AL"
    , zipCode = ""
    , country = "US"
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
        (Decode.field "state" regionDecoder)
        |> Decode.andThen
            (\constr ->
                Decode.map3 constr
                    (Decode.field "zipCode" Decode.string)
                    (Decode.field "country" Decode.string)
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
            Locations.regionName locations address.state
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
    ]
        |> List.filterMap identity
        |> List.intersperse (br [] [])
        |> Html.address []


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
        |> Html.select [ class "form-control", onSelectInt selectMsg ]


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
                    field stateCode State "State / Province" "state" "address-level1" True

        stateCode =
            .state >> Locations.fromRegion
    in
    div []
        [ generalErrors
        , field .firstName FirstName "First Name" "firstName" "given-name" True
        , field .lastName LastName "Last Name" "lastName" "family-name" True
        , field .companyName CompanyName "Company Name" "companyName" "organization" False
        , field .street Street "Street Address" "addressOne" "address-line1" True
        , field .addressTwo AddressTwo "Address Line 2" "addressTwo" "address-line2" False
        , field .city City "City" "city" "address-level2" True
        , regionField
        , field .zipCode ZipCode "Zip Code" "zipCode" "postal-code" True
        , selectField .country Country locations.countries "Country" "country"
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

        selectRow =
            Form.selectRow Ok

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
                    requiredField stateCode State "State / Province" "state" "state" "address-level1"

        regionSelect labelText =
            List.map (locationToOption stateCode) >> selectRow State labelText True

        stateCode =
            .state >> Locations.fromRegion
    in
    [ requiredField .firstName FirstName "First Name" "firstName" "text" "given-name"
    , requiredField .lastName LastName "Last Name" "lastName" "text" "family-name"
    , requiredField .street Street "Street Address" "addressOne" "text" "address-line1"
    , optionalField .addressTwo AddressTwo "Address Line 2" "addressTwo" "text" "address-line2"
    , requiredField .city City "City" "city" "text" "address-level2"
    , regionField
    , requiredField .zipCode ZipCode "Zip Code" "zipCode" "text" "postal-code"
    , countrySelect
    ]


{-| TODO: Refactor into separate Views.Form module. Turn parameters into a Config type.
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
                    small [ class "text-muted" ] [ text " (optional)" ]

                  else
                    text ""
                ]

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
            , onInput inputMsg
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
        fieldLabel =
            label [ class "mb-0", for inputId ]
                [ text labelText ]

        toOption { code, name } =
            option [ value code, selected <| selectedValue == code ] [ text name ]

        inputId =
            prefix ++ "-" ++ inputName ++ "Input"

        onSelect =
            on "change" (targetValue |> Decode.map selectMsg)

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
        , List.map toOption locations
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
