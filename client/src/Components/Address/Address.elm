module Components.Address.Address exposing
    ( AddressCompletionSetting(..)
    , AddressId(..)
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

import Components.Aria as Aria
import Components.HorizontalForm as Form
import Data.Api as Api
import Data.Locations as Locations exposing (AddressLocations, Location, Region(..), regionDecoder, regionEncoder)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, for, id, list, name, placeholder, required, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Html.Events.Extra exposing (onChange)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded)
import Json.Encode as Encode exposing (Value)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url exposing (percentEncode)
import Utils.Update exposing (noCommand)
import Utils.View exposing (autocomplete, labelView)



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
    , showSuggestions : Bool
    , suggestions : List AutocompleteData
    , debounceId : Int
    , warnings : Dict String (List String)
    , selectSuggestionRd : WebData ()
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
    , showSuggestions = False
    , suggestions = []
    , debounceId = 0
    , warnings = Dict.empty
    , selectSuggestionRd = NotAsked
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
                    |> hardcoded False
                    |> hardcoded []
                    |> hardcoded 0
                    |> hardcoded Dict.empty
                    |> hardcoded NotAsked
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
    | DebouncedRequest Int String
    | AutocompleteSuggestions (WebData (Result Api.FormErrors AutocompletePreview))
    | SuggestionSelected AutocompleteData
    | AddressCompleted (WebData (Result Api.FormErrors AutocompleteResponse))


type alias PostgridApiKey =
    String


type AddressCompletionSetting
    = AddressCompletionEnabled PostgridApiKey
    | AddressCompletionDisabled


update : AddressCompletionSetting -> Msg -> Form -> ( Form, Cmd Msg )
update addressCompletionSetting msg ({ model } as f) =
    let
        ( newModel, cmd ) =
            updateModel addressCompletionSetting msg model
    in
    ( { f | model = newModel }, cmd )


updateModel : AddressCompletionSetting -> Msg -> Model -> ( Model, Cmd Msg )
updateModel addressCompletionSetting msg model =
    case msg of
        FirstName str ->
            { model | firstName = str } |> noCommand

        LastName str ->
            ( { model | lastName = str }, Cmd.none )

        CompanyName str ->
            { model | companyName = str } |> noCommand

        Street str ->
            handleAutocompleteField addressCompletionSetting msg (\m -> { m | street = str }) model

        AddressTwo str ->
            handleAutocompleteField addressCompletionSetting msg (\m -> { m | addressTwo = str }) model

        City str ->
            handleAutocompleteField addressCompletionSetting msg (\m -> { m | city = str }) model

        State str ->
            handleAutocompleteField addressCompletionSetting
                msg
                (\m ->
                    case m.country of
                        "US" ->
                            if List.member str Locations.armedForcesCodes then
                                { m | state = Just <| ArmedForces str }

                            else
                                { m | state = Just <| USState str }

                        "CA" ->
                            { m | state = Just <| CAProvince str }

                        _ ->
                            { m | state = Just <| Custom str }
                )
                model

        ZipCode str ->
            handleAutocompleteField addressCompletionSetting msg (\m -> { m | zipCode = str }) model

        Country str ->
            if model.country /= str then
                { model | country = str, state = Nothing, warnings = Dict.empty } |> noCommand

            else
                { model | warnings = Dict.empty } |> noCommand

        PhoneNumber str ->
            { model | phoneNumber = str } |> noCommand

        DebouncedRequest debounceId str ->
            case addressCompletionSetting of
                AddressCompletionEnabled postgridApiKey ->
                    if model.debounceId == debounceId then
                        if String.length str > 3 then
                            ( model, fetchSuggestions str postgridApiKey )

                        else
                            ( { model | showSuggestions = False, suggestions = [] }, Cmd.none )

                    else
                        ( model, Cmd.none )

                AddressCompletionDisabled ->
                    ( model, Cmd.none )

        AutocompleteSuggestions (RemoteData.Success (Ok results)) ->
            { model | suggestions = results.data } |> noCommand

        AutocompleteSuggestions _ ->
            model |> noCommand

        SuggestionSelected suggestion ->
            case addressCompletionSetting of
                AddressCompletionEnabled postgridApiKey ->
                    ( { model | showSuggestions = False, selectSuggestionRd = Loading }, completeAddress suggestion postgridApiKey )

                AddressCompletionDisabled ->
                    ( { model | showSuggestions = False, selectSuggestionRd = Loading }, Cmd.none )

        AddressCompleted (RemoteData.Success (Ok response)) ->
            case response.data of
                Nothing ->
                    ( { model | selectSuggestionRd = Success () }, Cmd.none )

                Just addr ->
                    let
                        newModel =
                            { model
                                | street = addr.address
                                , city = addr.city
                                , state = Just <| USState (String.toUpper addr.prov)
                                , zipCode = addr.pc
                                , country = String.toUpper addr.country
                                , warnings = addr.errors
                                , selectSuggestionRd = Success ()
                            }
                    in
                    ( newModel, Cmd.none )

        AddressCompleted _ ->
            ( model, Cmd.none )


type alias AutocompleteData =
    { address : String
    , city : String
    , pc : String
    }


autocompleteDataDecoder : Decoder AutocompleteData
autocompleteDataDecoder =
    Decode.field "preview" <|
        Decode.map3 AutocompleteData
            (Decode.field "address" Decode.string)
            (Decode.field "city" Decode.string)
            (Decode.field "pc" Decode.string)


{-| <https://postgrid.readme.io/reference/get-autocomplete-previews>
-}
type alias AutocompletePreview =
    { status : String
    , message : String
    , data : List AutocompleteData
    }


autocompletePreviewDecoder : Decoder AutocompletePreview
autocompletePreviewDecoder =
    Decode.map3 AutocompletePreview
        (Decode.field "status" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "data" (Decode.list autocompleteDataDecoder))


fetchSuggestions : String -> String -> Cmd Msg
fetchSuggestions str postgridApiKey =
    let
        request =
            { method = "GET"
            , headers = [ Http.header "x-api-key" postgridApiKey ]
            , url = "https://api.postgrid.com/v1/addver/completions?partialStreet=" ++ percentEncode str ++ "&countryFilter=US&provInsteadOfPC=false"
            , body = Http.emptyBody
            , expect = Http.expectString Result.toMaybe
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Api.sendRequest AutocompleteSuggestions (Api.withErrorHandler autocompletePreviewDecoder request)


{-| <https://postgrid.readme.io/reference/autocomplete-an-address>
-}
type alias AutocompleteResponse =
    { status : String
    , message : String
    , data : Maybe AutocompleteResponseData
    }


autocompleteResponseDecoder : Decoder AutocompleteResponse
autocompleteResponseDecoder =
    let
        dataDecoder =
            Decode.oneOf
                [ Decode.list Decode.value
                    |> Decode.andThen (\_ -> Decode.succeed Nothing)
                , autocompleteResponseDataDecoder |> Decode.map Just
                ]
    in
    Decode.map3 AutocompleteResponse
        (Decode.field "status" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "data" dataDecoder)


type alias AutocompleteResponseData =
    { address : String
    , city : String
    , prov : String
    , pc : String
    , country : String
    , errors : Dict.Dict String (List String)
    }


autocompleteResponseDataDecoder : Decoder AutocompleteResponseData
autocompleteResponseDataDecoder =
    let
        errorsDecoder : Decoder (Dict.Dict String (List String))
        errorsDecoder =
            Decode.dict (Decode.list Decode.string)
    in
    Decode.map6 AutocompleteResponseData
        (Decode.at [ "address", "address" ] Decode.string)
        (Decode.at [ "address", "city" ] Decode.string)
        (Decode.at [ "address", "prov" ] Decode.string)
        (Decode.at [ "address", "pc" ] Decode.string)
        (Decode.at [ "address", "country" ] Decode.string)
        (Decode.field "errors" errorsDecoder)


completeAddress : AutocompleteData -> String -> Cmd Msg
completeAddress suggestion postgridApiKey =
    let
        request =
            { method = "POST"
            , headers = [ Http.header "x-api-key" postgridApiKey ]
            , url = "https://api.postgrid.com/v1/addver/completions?index=0"
            , body =
                Http.jsonBody
                    (Encode.object
                        [ ( "partialStreet", Encode.string suggestion.address )
                        , ( "cityFilter", Encode.string suggestion.city )
                        , ( "pcFilter", Encode.string suggestion.pc )
                        , ( "countryFilter", Encode.string "US" )
                        ]
                    )
            , expect = Http.expectString Result.toMaybe
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Api.sendRequest AddressCompleted (Api.withErrorHandler autocompleteResponseDecoder request)


{-| Helper for debounced autocomplete fields
-}
handleAutocompleteField : AddressCompletionSetting -> Msg -> (Model -> Model) -> Model -> ( Model, Cmd Msg )
handleAutocompleteField addressCompletionSetting msg updateField m =
    let
        updatedModel =
            updateField m

        concatFields =
            String.join " "
                [ updatedModel.street
                , updatedModel.city
                , Maybe.withDefault "" (updatedModel.state |> Maybe.map Locations.fromRegion)
                , updatedModel.zipCode
                , updatedModel.country
                ]

        newDebounceId =
            updatedModel.debounceId + 1

        cmd =
            case addressCompletionSetting of
                AddressCompletionEnabled _ ->
                    if updatedModel.country == "US" && String.length concatFields > 3 then
                        Task.perform identity
                            (Process.sleep 300
                                |> Task.andThen (always <| Task.succeed <| DebouncedRequest newDebounceId concatFields)
                            )

                    else
                        Cmd.none

                AddressCompletionDisabled ->
                    Cmd.none

        suggestions =
            if String.length concatFields < 3 then
                []

            else
                updatedModel.suggestions

        showSuggestions =
            case msg of
                Street _ ->
                    String.length concatFields > 3

                _ ->
                    updatedModel.showSuggestions
    in
    ( { updatedModel | debounceId = newDebounceId, suggestions = suggestions, showSuggestions = showSuggestions, warnings = Dict.empty }, cmd )



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
        |> Html.address [ class "" ]


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
        , viewWarnings model
        , field .firstName (InputMsg FirstName) "First Name" "firstName" "given-name" True
        , field .lastName (InputMsg LastName) "Last Name" "lastName" "family-name" True
        , field .companyName (InputMsg CompanyName) "Company Name" "companyName" "organization" False
        , disableWhenLoading model <| field .street (InputMsg Street) "Street Address" "addressOne" "address-line1" True
        , viewSuggestions model
        , field .addressTwo (InputMsg AddressTwo) "Address Line 2" "addressTwo" "address-line2" False
        , disableWhenLoading model <| field .city (InputMsg City) "City" "city" "address-level2" True
        , disableWhenLoading model <| regionField
        , disableWhenLoading model <| field .zipCode (ChangeMsg ZipCode) "Zip Code" "zipCode" "postal-code" True
        , disableWhenLoading model <| selectField .country Country locations.countries "Country" "country"
        , field .phoneNumber (InputMsg PhoneNumber) "Phone Number" "phoneNumber" "tel" True
        ]


{-| Make the form less visible and unclickable when it is loading.
-}
disableWhenLoading model view =
    case model.selectSuggestionRd of
        Loading ->
            div [ class "tw:opacity-50 tw:pointer-events-none" ] [ view ]

        _ ->
            div [] [ view ]


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
    , viewWarnings model
    , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[16px]" ]
        [ requiredField .firstName FirstName "First Name" "firstName" "text" "given-name"
        , requiredField .lastName LastName "Last Name" "lastName" "text" "family-name"
        ]
    , disableWhenLoading model <| requiredField .street Street "Street Address" "addressOne" "text" "address-line1"
    , viewSuggestions model
    , optionalField .addressTwo AddressTwo "Address Line 2" "addressTwo" "text" "address-line2"
    , disableWhenLoading model <|
        div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[16px]" ]
            [ countrySelect
            , regionField
            ]
    , disableWhenLoading model <|
        div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[16px]" ]
            [ requiredField .city City "City" "city" "text" "address-level2"
            , requiredField .zipCode ZipCode "Zip Code" "zipCode" "text" "postal-code"
            ]
    , requiredField .phoneNumber PhoneNumber "Phone Number" "phoneNumber" "tel" "tel"
    ]


type FieldMsg msg
    = InputMsg (String -> msg)
    | ChangeMsg (String -> msg)


{-| TODO: Refactor into separate Components.Admin.Form module. Turn parameters into a Config type.
-}
inputField : Api.FormErrors -> String -> String -> FieldMsg msg -> String -> String -> String -> Bool -> Html msg
inputField errors prefix inputValue fieldMsg labelText inputName autocompleteType isRequired =
    let
        fieldLabel =
            labelView (prefix ++ "-" ++ inputName ++ "Input") labelText isRequired

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
            , placeholder labelText
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


{-| TODO: Refactor into Locations module? Or Components.Admin.Form if can't combine w/ HorizontalForm.
-}
locationSelect : Api.FormErrors -> String -> String -> (String -> msg) -> List Location -> String -> String -> Html msg
locationSelect errors prefix selectedValue selectMsg locations labelText inputName =
    let
        fieldLabel =
            labelView inputId labelText True

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


viewSuggestions : Model -> Html Msg
viewSuggestions model =
    if model.showSuggestions then
        div [ class "tw:relative" ]
            [ div [ class "tw:absolute tw:z-[10]" ]
                [ ul [ class "tw:flex tw:flex-col tw:bg-white tw:rounded-[16px] tw:shadow-md tw:max-h-[300px] tw:overflow-auto" ]
                    (List.map viewSuggestion model.suggestions)
                ]
            ]

    else
        text ""


viewSuggestion : AutocompleteData -> Html Msg
viewSuggestion suggestion =
    let
        suggestionText =
            suggestion.address ++ ", " ++ suggestion.city ++ " " ++ suggestion.pc
    in
    li [ class "tw:p-[8px] tw:hover:bg-gray-200 tw:cursor-pointer", onClick (SuggestionSelected suggestion) ]
        [ text suggestionText ]


viewWarnings : Model -> Html msg
viewWarnings model =
    List.concat (Dict.values model.warnings)
        |> List.map Html.text
        |> List.intersperse (Html.br [] [])
        |> Html.div [ class "text-warning" ]
