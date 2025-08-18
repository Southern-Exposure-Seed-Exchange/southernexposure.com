module Components.Admin.ShippingAdmin exposing (Form, Msg, getShippingMethods, initialForm, update, view)

import Array exposing (Array)
import Components.Admin.Admin as Admin exposing (categorySelects, formSavingClass, multiSelect)
import Components.HorizontalForm as Form
import Data.Api as Api
import Data.Category as Category exposing (CategoryId(..))
import Data.Fields exposing (Cents, centsDecoder, centsEncoder, centsToString)
import Data.Locations as Locations exposing (Location)
import Data.PageData as PageData
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Data.Validation as Validation exposing (FormValidation)
import Dict
import Html exposing (Html, button, div, fieldset, form, legend, option, table, tbody, td, text, tr)
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Utils.Update exposing (noCommand, removeIndex, updateArray)
import Utils.View exposing (icon)



-- MODEL


type alias Form =
    { methods : Array MethodForm
    , categories : List PageData.AdminCategorySelect
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialForm : Form
initialForm =
    { methods = Array.fromList [ initialMethodForm ]
    , categories = []
    , errors = Api.initialErrors
    , isSaving = False
    }


formDecoder : Decoder Form
formDecoder =
    Decode.map4 Form
        (Decode.field "methods" <| Decode.array methodDecoder)
        (Decode.field "categories" <| Decode.list PageData.adminCategorySelectDecoder)
        (Decode.succeed Api.initialErrors)
        (Decode.succeed False)


validateForm : Form -> FormValidation (Array ValidMethod)
validateForm f =
    Validation.array "method" validateMethod f.methods


validFormEncoder : Array ValidMethod -> Value
validFormEncoder =
    Encode.array validMethodEncoder


type alias MethodForm =
    { id : Maybe Int
    , description : String
    , countries : Array String
    , rates : Array ShippingRate
    , priorityFee : String
    , priorityRate : String
    , priorityEnabled : Bool
    , categories : Array CategoryId
    , priorityExcludedCategories : Array CategoryId
    , priority : String
    , isActive : Bool
    }


initialMethodForm : MethodForm
initialMethodForm =
    { id = Nothing
    , description = ""
    , countries = Array.empty
    , rates = Array.fromList [ initialShippingRate ]
    , priorityFee = ""
    , priorityRate = ""
    , priorityEnabled = True
    , categories = Array.empty
    , priorityExcludedCategories = Array.empty
    , priority = "1"
    , isActive = True
    }


methodDecoder : Decoder MethodForm
methodDecoder =
    Decode.map8 MethodForm
        (Decode.field "id" <| Decode.map Just Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "countries" <| Decode.array Decode.string)
        (Decode.field "rates" <| Decode.array rateDecoder)
        (Decode.field "priorityFee" <| Decode.map centsToString centsDecoder)
        (Decode.field "priorityRate" <| Decode.map String.fromInt Decode.int)
        (Decode.field "isPriorityEnabled" Decode.bool)
        (Decode.field "categories" <| Decode.array Category.idDecoder)
        |> Decode.andThen
            (\f ->
                Decode.map3 f
                    (Decode.field "priorityCategories" <| Decode.array Category.idDecoder)
                    (Decode.field "priority" <| Decode.map String.fromInt Decode.int)
                    (Decode.field "isActive" Decode.bool)
            )


type alias ValidMethod =
    { id : Maybe Int
    , description : String
    , countries : Array String
    , rates : Array ValidShippingRate
    , priorityFee : Cents
    , priorityRate : Int
    , priorityEnabled : Bool
    , categories : Array CategoryId
    , priorityExcludedCategories : Array CategoryId
    , priority : Int
    , isActive : Bool
    }


validateMethod : MethodForm -> FormValidation ValidMethod
validateMethod f =
    Validation.formValidation
        (\rates pFee pRate priority ->
            { id = f.id
            , description = f.description
            , countries = f.countries
            , rates = rates
            , priorityFee = pFee
            , priorityRate = pRate
            , priorityEnabled = f.priorityEnabled
            , categories = f.categories
            , priorityExcludedCategories = f.priorityExcludedCategories
            , priority = priority
            , isActive = f.isActive
            }
        )
        |> Validation.applyValidation (Validation.array "rate" validateShippingRate f.rates)
        |> Validation.apply "priority-fee" (Validation.cents f.priorityFee)
        |> Validation.apply "priority-rate" (Validation.natural f.priorityRate)
        |> Validation.apply "priority" (Validation.natural f.priority)


validMethodEncoder : ValidMethod -> Value
validMethodEncoder m =
    Encode.object
        [ ( "id", Maybe.withDefault Encode.null <| Maybe.map Encode.int m.id )
        , ( "description", Encode.string m.description )
        , ( "countries", Encode.array Encode.string m.countries )
        , ( "rates", Encode.array validRateEncoder m.rates )
        , ( "priorityFee", centsEncoder m.priorityFee )
        , ( "priorityRate", Encode.int m.priorityRate )
        , ( "isPriorityEnabled", Encode.bool m.priorityEnabled )
        , ( "categories", Encode.array Category.idEncoder m.categories )
        , ( "priorityCategories", Encode.array Category.idEncoder m.priorityExcludedCategories )
        , ( "priority", Encode.int m.priority )
        , ( "isActive", Encode.bool m.isActive )
        ]


type alias ShippingRate =
    { threshold : String
    , amount : String
    , type_ : RateType
    }


type RateType
    = FlatRate
    | PercentageRate


initialShippingRate : ShippingRate
initialShippingRate =
    { threshold = "", amount = "", type_ = FlatRate }


rateDecoder : Decoder ShippingRate
rateDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "flat" ->
                        Decode.map3 ShippingRate
                            (Decode.field "threshold" <| Decode.map centsToString centsDecoder)
                            (Decode.field "amount" <| Decode.map centsToString centsDecoder)
                            (Decode.succeed FlatRate)

                    "percentage" ->
                        Decode.map3 ShippingRate
                            (Decode.field "threshold" <| Decode.map centsToString centsDecoder)
                            (Decode.field "amount" <| Decode.map String.fromInt Decode.int)
                            (Decode.succeed PercentageRate)

                    _ ->
                        Decode.fail <| "Could not parse ShippingRate type: " ++ type_
            )


type ValidShippingRate
    = ValidFlat { threshold : Cents, amount : Cents }
    | ValidPercentage { threshold : Cents, amount : Int }


validateShippingRate : ShippingRate -> FormValidation ValidShippingRate
validateShippingRate rate =
    case rate.type_ of
        FlatRate ->
            Validation.formValidation (\t a -> ValidFlat { threshold = t, amount = a })
                |> Validation.apply "threshold" (Validation.cents rate.threshold)
                |> Validation.apply "amount" (Validation.cents rate.amount)

        PercentageRate ->
            Validation.formValidation (\t a -> ValidPercentage { threshold = t, amount = a })
                |> Validation.apply "threshold" (Validation.cents rate.threshold)
                |> Validation.apply "amount" (Validation.natural rate.amount)


validRateEncoder : ValidShippingRate -> Value
validRateEncoder r =
    let
        ( ty, th, am ) =
            case r of
                ValidFlat rate ->
                    ( "flat", centsEncoder rate.threshold, centsEncoder rate.amount )

                ValidPercentage rate ->
                    ( "percentage", centsEncoder rate.threshold, Encode.int rate.amount )
    in
    Encode.object
        [ ( "type", Encode.string ty )
        , ( "threshold", th )
        , ( "amount", am )
        ]



-- UPDATE


type Msg
    = AddMethod
    | RemoveMethod Int
    | UpdateMethod Int MethodMsg
    | GetShippingData (WebData Form)
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors ()))


update : Routing.Key -> Msg -> Form -> ( Form, Cmd Msg )
update key msg model =
    case msg of
        AddMethod ->
            { model | methods = Array.push initialMethodForm model.methods }
                |> noCommand

        RemoveMethod index ->
            { model | methods = removeIndex index model.methods }
                |> noCommand

        UpdateMethod index subMsg ->
            { model | methods = updateArray index (updateMethod subMsg) model.methods }
                |> noCommand

        GetShippingData resp ->
            case resp of
                RemoteData.Success f ->
                    ( f, Cmd.none )

                _ ->
                    ( { model | errors = Api.addError "" "Failed to fetch Shipping Methods from server." model.errors }
                    , Ports.scrollToErrorMessage
                    )

        Submit ->
            case validateForm model of
                Ok validForm ->
                    ( { model | isSaving = True, errors = Api.initialErrors }
                    , Api.post Api.AdminShipping
                        |> Api.withJsonBody (validFormEncoder validForm)
                        |> Api.withErrorHandler (Decode.succeed ())
                        |> Api.sendRequest SubmitResponse
                    )

                Err err ->
                    ( { model | errors = err }
                    , Ports.scrollToErrorMessage
                    )

        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( { model | isSaving = False }
                    , Routing.newUrl key <| Admin ShippingMethods
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand { model | isSaving = False }


getShippingMethods : Cmd Msg
getShippingMethods =
    Api.get Api.AdminShippingData
        |> Api.withJsonResponse formDecoder
        |> Api.sendRequest GetShippingData


type MethodMsg
    = InputDescription String
    | InputPriorityFee String
    | InputPriorityRate String
    | InputPriority String
    | InputPriorityEnabled Bool
    | InputIsActive Bool
      -- Country Select
    | AddCountry
    | SelectCountry Int String
    | RemoveCountry Int
      -- Category Select
    | AddCategory
    | SelectCategory Int CategoryId
    | RemoveCategory Int
      -- Priority Excluded Categories
    | AddPriorityCategory
    | SelectPriorityCategory Int CategoryId
    | RemovePriorityCategory Int
      -- Rate Forms
    | AddRate
    | UpdateRate Int RateMsg
    | RemoveRate Int


updateMethod : MethodMsg -> MethodForm -> MethodForm
updateMethod msg model =
    case msg of
        InputDescription v ->
            { model | description = v }

        InputPriorityFee v ->
            { model | priorityFee = v }

        InputPriorityRate v ->
            { model | priorityRate = v }

        InputPriority v ->
            { model | priority = v }

        InputPriorityEnabled v ->
            { model | priorityEnabled = v }

        InputIsActive v ->
            { model | isActive = v }

        AddCountry ->
            { model | countries = Array.push "US" model.countries }

        SelectCountry index v ->
            { model | countries = updateArray index (always v) model.countries }

        RemoveCountry index ->
            { model | countries = removeIndex index model.countries }

        AddCategory ->
            { model | categories = Array.push (CategoryId 0) model.categories }

        SelectCategory index v ->
            { model | categories = updateArray index (always v) model.categories }

        RemoveCategory index ->
            { model | categories = removeIndex index model.categories }

        AddPriorityCategory ->
            { model
                | priorityExcludedCategories =
                    Array.push (CategoryId 0) model.priorityExcludedCategories
            }

        SelectPriorityCategory index v ->
            { model
                | priorityExcludedCategories =
                    updateArray index (always v) model.priorityExcludedCategories
            }

        RemovePriorityCategory index ->
            { model
                | priorityExcludedCategories =
                    removeIndex index model.priorityExcludedCategories
            }

        AddRate ->
            { model | rates = Array.push initialShippingRate model.rates }

        UpdateRate index subMsg ->
            { model | rates = updateArray index (updateRate subMsg) model.rates }

        RemoveRate index ->
            { model | rates = removeIndex index model.rates }


type RateMsg
    = SelectRate RateType
    | InputThreshold String
    | InputAmount String


updateRate : RateMsg -> ShippingRate -> ShippingRate
updateRate msg model =
    case msg of
        SelectRate v ->
            { model | amount = "", type_ = v }

        InputThreshold v ->
            { model | threshold = v }

        InputAmount v ->
            { model | amount = v }



-- VIEW


view : Form -> List Location -> List (Html Msg)
view model countries =
    [ form [ class <| formSavingClass model, onSubmit Submit ] <|
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , div [] <|
            Array.toList <|
                Array.indexedMap (renderMethodForm model.categories countries model.errors) model.methods
        , div [ class "form-group mb-4" ]
            [ Admin.submitOrSavingButton model "Update Shipping Methods"
            , button
                [ class "ml-3 btn btn-secondary", type_ "button", onClick AddMethod ]
                [ text "Add Method" ]
            ]
        ]
    ]


renderMethodForm : List PageData.AdminCategorySelect -> List Location -> Api.FormErrors -> Int -> MethodForm -> Html Msg
renderMethodForm categories countries errors_ index model =
    let
        errors =
            Api.prefixedArrayErrors "method" index errors_

        removeButton =
            div [ class "form-group" ]
                [ button [ class "ml-3 btn btn-danger", type_ "button", onClick <| RemoveMethod index ]
                    [ text "Remove Method" ]
                ]

        rateForms =
            Form.withLabel "Rates" True <|
                [ table [ class "table table-sm table-striped px-4 rate-table" ]
                    [ tbody [] <|
                        Array.toList <|
                            Array.indexedMap
                                (\i f ->
                                    renderRateForm errors i f
                                        |> List.map (Html.map (UpdateRate i))
                                        |> (\html -> html ++ [ removeRateButton i ])
                                        |> tr []
                                )
                                model.rates
                    ]
                , button [ class "ml-3 btn btn-sm btn-secondary", type_ "button", onClick AddRate ]
                    [ text "Add Rate" ]
                ]

        removeRateButton i =
            td [ class "text-center align-middle" ]
                [ button [ class "btn btn-sm btn-danger", type_ "button", onClick <| RemoveRate i ]
                    [ icon "times" ]
                ]
    in
    [ legend [] [ text model.description ]
    , Form.inputRow errors model.description InputDescription True "Description" "description" "text" "off"
    , countrySelects countries model errors
    , rateForms
    , Form.inputRow errors model.priorityFee InputPriorityFee True "Priority Fee ($)" "priority-fee" "text" "off"
    , Form.inputRow errors model.priorityRate InputPriorityRate True "Priority Rate (%)" "priority-rate" "text" "off"
    , Form.checkboxRow model.priorityEnabled InputPriorityEnabled "Is Priority Enabled" ("priority-enabled-1" ++ String.fromInt index)
    , categorySelects False
        SelectCategory
        AddCategory
        RemoveCategory
        { errors = errors, categories = model.categories }
        categories
    , prioritySelects categories model errors
    , Form.inputRow errors model.priority InputPriority True "Priority" "priority" "text" "off"
    , Form.checkboxRow model.isActive InputIsActive "Is Active" ("is-active-" ++ String.fromInt index)
    ]
        |> List.map (Html.map (UpdateMethod index))
        |> (\html -> html ++ [ removeButton ])
        |> fieldset [ class "form-group" ]


renderRateForm : Api.FormErrors -> Int -> ShippingRate -> List (Html RateMsg)
renderRateForm errors_ index model =
    let
        errors =
            Api.prefixedArrayErrors "rate" index errors_

        amountLabel =
            if model.type_ == FlatRate then
                "Amount ($)"

            else
                "Amount (%)"

        parseType str =
            case str of
                "flat" ->
                    Ok FlatRate

                "percentage" ->
                    Ok PercentageRate

                _ ->
                    Err <| "Could not parse RateType: " ++ str

        typeOptions =
            List.map
                (\( t, n, v ) ->
                    option [ value v, selected <| t == model.type_ ]
                        [ text n ]
                )
                [ ( FlatRate, "Flat Rate", "flat" )
                , ( PercentageRate, "Percentage", "percentage" )
                ]
    in
    [ td [] [ Form.selectRow parseType SelectRate "Type" True typeOptions ]
    , td [] [ Form.inputRow errors model.threshold InputThreshold True "Threshold" "threshold" "text" "off" ]
    , td [] [ Form.inputRow errors model.amount InputAmount True amountLabel "amount" "text" "off" ]
    ]


prioritySelects : List PageData.AdminCategorySelect -> MethodForm -> Api.FormErrors -> Html MethodMsg
prioritySelects categories model errors =
    let
        cfg : Admin.MultiSelectConfig MethodMsg CategoryId
        cfg =
            { isRequired = False
            , selectMsg = SelectPriorityCategory
            , addMsg = AddPriorityCategory
            , removeMsg = RemovePriorityCategory
            , errors = errors
            , selected = model.priorityExcludedCategories
            , items = List.map (\c -> { name = c.name, value = c.id }) categories
            , toValue = \(CategoryId i) -> String.fromInt i
            , fromValue = Category.idParser
            , blankOption = { name = "", value = CategoryId 0 }
            , prefix = "priority-category"
            , label = "Priority-Excluded Categories"
            , addLabel = "Category"
            }
    in
    multiSelect cfg


countrySelects : List Location -> MethodForm -> Api.FormErrors -> Html MethodMsg
countrySelects countries model errors =
    let
        cfg : Admin.MultiSelectConfig MethodMsg String
        cfg =
            { isRequired = False
            , selectMsg = SelectCountry
            , addMsg = AddCountry
            , removeMsg = RemoveCountry
            , errors = errors
            , selected = model.countries
            , items = List.map (\c -> { name = c.name, value = c.code }) countries
            , toValue = identity
            , fromValue = Ok
            , blankOption = { name = "Select a Country", value = "" }
            , prefix = "country"
            , label = "Countries"
            , addLabel = "Country"
            }
    in
    multiSelect cfg
