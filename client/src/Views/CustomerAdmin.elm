module Views.CustomerAdmin exposing
    ( EditForm
    , EditMsg
    , SearchForm
    , SearchMsg
    , edit
    , initialEditForm
    , initialSearchForm
    , list
    , updateEditForm
    , updateSearchForm
    )

import Api
import Dict
import Html exposing (Html, a, div, form, input, label, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, for, id, name, type_)
import Html.Events exposing (onCheck, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Fields exposing (Cents(..), centsFromString)
import PageData exposing (CustomerData)
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Update.Utils exposing (noCommand)
import Views.Admin as Admin exposing (equalsOriginal, updateEditField)
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Pager as Pager
import Views.Utils exposing (routeLinkAttributes)



-- LIST


type alias SearchForm =
    { query : String
    }


initialSearchForm : SearchForm
initialSearchForm =
    { query = ""
    }


type SearchMsg
    = InputQuery String
    | SubmitSearch


updateSearchForm : Routing.Key -> SearchMsg -> SearchForm -> ( SearchForm, Cmd SearchMsg )
updateSearchForm key msg model =
    case msg of
        InputQuery val ->
            ( { model | query = val }, Cmd.none )

        SubmitSearch ->
            ( model
            , Routing.newUrl key <| Admin <| CustomerList { page = 1, perPage = 50, query = model.query }
            )


list : String -> SearchForm -> Paginated CustomerData String () -> List (Html SearchMsg)
list currentQuery { query } customers =
    let
        tableConfig =
            { itemDescription = "Customers"
            , searchFormQuery = query
            , searchMsg = SubmitSearch
            , queryInputMsg = InputQuery
            , pager = pager
            , tableHeader = header
            , rowRenderer = renderCustomer
            }

        header =
            thead [ class "text-center" ]
                [ th [] [ text "ID" ]
                , th [] [ text "Name" ]
                , th [] [ text "Email" ]
                , th [] []
                ]

        renderCustomer customer =
            tr []
                [ td [] [ text <| String.fromInt customer.id ]
                , td [] [ text customer.name ]
                , td [] [ text customer.email ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| CustomerEdit customer.id)
                        [ text "Edit" ]
                    ]
                ]

        pager =
            Pager.elements
                { itemDescription = "Customers"
                , pagerAriaLabel = "Customers Table Pages"
                , pagerCssClass = ""
                , pageSizes = [ 25, 50, 100, 250 ]
                , routeConstructor =
                    \{ page, perPage } ->
                        Admin <|
                            CustomerList { page = page, perPage = perPage, query = currentQuery }
                }
                customers
    in
    Admin.searchableTable tableConfig customers



-- EDIT


type alias EditForm =
    { email : Maybe String
    , storeCredit : Maybe String
    , isAdmin : Maybe Bool
    , password : String
    , passwordConfirm : String
    , isSaving : Bool
    , errors : Api.FormErrors
    }


initialEditForm : EditForm
initialEditForm =
    { email = Nothing
    , storeCredit = Nothing
    , isAdmin = Nothing
    , password = ""
    , passwordConfirm = ""
    , isSaving = False
    , errors = Api.initialErrors
    }


type EditMsg
    = InputEmail String
    | InputStoreCredit String
    | InputIsAdmin Bool
    | InputPassword String
    | InputPasswordConfirm String
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors Int))


updateEditForm : Routing.Key -> WebData PageData.AdminEditCustomerData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    case msg of
        InputEmail val ->
            noCommand <|
                updateEditField val original .email <|
                    \v -> { model | email = v }

        InputStoreCredit val ->
            noCommand <|
                updateEditField val original (.storeCredit >> Format.centsNumber) <|
                    \v -> { model | storeCredit = v }

        InputIsAdmin val ->
            noCommand <|
                if equalsOriginal val original .isAdmin then
                    { model | isAdmin = Nothing }

                else
                    { model | isAdmin = Just val }

        InputPassword val ->
            noCommand <|
                { model | password = val }

        InputPasswordConfirm val ->
            noCommand <|
                { model | passwordConfirm = val }

        Submit ->
            let
                passwordsMatch =
                    model.password == model.passwordConfirm

                encodedPassword =
                    if not (String.isEmpty model.password) && passwordsMatch then
                        [ ( "password", Encode.string model.password )
                        , ( "passwordConfirm", Encode.string model.passwordConfirm )
                        ]

                    else
                        [ ( "password", Encode.null ), ( "passwordConfirm", Encode.null ) ]

                storeCreditCents =
                    model.storeCredit |> Maybe.andThen centsFromString

                encodedStoreCredit =
                    case storeCreditCents of
                        Nothing ->
                            [ ( "storeCredit", Encode.null ) ]

                        Just (Cents c) ->
                            [ ( "storeCredit", Encode.int c ) ]

                jsonBody customerId =
                    Encode.object <|
                        [ ( "id", Encode.int customerId )
                        , ( "email", encodeMaybe Encode.string model.email )
                        , ( "isAdmin", encodeMaybe Encode.bool model.isAdmin )
                        ]
                            ++ encodedStoreCredit
                            ++ encodedPassword

                encodeMaybe encoder =
                    Maybe.map encoder >> Maybe.withDefault Encode.null
            in
            case RemoteData.toMaybe original |> Maybe.map .id of
                Just customerId ->
                    if not passwordsMatch then
                        ( { model | errors = Api.addError "password" "Passwords do not match." model.errors }
                        , Cmd.none
                        )

                    else
                        case ( model.storeCredit, storeCreditCents ) of
                            ( Just _, Nothing ) ->
                                ( { model | errors = Api.addError "storeCredit" "Please enter a valid dollar amount." model.errors }
                                , Cmd.none
                                )

                            _ ->
                                ( { model | isSaving = True }
                                , Api.patch Api.AdminEditCustomer
                                    |> Api.withJsonBody (jsonBody customerId)
                                    |> Api.withErrorHandler Decode.int
                                    |> Api.sendRequest SubmitResponse
                                )

                Nothing ->
                    noCommand model

        SubmitResponse response ->
            case response of
                RemoteData.Success (Ok customerId) ->
                    ( { model | isSaving = False }
                    , Routing.newUrl key <| Admin <| CustomerEdit customerId
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }, Cmd.none )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }, Cmd.none )

                _ ->
                    noCommand model


edit : EditForm -> PageData.AdminEditCustomerData -> List (Html EditMsg)
edit model original =
    let
        valueWithFallback s1 s2 =
            s1 model |> Maybe.withDefault (s2 original)

        inputRow modelSelector originalSelector =
            Form.inputRow model.errors (valueWithFallback modelSelector originalSelector)

        adminCheckbox =
            div [ class "form-group form-row align-items-center" ]
                [ div [ class "col-sm-3 col-form-label" ] [ text "" ]
                , div [ class "col" ]
                    [ div [ class "form-check" ]
                        [ input
                            [ id "inputIsAdmin"
                            , name "IsAdmin"
                            , class "form-check-input"
                            , type_ "checkbox"
                            , onCheck InputIsAdmin
                            , checked <| valueWithFallback .isAdmin .isAdmin
                            ]
                            []
                        , label [ class "form-check-label", for "inputIsAdmin" ] [ text "Is Administrator" ]
                        ]
                    ]
                ]
    in
    [ form [ class (Admin.formSavingClass model), onSubmit Submit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow .email .email InputEmail True "Email" "email" "email" "off"
        , inputRow .storeCredit
            (.storeCredit >> Format.centsNumber)
            InputStoreCredit
            True
            "Store Credit"
            "storeCredit"
            "text"
            "off"
        , Form.inputRow model.errors model.password InputPassword False "New Password" "password" "password" "off"
        , Form.inputRow model.errors model.passwordConfirm InputPasswordConfirm False "Confirm Password" "passwordConfirm" "password" "off"
        , adminCheckbox
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Update Customer" ]
        ]
    ]
