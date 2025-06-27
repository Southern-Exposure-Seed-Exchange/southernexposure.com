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

import Address
import Api
import Dict
import Html exposing (Html, a, button, div, form, h2, hr, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, target, type_)
import Html.Events exposing (onClick, onSubmit)
import Html.Extra exposing (viewIf)
import Json.Decode as Decode
import Json.Encode as Encode
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), centsFromString)
import PageData exposing (CustomerData, statusText)
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Zone)
import Update.Utils exposing (noCommand)
import Views.Admin as Admin exposing (equalsOriginal, updateEditField)
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Pager as Pager
import Views.Utils exposing (htmlOrBlank, routeLinkAttributes)



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
    , showDeleteConfirm : Bool
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
    , showDeleteConfirm = False
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
    | Delete
    | DeleteConfirmed
    | DeleteResponse (WebData ())


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

        Delete ->
            noCommand { model | showDeleteConfirm = True }

        DeleteConfirmed ->
            case RemoteData.toMaybe original |> Maybe.map .id of
                Just customerId ->
                    ( { model | isSaving = True }
                    , Api.delete (Api.AdminDeleteCustomer customerId)
                        |> Api.withJsonResponse (Decode.succeed ())
                        |> Api.sendRequest DeleteResponse
                    )

                Nothing ->
                    noCommand model

        DeleteResponse response ->
            case response of
                RemoteData.Success () ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin <| CustomerList { page = 1, perPage = 50, query = "" }
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Cmd.none
                    )

                _ ->
                    noCommand { model | isSaving = False }


edit : Zone -> EditForm -> AddressLocations -> PageData.AdminEditCustomerData -> List (Html EditMsg)
edit zone model locations original =
    let
        valueWithFallback s1 s2 =
            s1 model |> Maybe.withDefault (s2 original)

        inputRow modelSelector originalSelector =
            Form.inputRow model.errors (valueWithFallback modelSelector originalSelector)

        orderRow order =
            tr []
                [ td [] [ text <| String.fromInt order.id ]
                , td [] [ text <| Format.date zone order.date ]
                , td [] [ text <| statusText order.status ]
                , td [] [ Address.card order.shipping locations ]
                , td [] [ text <| Format.cents order.total ]
                , td [] [ a (routeLinkAttributes <| Admin <| AdminOrderDetails order.id) [ text "View Order" ] ]
                ]

        deleteEvent =
            if model.showDeleteConfirm then
                DeleteConfirmed

            else
                Delete

        deleteButton =
            viewIf (original.email /= "gardens+deleted@southernexposure.com") <|
                button [ type_ "button", onClick deleteEvent, class "btn btn-danger" ]
                    [ text <|
                        if model.showDeleteConfirm then
                            "Yes, Delete the Customer"

                        else
                            "Delete Customer"
                    ]

        deleteWarningText =
            viewIf model.showDeleteConfirm <|
                div [ class "text-danger text-right" ]
                    [ text "Are you sure? This will completely move all the Customer's orders to the "
                    , text "\"gardens+deleted@southernexposure.com\" account and then erase the Customer!"
                    ]
    in
    [ div [ class "text-right mb-4" ]
        [ htmlOrBlank
            (\stripeId ->
                a
                    [ href <| "https://dashboard.stripe.com/customers/" ++ stripeId
                    , target "_blank"
                    , class "btn btn-sm btn-secondary"
                    ]
                    [ text "View on Stripe" ]
            )
            original.stripeId
        , htmlOrBlank
            (\avalaraCode ->
                a
                    [ href <| "https://admin.avalara.com/exemptions/customers?searchTerm=" ++ avalaraCode
                    , target "_blank"
                    , class "btn btn-sm btn-secondary ml-2"
                    ]
                    [ text "View on Avalara" ]
            )
            original.avalaraCode
        ]
    , form [ class (Admin.formSavingClass model), onSubmit Submit ]
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
        , Form.checkboxRow (valueWithFallback .isAdmin .isAdmin)
            InputIsAdmin
            "Is Administrator"
            "IsAdmin"
        , div [ class "form-group form-row align-items-center" ] 
              [ div [ class "col-sm-3 col-form-label" ] []
              , text <| if original.verified then "This user is verified" else "This user is not verified" ]
        , deleteWarningText
        , div [ class "form-group d-flex justify-content-between" ]
            [ Admin.submitOrSavingButton model "Update Customer"
            , deleteButton
            ]
        ]
    , hr [] []
    , h2 [] [ text "Order History" ]
    , table [ class "table table-sm table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Order #" ]
                , th [] [ text "Date" ]
                , th [] [ text "Status" ]
                , th [] [ text "Address" ]
                , th [] [ text "Total" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody [] <| List.map orderRow original.orders
        ]
    ]
