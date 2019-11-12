module Views.OrderAdmin exposing
    ( DetailsForm
    , DetailsMsg
    , SearchForm
    , SearchMsg
    , details
    , initialDetailsForm
    , initialSearchForm
    , list
    , updateDetailsForm
    , updateSearchForm
    )

import Api
import Dict
import Html exposing (Html, a, div, form, h4, hr, li, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), centsFromString)
import OrderDetails
import PageData exposing (OrderData)
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Zone)
import Update.Utils exposing (noCommand)
import Views.Admin as Admin
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
            , Routing.newUrl key <| Admin <| OrderList { page = 1, perPage = 50, query = model.query }
            )


list : Zone -> AddressLocations -> String -> SearchForm -> Paginated OrderData String () -> List (Html SearchMsg)
list zone locations currentQuery { query } orders =
    let
        tableConfig =
            { itemDescription = "Orders"
            , searchFormQuery = query
            , searchMsg = SubmitSearch
            , queryInputMsg = InputQuery
            , pager = pager
            , tableHeader = header
            , rowRenderer = renderOrder
            }

        header =
            thead [ class "text-center" ]
                [ th [] [ text "ID" ]
                , th [] [ text "Date" ]
                , th [] [ text "Customer" ]
                , th [] [ text "Email" ]
                , th [] [ text "Street" ]
                , th [] [ text "State" ]
                , th [] [ text "Status" ]
                , th [] [ text "Total" ]
                , th [] [ text "" ]
                ]

        renderOrder order =
            tr []
                [ td [] [ text <| String.fromInt order.id ]
                , td [] [ text <| Format.date zone order.date ]
                , td [] [ text order.name ]
                , td [] [ text order.email ]
                , td [] [ text order.street ]
                , td [] [ text <| Maybe.withDefault "" <| Locations.regionName locations order.state ]
                , td [] [ text <| PageData.statusText order.status ]
                , td [] [ text <| Format.cents order.total ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| AdminOrderDetails order.id)
                        [ text "View" ]
                    ]
                ]

        pager =
            Pager.elements
                { itemDescription = "Orders"
                , pagerAriaLabel = "Orders Table Pages"
                , pagerCssClass = ""
                , pageSizes = [ 25, 50, 100, 250 ]
                , routeConstructor =
                    \{ page, perPage } ->
                        Admin <|
                            OrderList { page = page, perPage = perPage, query = currentQuery }
                }
                orders
    in
    Admin.searchableTable tableConfig orders



-- DETAILS


type alias DetailsForm =
    { comment : String
    , commentErrors : Api.FormErrors
    , refundAmount : String
    , refundErrors : Api.FormErrors
    , isSaving : Bool
    }


initialDetailsForm : DetailsForm
initialDetailsForm =
    { comment = ""
    , commentErrors = Api.initialErrors
    , refundAmount = ""
    , refundErrors = Api.initialErrors
    , isSaving = False
    }


type DetailsMsg
    = InputComment String
    | SubmitComment
    | SubmitCommentResponse (WebData (Result Api.FormErrors Int))
    | InputRefundAmount String
    | SubmitRefund
    | SubmitRefundResponse (WebData (Result Api.FormErrors Int))


updateDetailsForm : Routing.Key -> WebData PageData.AdminOrderDetails -> DetailsMsg -> DetailsForm -> ( DetailsForm, Cmd DetailsMsg )
updateDetailsForm key orderDetails msg model =
    case msg of
        InputComment val ->
            { model | comment = val }
                |> noCommand

        SubmitComment ->
            case RemoteData.toMaybe orderDetails |> Maybe.map (.details >> .order >> .id) of
                Just orderId ->
                    let
                        body =
                            Encode.object
                                [ ( "id", Encode.int orderId )
                                , ( "comment", Encode.string model.comment )
                                ]
                    in
                    ( { model | isSaving = True }
                    , Api.post Api.AdminOrderComment
                        |> Api.withJsonBody body
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest SubmitCommentResponse
                    )

                Nothing ->
                    noCommand model

        SubmitCommentResponse response ->
            case response of
                RemoteData.Success (Ok orderId) ->
                    ( initialDetailsForm
                    , Routing.newUrl key <| Admin (AdminOrderDetails orderId)
                    )

                RemoteData.Success (Err errors) ->
                    { model | commentErrors = errors, isSaving = False }
                        |> noCommand

                RemoteData.Failure errors ->
                    { model | commentErrors = Api.apiFailureToError errors, isSaving = False }
                        |> noCommand

                _ ->
                    { model | isSaving = False }
                        |> noCommand

        InputRefundAmount val ->
            { model | refundAmount = val }
                |> noCommand

        SubmitRefund ->
            case centsFromString model.refundAmount of
                Just (Cents refundCents) ->
                    case RemoteData.toMaybe orderDetails |> Maybe.map (.details >> .order >> .id) of
                        Just orderId ->
                            let
                                body =
                                    Encode.object
                                        [ ( "id", Encode.int orderId )
                                        , ( "amount", Encode.int refundCents )
                                        ]
                            in
                            ( { model | isSaving = True }
                            , Api.post Api.AdminOrderRefund
                                |> Api.withJsonBody body
                                |> Api.withErrorHandler Decode.int
                                |> Api.sendRequest SubmitRefundResponse
                            )

                        Nothing ->
                            noCommand model

                Nothing ->
                    { model
                        | refundErrors =
                            Api.addError "amount" "Please enter a decimal number." model.refundErrors
                    }
                        |> noCommand

        SubmitRefundResponse response ->
            case response of
                RemoteData.Success (Ok orderId) ->
                    ( initialDetailsForm
                    , Routing.newUrl key <| Admin <| AdminOrderDetails orderId
                    )

                RemoteData.Success (Err errors) ->
                    { model | refundErrors = errors, isSaving = False }
                        |> noCommand

                RemoteData.Failure error ->
                    { model | refundErrors = Api.apiFailureToError error, isSaving = False }
                        |> noCommand

                _ ->
                    { model | isSaving = False }
                        |> noCommand


details : Zone -> Int -> DetailsForm -> AddressLocations -> PageData.AdminOrderDetails -> List (Html DetailsMsg)
details zone orderId model locations orderDetails =
    [ div [] <| OrderDetails.view zone orderId locations orderDetails.details
    , hr [] []
    , h4 [] [ text "Admin Comments" ]
    , Form.genericErrorText <| not <| Dict.isEmpty model.commentErrors
    , ul [] <| List.map (renderAdminComment zone) orderDetails.adminComments
    , form [ onSubmit SubmitComment, class <| Admin.formSavingClass model ]
        [ Api.getErrorHtml "" model.commentErrors
        , Form.textareaRow model.commentErrors model.comment InputComment True "Comment" "comment" 5
        , Admin.submitOrSavingButton model "Add Comment"
        ]
    , hr [] []
    , h4 [] [ text "Issue Refund" ]
    , Form.genericErrorText <| not <| Dict.isEmpty model.refundErrors
    , form [ onSubmit SubmitRefund, class <| Admin.formSavingClass model ++ "mb-4" ]
        [ Api.getErrorHtml "" model.refundErrors
        , Form.inputRow model.refundErrors
            model.refundAmount
            InputRefundAmount
            True
            "Amount"
            "amount"
            "text"
            "off"
        , Admin.submitOrSavingButton model "Submit Refund"
        ]
    ]


renderAdminComment : Time.Zone -> PageData.AdminComment -> Html msg
renderAdminComment zone { time, content } =
    li []
        [ text <| "[" ++ Format.date zone time ++ "]: " ++ content ]
