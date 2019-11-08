module Views.OrderAdmin exposing
    ( SearchForm
    , SearchMsg
    , details
    , initialSearchForm
    , list
    , updateSearchForm
    )

import Html exposing (Html, a, button, div, form, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Locations exposing (AddressLocations)
import OrderDetails
import PageData exposing (OrderData)
import Paginate exposing (Paginated)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Zone)
import Views.Format as Format
import Views.Pager as Pager
import Views.Utils exposing (icon, routeLinkAttributes)



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
        searchForm =
            form [ onSubmit SubmitSearch ]
                [ div [ class "input-group input-group-sm" ]
                    [ input
                        [ class "form-control"
                        , value query
                        , type_ "search"
                        , onInput InputQuery
                        ]
                        []
                    , div [ class "input-group-append" ]
                        [ button [ class "btn btn-primary", type_ "submit" ] [ icon "search" ] ]
                    ]
                ]

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

        orderTable =
            if Paginate.isLoading orders then
                div [ class "p-4 text-center" ]
                    [ text "Loading..."
                    , icon "spinner fa-spin ml-2"
                    ]

            else if Paginate.hasNone orders then
                div [ class "p-4 text-center" ]
                    [ text "No Orders Found" ]

            else
                table [ class "table table-striped table-sm my-2 order-table" ]
                    [ header
                    , tbody [] <| List.map renderOrder <| Paginate.getCurrent orders
                    ]
    in
    [ div [ class "d-flex justify-content-between align-items-center mb-2" ]
        [ searchForm, pager.perPageLinks () ]
    , pager.viewTop ()
    , orderTable
    , pager.viewBottom ()
    ]



-- DETAILS


details : Zone -> Int -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
details zone orderId locations orderDetails =
    [ div [] <| OrderDetails.view zone orderId locations orderDetails
    ]
