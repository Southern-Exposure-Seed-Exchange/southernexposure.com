module Views.AdminDashboard exposing (view)

import Decimal
import Html exposing (Html, a, div, h5, li, p, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (class)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..))
import PageData exposing (AdminDashboardData, DashboardCustomer, DashboardGraphData, DashboardOrder)
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Zone)
import Views.Format as Format
import Views.Utils exposing (icon, routeLinkAttributes)


view : Zone -> WebData AdminDashboardData -> AddressLocations -> List (Html msg)
view zone dashboardData locations =
    [ text "This page will eventually contain sections like:"
    , ul []
        [ li [] [ text "Monthly Order Totals" ]
        ]
    , div [ class "row mb-4 justify-content-center" ]
        [ section "Recent Orders" "col-md-8" (.orders >> orderTable zone locations) dashboardData
        , section "Newest Customers" "col-md-4" (.customers >> customerTable) dashboardData
        , section "Daily Sales" "col-12 col-xl-10" (.dailySales >> dailySalesGraph zone) dashboardData
        ]
    ]


section : String -> String -> (a -> Html msg) -> WebData a -> Html msg
section name classes renderer webData =
    let
        cardBody =
            case webData of
                RemoteData.Success resp ->
                    renderer resp

                RemoteData.Loading ->
                    div [ class "m-4 p-4 text-center" ]
                        [ div [] [ icon "spinner fa-spin fa-5x" ]
                        , div [ class "mt-4 font-weight-bold" ] [ text "Loading..." ]
                        ]

                RemoteData.Failure _ ->
                    div [ class "m-4 p-4 text-center" ]
                        [ div [] [ text "An error occured while loading the report data." ]
                        ]

                RemoteData.NotAsked ->
                    p [] [ text "Programming Error: Dashboard reports not requested from server." ]
    in
    div [ class <| classes ++ " mb-3" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ h5 [ class "mb-0" ] [ text name ] ]
            , div [ class "card-body p-0" ] [ cardBody ]
            ]
        ]


orderTable : Zone -> AddressLocations -> List DashboardOrder -> Html msg
orderTable zone locations orders =
    let
        renderOrder : DashboardOrder -> Html msg
        renderOrder order =
            tr [ class "text-center" ]
                [ td [] [ text <| Format.date zone order.date ]
                , td [] [ text order.customer ]
                , td [] [ text <| Maybe.withDefault "" <| Locations.regionName locations order.state ]
                , td [ class "text-right" ] [ text <| Format.cents order.total ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| AdminOrderDetails order.id)
                        [ text "View" ]
                    ]
                ]
    in
    table [ class "table table-sm table-striped mb-0" ]
        [ tbody [] <| List.map renderOrder orders
        ]


customerTable : List DashboardCustomer -> Html msg
customerTable customers =
    let
        renderCustomer { id, email } =
            tr []
                [ td [] [ text email ]
                , td []
                    [ a (routeLinkAttributes (Admin <| CustomerEdit id))
                        [ text "View" ]
                    ]
                ]
    in
    table [ class "table table-sm table-striped mb-0" ]
        [ tbody [] <| List.map renderCustomer customers
        ]


dailySalesGraph : Zone -> List DashboardGraphData -> Html msg
dailySalesGraph zone dataPoints =
    let
        centsToFloat (Cents c) =
            Decimal.fromInt c
                |> Decimal.mul (Decimal.fromIntWithExponent 1 -2)
                |> Decimal.toFloat

        chartConfig : LineChart.Config DashboardGraphData msg
        chartConfig =
            { x = Axis.time zone 1500 "" (.date >> Time.posixToMillis >> toFloat)
            , y = Axis.default 600 "" (.amount >> centsToFloat)
            , container = containerConfig
            , intersection = Intersection.default
            , interpolation = Interpolation.monotone
            , legends = Legends.none
            , events = Events.default
            , area = Area.normal 0.5
            , grid = Grid.default
            , line = Line.default
            , dots = Dots.default
            , junk = Junk.default
            }

        containerConfig : Container.Config msg
        containerConfig =
            Container.custom
                { attributesHtml = []
                , attributesSvg = []
                , size = Container.relative
                , margin = Container.Margin 10 0 30 70
                , id = "daily-sales-graph"
                }
    in
    LineChart.viewCustom chartConfig
        [ LineChart.line Colors.green Dots.circle "" dataPoints ]
