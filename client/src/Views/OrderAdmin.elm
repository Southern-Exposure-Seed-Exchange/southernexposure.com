module Views.OrderAdmin exposing (list)

import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Locations exposing (AddressLocations)
import PageData exposing (OrderData)
import Paginate exposing (Paginated)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Zone)
import Views.Format as Format
import Views.Pager as Pager


list : Zone -> AddressLocations -> Paginated OrderData () () -> List (Html msg)
list zone locations orders =
    let
        header =
            thead []
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

                -- TODO: Link to view/refund page
                , td [] [ text "View" ]
                ]

        pager =
            Pager.elements
                { itemDescription = "Orders"
                , pagerAriaLabel = "Orders Table Pages"
                , pagerCssClass = ""
                , pageSizes = [ 25, 50, 100, 250 ]
                , routeConstructor = \{ page, perPage } -> Admin <| OrderList page perPage
                }
                orders
    in
    [ div [ class "text-right mb-2" ] [ pager.perPageLinks () ]
    , pager.viewTop ()
    , table [ class "table table-striped table-sm my-2" ]
        [ header
        , tbody [] <| List.map renderOrder <| Paginate.getCurrent orders
        ]
    , pager.viewBottom ()
    ]
