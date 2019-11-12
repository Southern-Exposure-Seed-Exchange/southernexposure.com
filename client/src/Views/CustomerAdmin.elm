module Views.CustomerAdmin exposing
    ( SearchForm
    , SearchMsg
    , initialSearchForm
    , list
    , updateSearchForm
    )

import Html exposing (Html, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import PageData exposing (CustomerData)
import Paginate exposing (Paginated)
import Routing exposing (AdminRoute(..), Route(..))
import Views.Admin as Admin
import Views.Pager as Pager


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

                -- TODO: Link to CustomerEdit page
                , td [] [ text "Edit" ]
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
