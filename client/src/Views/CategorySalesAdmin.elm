module Views.CategorySalesAdmin exposing (..)

import Category exposing (CategoryId(..))
import Dict
import Html exposing (Html, a, br, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import PageData exposing (AdminCategorySaleListData, SaleType(..))
import Time exposing (Zone)
import Views.Format as Format


list : Zone -> AdminCategorySaleListData -> List (Html msg)
list zone { sales, categories } =
    let
        renderSale ({ name, saleType, start, end } as sale) =
            tr []
                [ td [] [ text name ]
                , td [] [ renderCategories sale.categories ]
                , td [ class "text-right" ] [ text <| renderSaleType saleType ]
                , td [ class "text-center" ] [ text <| Format.date zone start ]
                , td [ class "text-center" ] [ text <| Format.date zone end ]
                , td [] [ a [] [ text "Edit" ] ]
                ]

        renderCategories =
            List.filterMap (\(CategoryId i) -> Dict.get i categories)
                >> List.sort
                >> List.map text
                >> List.intersperse (br [] [])
                >> div []

        renderSaleType t =
            case t of
                FlatSale c ->
                    Format.cents c

                PercentSale p ->
                    String.fromInt p ++ "%"
    in
    [ div [ class "form-group mb-4" ]
        [ a [ class "btn btn-primary" ]
            [ text "New Category Sale" ]
        ]
    , table [ class "table table-sm table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Categories" ]
                , th [ class "text-right" ] [ text "Discount" ]
                , th [ class "text-center" ] [ text "Start Date" ]
                , th [ class "text-center" ] [ text "End Date" ]
                , th [] []
                ]
            ]
        , tbody [] <| List.map renderSale sales
        ]
    ]
