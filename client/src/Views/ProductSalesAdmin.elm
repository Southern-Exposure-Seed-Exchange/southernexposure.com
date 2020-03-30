module Views.ProductSalesAdmin exposing (list)

import Dict
import Html exposing (Html, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import PageData exposing (AdminProductSaleListData)
import Product exposing (ProductVariantId(..))
import Time exposing (Zone)
import Views.Admin as Admin
import Views.Format as Format


list : Zone -> AdminProductSaleListData -> List (Html msg)
list zone { sales, variants } =
    let
        renderSale { variant, price, start, end } =
            let
                (ProductVariantId rawVariantId) =
                    variant

                ( sku, name, active ) =
                    case Dict.get rawVariantId variants of
                        Nothing ->
                            ( "<no data>", text "<no data>", False )

                        Just variantData ->
                            ( variantData.sku
                            , Product.nameWithLotSize variantData variantData
                            , variantData.isActive
                            )
            in
            tr []
                [ td [] [ text sku ]
                , td [] [ name ]
                , td [ class "text-center" ] [ Admin.activeIcon active ]
                , td [ class "text-right" ] [ text <| Format.cents price ]
                , td [ class "text-center" ] [ text <| Format.date zone start ]
                , td [ class "text-center" ] [ text <| Format.date zone end ]
                , td [] [ text "Edit" ]
                ]
    in
    [ table [ class "table table-striped table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "SKU" ]
                , th [] [ text "Name" ]
                , th [] [ text "Product Active" ]
                , th [] [ text "Sale Price" ]
                , th [] [ text "Start Date" ]
                , th [] [ text "End Date" ]
                , td [] [ text "" ]
                ]
            ]
        , tbody [] <| List.map renderSale sales
        ]
    ]
