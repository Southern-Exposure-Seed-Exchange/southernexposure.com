module Views.CouponAdmin exposing (list)

import Html exposing (Html, a, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import PageData exposing (CouponType(..))
import Time
import Views.Admin as Admin
import Views.Format as Format


{-| TODO: Link up the Add Button & Edit Columns
-}
list : Time.Zone -> PageData.AdminCouponListData -> List (Html msg)
list zone { coupons } =
    let
        headerRow =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , th [] [ text "Code" ]
                , th [] [ text "Active" ]
                , th [] [ text "Expires" ]
                , th [] [ text "Edit" ]
                ]

        renderCoupon coupon =
            tr []
                [ td [] [ text coupon.name ]
                , td [] [ text <| renderType coupon.discount ]
                , td [] [ text coupon.code ]
                , td [ class "text-center" ] [ Admin.activeIcon coupon.isActive ]
                , td [] [ text <| Format.date zone coupon.expires ]
                , td [] [ text "Edit" ]
                ]

        renderType couponType =
            case couponType of
                FreeShipping ->
                    "Free Shipping"

                FlatDiscount cents ->
                    Format.cents cents

                PercentageDiscount percent ->
                    String.fromInt percent ++ "%"
    in
    [ a [ class "mb-3 btn btn-primary" ] [ text "New Coupon" ]
    , table [ class "table table-sm table-striped" ]
        [ thead [] [ headerRow ]
        , tbody [] <| List.map renderCoupon coupons
        ]
    ]
