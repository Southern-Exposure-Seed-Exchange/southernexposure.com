module OrderDetails exposing
    ( addressCard
    , orderTable
    , view
    )

import Address
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), centsMap, centsMap2, milligramsToString)
import PageData
import Time
import Views.Format as Format


view : Time.Zone -> Int -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
view zone orderId locations orderDetails =
    [ h1 []
        [ text <| "Order #" ++ String.fromInt orderId
        , small [] [ text <| " " ++ Format.date zone orderDetails.order.createdAt ]
        ]
    , hr [] []
    , div [ class "row mb-3" ]
        [ div [ class "col-6" ]
            [ addressCard locations "Shipping Details" orderDetails.shippingAddress ]
        , div [ class "col-6" ]
            [ addressCard locations "Billing Details" orderDetails.billingAddress ]
        ]
    , orderTable orderDetails
    ]


addressCard : AddressLocations -> String -> Address.Model -> Html msg
addressCard locations titleText address =
    div [ class "card h-100" ]
        [ div [ class "card-body" ]
            [ h4 [ class "card-title" ] [ text titleText ]
            , Address.card address locations
            ]
        ]


type Tuple6 a b c d e f
    = Tuple6 a b c d e f


orderTable : PageData.OrderDetails -> Html msg
orderTable ({ order, lineItems, products } as details) =
    let
        productRow product =
            tr []
                [ td [] [ text <| product.name ++ " " ++ milligramsToString product.weight ++ "g" ]
                , td [ class "text-right" ] [ text <| String.fromInt product.quantity ]
                , td [ class "text-right" ] [ text <| Format.cents product.price ]
                , td [ class "text-right" ] [ text <| Format.cents (productTotal product) ]
                ]

        productTotal product =
            centsMap ((*) product.quantity) product.price

        orderTotals =
            PageData.orderTotals details

        subTotal =
            orderTotals.subTotal

        (Tuple6 maybeShippingCharge maybeStoreCredit maybeMemberDiscount maybePriorityShipping maybeCouponDiscount surcharges) =
            List.foldl
                (\lineItem (Tuple6 maybeShipping maybeCredit maybeMember maybePriority maybeCoupon ss) ->
                    case lineItem.itemType of
                        PageData.Shipping ->
                            Tuple6 (Just lineItem)
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                ss

                        PageData.StoreCredit ->
                            Tuple6 maybeShipping
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybeMember
                                maybePriority
                                maybeCoupon
                                ss

                        PageData.MemberDiscount ->
                            Tuple6 maybeShipping
                                maybeCredit
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybePriority
                                maybeCoupon
                                ss

                        PageData.PriorityShipping ->
                            Tuple6 maybeShipping
                                maybeCredit
                                maybeMember
                                (Just lineItem)
                                maybeCoupon
                                ss

                        PageData.CouponDiscount ->
                            Tuple6 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                ss

                        PageData.Surcharge ->
                            Tuple6 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                (lineItem :: ss)
                )
                (Tuple6 Nothing Nothing Nothing Nothing Nothing [])
                lineItems

        total =
            orderTotals.total

        maybeAddCents maybeCents =
            centsMap2 (+) (Maybe.map .amount maybeCents |> Maybe.withDefault (Cents 0))

        taxRow =
            if orderTotals.tax == Cents 0 then
                text ""

            else
                footerRow "" order.taxDescription orderTotals.tax

        chargeRow { description, amount } =
            footerRow "" description amount

        footerRow rowClass description amount =
            tr [ class rowClass ]
                [ td [ class "text-right", colspan 3 ] [ text <| description ++ ":" ]
                , td [ class "text-right" ] [ text <| Format.cents amount ]
                ]

        htmlOrBlank f maybe =
            case maybe of
                Nothing ->
                    text ""

                Just a ->
                    f a
    in
    table [ class "table table-striped table-sm" ]
        [ thead []
            [ tr [ class "font-weight-bold" ]
                [ th [] [ text "Product" ]
                , th [ class "text-right" ] [ text "Quantity" ]
                , th [ class "text-right" ] [ text "Price" ]
                , th [ class "text-right" ] [ text "Total" ]
                ]
            ]
        , tbody [] <| List.map productRow products
        , tfoot [] <|
            [ footerRow "font-weight-bold" "Sub-Total" subTotal
            , htmlOrBlank chargeRow maybeShippingCharge
            , htmlOrBlank chargeRow maybePriorityShipping
            ]
                ++ List.map chargeRow surcharges
                ++ [ taxRow
                   , htmlOrBlank chargeRow maybeStoreCredit
                   , htmlOrBlank chargeRow maybeMemberDiscount
                   , htmlOrBlank chargeRow maybeCouponDiscount
                   , footerRow "font-weight-bold" "Total" total
                   ]
        ]
