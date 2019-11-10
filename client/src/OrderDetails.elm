module OrderDetails exposing
    ( addressCard
    , orderTable
    , view
    )

import Address
import Html exposing (..)
import Html.Attributes exposing (class, colspan)
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), centsMap)
import PageData
import Product
import Time
import Views.Format as Format


view : Time.Zone -> Int -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
view zone orderId locations orderDetails =
    [ h1 []
        [ text <| "Order #" ++ String.fromInt orderId
        , small [] [ text <| " " ++ Format.date zone orderDetails.order.createdAt ]
        ]
    , hr [] []
    , div [ class "row mb-3" ] <|
        case orderDetails.billingAddress of
            Nothing ->
                [ div [ class "col-12" ]
                    [ addressCard locations "Shipping Details" orderDetails.shippingAddress ]
                ]

            Just billingAddress ->
                [ div [ class "col-sm-6 mb-2" ]
                    [ addressCard locations "Shipping Details" orderDetails.shippingAddress ]
                , div [ class "col-sm-6" ]
                    [ addressCard locations "Billing Details" billingAddress ]
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


type Tuple7 a b c d e f g
    = Tuple7 a b c d e f g


orderTable : PageData.OrderDetails -> Html msg
orderTable ({ order, lineItems, products } as details) =
    let
        productRow product =
            tr []
                [ td [] [ Product.nameWithLotSize product product ]
                , td [ class "text-right" ] [ text <| String.fromInt product.quantity ]
                , td [ class "text-right" ] [ text <| Format.cents product.price ]
                , td [ class "text-right" ] [ text <| Format.cents (productTotal product) ]
                ]

        productBlock product =
            div []
                [ h5 [] [ Product.nameWithLotSize product product ]
                , div [ class "d-flex" ]
                    [ span []
                        [ text <| Format.cents product.price
                        , text " x "
                        , text <| String.fromInt product.quantity
                        ]
                    , span [ class "ml-auto font-weight-bold item-total" ]
                        [ text <| Format.cents <| productTotal product
                        ]
                    ]
                ]

        productTotal product =
            centsMap ((*) product.quantity) product.price

        orderTotals =
            PageData.orderTotals details

        subTotal =
            orderTotals.subTotal

        (Tuple7 maybeShippingCharge maybeStoreCredit maybeMemberDiscount maybePriorityShipping maybeCouponDiscount refunds surcharges) =
            List.foldl
                (\lineItem (Tuple7 maybeShipping maybeCredit maybeMember maybePriority maybeCoupon rs ss) ->
                    case lineItem.itemType of
                        PageData.Shipping ->
                            Tuple7 (Just lineItem)
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                rs
                                ss

                        PageData.StoreCredit ->
                            Tuple7 maybeShipping
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybeMember
                                maybePriority
                                maybeCoupon
                                rs
                                ss

                        PageData.MemberDiscount ->
                            Tuple7 maybeShipping
                                maybeCredit
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybePriority
                                maybeCoupon
                                rs
                                ss

                        PageData.PriorityShipping ->
                            Tuple7 maybeShipping
                                maybeCredit
                                maybeMember
                                (Just lineItem)
                                maybeCoupon
                                rs
                                ss

                        PageData.CouponDiscount ->
                            Tuple7 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                rs
                                ss

                        PageData.Surcharge ->
                            Tuple7 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                rs
                                (lineItem :: ss)

                        PageData.Refund ->
                            Tuple7 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                ({ lineItem | amount = centsMap negate lineItem.amount } :: rs)
                                ss
                )
                (Tuple7 Nothing Nothing Nothing Nothing Nothing [] [])
                lineItems

        tableFooter =
            tfoot [] <|
                [ footerRow "font-weight-bold" "Sub-Total" subTotal
                , htmlOrBlank chargeRow maybeShippingCharge
                , htmlOrBlank chargeRow maybePriorityShipping
                ]
                    ++ List.map chargeRow surcharges
                    ++ [ taxRow
                       , htmlOrBlank chargeRow maybeStoreCredit
                       , htmlOrBlank chargeRow maybeMemberDiscount
                       , htmlOrBlank chargeRow maybeCouponDiscount
                       ]
                    ++ List.map chargeRow refunds
                    ++ [ footerRow "font-weight-bold" "Total" total
                       ]

        total =
            orderTotals.total

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
    div []
        [ table [ class "d-none d-sm-table table table-striped table-sm" ]
            [ thead []
                [ tr [ class "font-weight-bold" ]
                    [ th [] [ text "Product" ]
                    , th [ class "text-right" ] [ text "Quantity" ]
                    , th [ class "text-right" ] [ text "Price" ]
                    , th [ class "text-right" ] [ text "Total" ]
                    ]
                ]
            , tbody [] <| List.map productRow products
            , tableFooter
            ]
        , div [ class "order-details-blocks d-sm-none" ] <|
            List.map productBlock products
                ++ [ table [ class "table table-striped table-sm" ] [ tableFooter ] ]
        ]
