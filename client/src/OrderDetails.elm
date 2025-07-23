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
    , h2 [] [text <| "Status: " ++ orderDetails.order.status ]
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
    , if orderDetails.deliveryData == [] then
        text ""
      else
        div [ class "row mb-3" ] [
            div [ class "col-12" ]
                [ deliveryCard orderDetails.deliveryData 
                ]
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

deliveryCard : List(PageData.DeliveryData) -> Html msg
deliveryCard deliveryData =
    div [ class "card h-100" ]
        [ div [ class "card-body" ]
            [ h4 [ class "card-title" ] [ text "Delivery" ]
            , ul []
                (List.map (\d ->
                    (li []
                        [ text <| "Carrier: " ++ d.trackCarrier
                        , br [] []
                        , text <| "Tracking Number: " ++ d.trackNumber
                        , br [] []
                        , text <| "Pickup Date: " ++ d.trackPickupDate
                        ])
                    )
                    deliveryData
                )
            ]
        ]

type Tuple8 a b c d e f g h
    = Tuple8 a b c d e f g h


orderTable : PageData.OrderDetails -> Html msg
orderTable ({ lineItems, products } as details) =
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

        (Tuple8 maybeShippingCharge maybeStoreCredit maybeMemberDiscount maybePriorityShipping maybeCouponDiscount maybeTaxLine refunds surcharges) =
            List.foldl
                (\lineItem (Tuple8 maybeShipping maybeCredit maybeMember maybePriority maybeCoupon maybeTax rs ss) ->
                    case lineItem.itemType of
                        PageData.Shipping ->
                            Tuple8 (Just lineItem)
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                maybeTax
                                rs
                                ss

                        PageData.StoreCredit ->
                            Tuple8 maybeShipping
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybeMember
                                maybePriority
                                maybeCoupon
                                maybeTax
                                rs
                                ss

                        PageData.MemberDiscount ->
                            Tuple8 maybeShipping
                                maybeCredit
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybePriority
                                maybeCoupon
                                maybeTax
                                rs
                                ss

                        PageData.PriorityShipping ->
                            Tuple8 maybeShipping
                                maybeCredit
                                maybeMember
                                (Just lineItem)
                                maybeCoupon
                                maybeTax
                                rs
                                ss

                        PageData.CouponDiscount ->
                            Tuple8 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                (Just { lineItem | amount = centsMap negate lineItem.amount })
                                maybeTax
                                rs
                                ss

                        PageData.Surcharge ->
                            Tuple8 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                maybeTax
                                rs
                                (lineItem :: ss)

                        PageData.Refund ->
                            Tuple8 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                maybeTax
                                ({ lineItem | amount = centsMap negate lineItem.amount } :: rs)
                                ss

                        PageData.Tax ->
                            Tuple8 maybeShipping
                                maybeCredit
                                maybeMember
                                maybePriority
                                maybeCoupon
                                (Just lineItem)
                                rs
                                ss
                )
                (Tuple8 Nothing Nothing Nothing Nothing Nothing Nothing [] [])
                lineItems

        tableFooter =
            tfoot [] <|
                [ footerRow "font-weight-bold" "Sub-Total" subTotal
                , htmlOrBlank chargeRow maybeShippingCharge
                , htmlOrBlank chargeRow maybePriorityShipping
                ]
                    ++ List.map chargeRow surcharges
                    ++ [ htmlOrBlank chargeRow maybeTaxLine
                       , htmlOrBlank chargeRow maybeStoreCredit
                       , htmlOrBlank chargeRow maybeMemberDiscount
                       , htmlOrBlank chargeRow maybeCouponDiscount
                       ]
                    ++ List.map chargeRow refunds
                    ++ [ footerRow "font-weight-bold" "Total" total
                       ]

        total =
            orderTotals.total

        chargeRow { description, amount } =
            footerRow "" description amount

        footerRow rowClass description amount =
            tr [ class rowClass ]
                [ td [ class "text-right", colspan 3 ] [ text <| description ++ ":" ]
                , td [ class "text-right" ] [ text <| Format.cents amount ]
                ]

        htmlOrBlank : ({ a | description : String, amount : Cents } -> Html msg) -> Maybe { a | description : String, amount : Cents } -> Html msg
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
