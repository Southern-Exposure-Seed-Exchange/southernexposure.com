module Checkout
    exposing
        ( Form
        , initial
        , Msg
        , update
        , view
        , successView
        )

import Html exposing (..)
import Html.Attributes exposing (class, type_, colspan, src, for, id, rows, value, href, target)
import Html.Events exposing (onSubmit, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Time.DateTime as DateTime
import Address
import Api
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), centsToString, centsMap2, centsMap, milligramsToString)
import PageData
import User exposing (AuthStatus)
import Views.Images as Images


-- Model


type alias Form =
    { shippingForm : Address.Form
    , billingForm : Address.Form
    , comment : String
    }


initial : Form
initial =
    { shippingForm = Address.initialForm
    , billingForm = Address.initialForm
    , comment = ""
    }



-- Update


type Msg
    = ShippingMsg Address.Msg
    | BillingMsg Address.Msg
    | Comment String
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors Int))


update : Msg -> Form -> AuthStatus -> ( Form, Maybe Int, Cmd Msg )
update msg model authStatus =
    case msg of
        ShippingMsg subMsg ->
            Address.update subMsg model.shippingForm
                |> \f ->
                    { model | shippingForm = f }
                        |> nothingAndNoCommand

        BillingMsg subMsg ->
            Address.update subMsg model.billingForm
                |> \f ->
                    { model | billingForm = f }
                        |> nothingAndNoCommand

        Comment comment ->
            { model | comment = comment } |> nothingAndNoCommand

        Submit ->
            ( model, Nothing, placeOrder model authStatus )

        SubmitResponse (RemoteData.Success (Ok orderId)) ->
            ( initial, Just orderId, Cmd.none )

        -- TODO: Split address errors & update nested models
        SubmitResponse (RemoteData.Success (Err errors)) ->
            model |> nothingAndNoCommand

        SubmitResponse _ ->
            model |> nothingAndNoCommand


nothingAndNoCommand : a -> ( a, Maybe b, Cmd msg )
nothingAndNoCommand m =
    ( m, Nothing, Cmd.none )


placeOrder : Form -> AuthStatus -> Cmd Msg
placeOrder model authStatus =
    let
        data =
            Encode.object
                [ ( "shippingAddress", Address.encode model.shippingForm )
                , ( "billingAddress", Address.encode model.billingForm )
                , ( "comment", Encode.string model.comment )
                ]

        decoder =
            Decode.field "orderId" Decode.int
    in
        case authStatus of
            User.Anonymous ->
                Cmd.none

            User.Authorized user ->
                Api.post Api.CheckoutPlaceOrderCustomer
                    |> Api.withToken user.authToken
                    |> Api.withJsonBody data
                    |> Api.withJsonResponse decoder
                    |> Api.withErrorHandler SubmitResponse



-- View


view : Form -> AddressLocations -> PageData.CartDetails -> List (Html Msg)
view model locations cartDetails =
    [ h1 [] [ text "Checkout" ]
    , hr [] []
    , form [ onSubmit Submit ]
        [ div [ class "row mb-3" ]
            [ addressForm "Shipping Details" ShippingMsg model.shippingForm "shipping" locations
            , addressForm "Billing Details" BillingMsg model.billingForm "billing" locations
            ]
        , div [ class "mb-3" ]
            [ h4 [] [ text "Order Summary" ]
            , summaryTable cartDetails
            ]
        , div [ class "form-group" ]
            [ label [ class "h4", for "commentsTextarea" ] [ text "Additional Comments" ]
            , textarea
                [ id "commentsTextarea"
                , class "form-control"
                , rows 4
                , onInput Comment
                , value model.comment
                ]
                []
            ]
        , div [ class "form-group text-right" ]
            [ button [ class "btn btn-primary", type_ "submit" ] [ text "Place Order" ] ]
        ]
    ]


addressForm : String -> (Address.Msg -> msg) -> Address.Form -> String -> AddressLocations -> Html msg
addressForm cardTitle msg model prefix locations =
    div [ class "col-6" ]
        [ div [ class "card" ]
            [ div [ class "card-body pt-3" ]
                [ h4 [ class "card-title" ] [ text cardTitle ]
                , Html.map msg <| Address.form model prefix locations
                ]
            ]
        ]


summaryTable : PageData.CartDetails -> Html msg
summaryTable ({ items, charges } as cartDetails) =
    let
        tableHeader =
            thead [ class "font-weight-bold" ]
                [ tr []
                    [ td [] [ text "" ]
                    , td [] [ text "Name" ]
                    , td [ class "text-center" ] [ text "Quantity" ]
                    , td [ class "text-right" ] [ text "Item Price" ]
                    , td [ class "text-right" ] [ text "Item Total" ]
                    ]
                ]

        productRow { product, variant, quantity } =
            tr []
                [ td [ class "align-middle text-center" ]
                    [ img [ src << Images.media <| "products/" ++ product.imageURL ] []
                    ]
                , td []
                    [ div [ class "font-weight-bold" ] [ text product.name ]
                    , small [ class "text-muted" ]
                        [ text <| "Item #" ++ product.baseSKU ++ variant.skuSuffix ]
                    ]
                , td [ class "text-center" ] [ text <| toString quantity ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString variant.price ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString (centsMap ((*) quantity) variant.price) ]
                ]

        tableFooter =
            tfoot [] <|
                subTotalRow
                    :: maybeChargeRow charges.shippingMethod
                    :: List.map chargeRow charges.surcharges
                    ++ [ taxRow
                       , footerRow "Total" totals.total "font-weight-bold"
                       ]

        subTotalRow =
            if totals.subTotal /= totals.total then
                footerRow "Sub-Total" totals.subTotal "font-weight-bold"
            else
                text ""

        taxRow =
            if charges.tax.amount == Cents 0 then
                text ""
            else
                chargeRow charges.tax

        chargeRow { description, amount } =
            footerRow description amount ""

        maybeChargeRow =
            Maybe.map chargeRow >> Maybe.withDefault (text "")

        footerRow content amount rowClass =
            tr [ class rowClass ]
                [ td [ colspan 4, class "text-right" ] [ text <| content ++ ":" ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString amount ]
                ]

        totals =
            PageData.cartTotals cartDetails

        centsMap f (Cents c) =
            Cents (f c)
    in
        table [ class "table table-striped table-sm checkout-products-table" ]
            [ tableHeader
            , tbody [] <| List.map productRow items
            , tableFooter
            ]


successView : Int -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
successView orderId locations orderDetails =
    let
        contactLink =
            a
                [ href <|
                    "mailto:gardens@southernexposure.com"
                        ++ "?subject=SESE+Website+Contact(Order+#"
                        ++ toString orderId
                        ++ ")"
                , target "_blank"
                ]
                [ text "contact us" ]

        addressCard titleText address =
            div [ class "card h-100" ]
                [ div [ class "card-body" ]
                    [ h4 [ class "card-title" ] [ text titleText ]
                    , Address.card address locations
                    ]
                ]

        commentHtml =
            if not (String.isEmpty orderDetails.order.comment) then
                p []
                    [ b [] [ text "Comment: " ]
                    , text orderDetails.order.comment
                    ]
            else
                text ""

        formattedDate =
            [ DateTime.month orderDate
            , DateTime.day orderDate
            , DateTime.year orderDate % 100
            ]
                |> List.map toString
                |> String.join "/"

        orderDate =
            orderDetails.order.createdAt
    in
        [ h1 [] [ text "Order Complete" ]
        , hr [] []
        , h2 [] [ text "Thanks for your order!" ]
        , p [] [ text "We will review your order today & will get in touch with you if we need any clarifications." ]
        , p []
            [ text "Please "
            , contactLink
            , text " as soon as possible if you have special shipping requirements."
            ]
        , p [ class "text-center font-weight-bold text-primary mb-2" ]
            [ text "We will email you a tracking number once your order has shipped."
            ]
        , h3 []
            [ text <| "Order #" ++ toString orderId
            , small [] [ text <| " " ++ formattedDate ]
            ]
        , div [ class "row mb-3" ]
            [ div [ class "col-6" ]
                [ addressCard "Shipping Details" orderDetails.shippingAddress ]
            , div [ class "col-6" ]
                [ addressCard "Billing Details" orderDetails.billingAddress ]
            ]
        , commentHtml
        , orderTable orderDetails
        ]


orderTable : PageData.OrderDetails -> Html msg
orderTable ({ order, lineItems, products } as details) =
    let
        productRow product =
            tr []
                [ td [] [ text <| product.name ++ " " ++ milligramsToString product.weight ++ "g" ]
                , td [ class "text-right" ] [ text <| toString product.quantity ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString product.price ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString (productTotal product) ]
                ]

        productTotal product =
            centsMap ((*) product.quantity) product.price

        orderTotals =
            PageData.orderTotals details

        subTotal =
            orderTotals.subTotal

        ( maybeShippingCharge, surcharges ) =
            List.foldl
                (\lineItem ( maybeShipping, ss ) ->
                    case lineItem.itemType of
                        PageData.Shipping ->
                            ( Just lineItem, ss )

                        PageData.Surcharge ->
                            ( maybeShipping, lineItem :: ss )
                )
                ( Nothing, [] )
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
                , td [ class "text-right" ] [ text <| "$" ++ centsToString amount ]
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
                ]
                    ++ List.map chargeRow surcharges
                    ++ [ taxRow
                       , footerRow "font-weight-bold" "Total" total
                       ]
            ]
