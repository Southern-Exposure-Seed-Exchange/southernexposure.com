module Checkout
    exposing
        ( Form
        , initial
        , initialWithDefaults
        , Msg
        , OutMsg(..)
        , update
        , getCustomerDetails
        , view
        , successView
        )

import Html exposing (..)
import Html.Attributes exposing (class, type_, colspan, src, for, id, rows, value, href, target, selected, checked)
import Html.Events exposing (onSubmit, onInput, onCheck, targetValue, on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Time.DateTime as DateTime
import Address exposing (AddressId(..))
import Api
import Locations exposing (AddressLocations, Region)
import Models.Fields exposing (Cents(..), centsToString, centsMap2, centsMap, milligramsToString)
import PageData
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.Images as Images


-- Model


type CheckoutAddress
    = ExistingAddress AddressId
    | NewAddress Address.Form


type alias Form =
    { shippingAddress : CheckoutAddress
    , makeShippingDefault : Bool
    , billingAddress : CheckoutAddress
    , makeBillingDefault : Bool
    , billingSameAsShipping : Bool
    , comment : String
    }


initial : Form
initial =
    { shippingAddress = NewAddress Address.initialForm
    , makeShippingDefault = False
    , billingAddress = NewAddress Address.initialForm
    , makeBillingDefault = False
    , billingSameAsShipping = False
    , comment = ""
    }


initialWithDefaults : List Address.Model -> List Address.Model -> Form
initialWithDefaults shippingAddresses billingAddresses =
    let
        withDefault addrs =
            findBy .isDefault addrs
                |> Maybe.andThen (.id >> Maybe.map ExistingAddress)
                |> Maybe.withDefault (NewAddress Address.initialForm)

        shippingAddress =
            withDefault shippingAddresses

        billingAddress =
            withDefault billingAddresses

        isNew a =
            case a of
                NewAddress _ ->
                    True

                ExistingAddress _ ->
                    False
    in
        { shippingAddress = shippingAddress
        , makeShippingDefault = isNew shippingAddress
        , billingAddress = billingAddress
        , makeBillingDefault = isNew billingAddress
        , billingSameAsShipping = False
        , comment = ""
        }



-- Update


type Msg
    = SelectShipping Int
    | ToggleShippingDefault Bool
    | ShippingMsg Address.Msg
    | SelectBilling Int
    | ToggleBillingDefault Bool
    | BillingMsg Address.Msg
    | BillingSameAsShipping Bool
    | Comment String
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors Int))
    | RefreshDetails (WebData PageData.CheckoutDetails)


type OutMsg
    = OrderCompleted Int
    | DetailsRefreshed PageData.CheckoutDetails


update : Msg -> Form -> AuthStatus -> ( Form, Maybe OutMsg, Cmd Msg )
update msg model authStatus =
    case msg of
        SelectShipping addressId ->
            { model
                | shippingAddress = selectAddress addressId
                , makeShippingDefault = False
            }
                |> refreshDetails authStatus model

        ToggleShippingDefault makeDefault ->
            { model | makeShippingDefault = makeDefault }
                |> nothingAndNoCommand

        ShippingMsg subMsg ->
            updateAddressForm model.shippingAddress
                subMsg
                model
                (\f -> { model | shippingAddress = f })
                |> refreshDetails authStatus model

        SelectBilling addressId ->
            { model
                | billingAddress = selectAddress addressId
                , makeBillingDefault = False
            }
                |> nothingAndNoCommand

        ToggleBillingDefault makeDefault ->
            { model | makeBillingDefault = makeDefault }
                |> nothingAndNoCommand

        BillingMsg subMsg ->
            updateAddressForm model.billingAddress
                subMsg
                model
                (\f -> { model | billingAddress = f })
                |> nothingAndNoCommand

        BillingSameAsShipping isSame ->
            { model | billingSameAsShipping = isSame }
                |> nothingAndNoCommand

        Comment comment ->
            { model | comment = comment } |> nothingAndNoCommand

        Submit ->
            ( model, Nothing, placeOrder model authStatus )

        SubmitResponse (RemoteData.Success (Ok orderId)) ->
            ( initial, Just (OrderCompleted orderId), Cmd.none )

        -- TODO: Split address errors & update nested models
        SubmitResponse (RemoteData.Success (Err errors)) ->
            model |> nothingAndNoCommand

        SubmitResponse _ ->
            model |> nothingAndNoCommand

        RefreshDetails (RemoteData.Success details) ->
            ( model, Just (DetailsRefreshed details), Cmd.none )

        RefreshDetails _ ->
            model |> nothingAndNoCommand


selectAddress : Int -> CheckoutAddress
selectAddress addressId =
    if addressId == 0 then
        NewAddress Address.initialForm
    else
        ExistingAddress (Address.AddressId addressId)


updateAddressForm : CheckoutAddress -> Address.Msg -> Form -> (CheckoutAddress -> Form) -> Form
updateAddressForm addr msg model updater =
    case addr of
        ExistingAddress _ ->
            model

        NewAddress form ->
            Address.update msg form
                |> (NewAddress >> updater)


refreshDetails : AuthStatus -> Form -> Form -> ( Form, Maybe a, Cmd Msg )
refreshDetails authStatus oldModel newModel =
    let
        fetchCommand token =
            case ( oldModel.shippingAddress, newModel.shippingAddress ) of
                ( ExistingAddress id1, ExistingAddress id2 ) ->
                    if id1 /= id2 then
                        getCustomerDetails RefreshDetails token Nothing Nothing (Just id2)
                    else
                        Cmd.none

                ( NewAddress oldForm, NewAddress newForm ) ->
                    if oldForm.model.country == newForm.model.country then
                        case ( oldForm.model.state, newForm.model.state ) of
                            ( Locations.Custom _, Locations.Custom _ ) ->
                                Cmd.none

                            ( oldState, newState ) ->
                                if oldState /= newState then
                                    getCustomerDetails RefreshDetails
                                        token
                                        (Just newForm.model.country)
                                        (Just newState)
                                        Nothing
                                else
                                    Cmd.none
                    else
                        getCustomerDetails RefreshDetails
                            token
                            (Just newForm.model.country)
                            (Just newForm.model.state)
                            Nothing

                ( _, ExistingAddress id ) ->
                    getCustomerDetails RefreshDetails token Nothing Nothing (Just id)

                ( _, NewAddress form ) ->
                    getCustomerDetails RefreshDetails
                        token
                        (Just form.model.country)
                        (Just form.model.state)
                        Nothing
    in
        case authStatus of
            User.Anonymous ->
                ( newModel, Nothing, Cmd.none )

            User.Authorized user ->
                ( newModel, Nothing, fetchCommand user.authToken )


findBy : (a -> Bool) -> List a -> Maybe a
findBy pred l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x
            else
                findBy pred xs


getCustomerDetails :
    (WebData PageData.CheckoutDetails -> msg)
    -> String
    -> Maybe String
    -> Maybe Region
    -> Maybe AddressId
    -> Cmd msg
getCustomerDetails msg token maybeCountry maybeRegion maybeAddressId =
    let
        data =
            Encode.object
                [ ( "region", encodeMaybe Locations.regionEncoder maybeRegion )
                , ( "country", encodeMaybe Encode.string maybeCountry )
                , ( "addressId", encodeMaybe (\(AddressId i) -> Encode.int i) maybeAddressId )
                ]

        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
        Api.post Api.CheckoutDetailsCustomer
            |> Api.withJsonBody data
            |> Api.withJsonResponse PageData.checkoutDetailsDecoder
            |> Api.withToken token
            |> Api.sendRequest msg


placeOrder : Form -> AuthStatus -> Cmd Msg
placeOrder model authStatus =
    let
        data =
            Encode.object
                [ ( "shippingAddress", encodedShippingAddress )
                , ( "billingAddress", encodedBillingAddress )
                , ( "comment", Encode.string model.comment )
                ]

        encodedShippingAddress =
            encodeAddress model.shippingAddress model.makeShippingDefault

        encodedBillingAddress =
            if model.billingSameAsShipping then
                encodedShippingAddress
            else
                encodeAddress model.billingAddress model.makeBillingDefault

        encodeAddress addr makeDefault =
            case addr of
                ExistingAddress (Address.AddressId id) ->
                    Encode.object
                        [ ( "id", Encode.int id )
                        , ( "makeDefault", Encode.bool makeDefault )
                        ]

                NewAddress form ->
                    let
                        formModel =
                            form.model

                        formModelWithDefault =
                            { formModel | isDefault = makeDefault }

                        formWithDefault =
                            { form | model = formModelWithDefault }
                    in
                        Address.encode formWithDefault

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


view : Form -> AddressLocations -> PageData.CheckoutDetails -> List (Html Msg)
view model locations checkoutDetails =
    let
        billingCard =
            if model.billingSameAsShipping then
                addressCard
                    [ h4 [ class "card-title" ]
                        [ span [ class "mr-4" ] [ text "Billing Details" ]
                        , sameAddressesCheckbox model.billingSameAsShipping
                        ]
                    , billingAddressText
                    ]
            else
                addressForm
                    { cardTitle = "Billing Details"
                    , msg = BillingMsg
                    , selectMsg = SelectBilling
                    , defaultMsg = ToggleBillingDefault
                    , model = model.billingAddress
                    , makeDefault = model.makeBillingDefault
                    , billingSameAsShipping = model.billingSameAsShipping
                    , prefix = "billing"
                    , locations = locations
                    , selectAddresses = checkoutDetails.billingAddresses
                    }

        billingAddressText =
            case model.shippingAddress of
                NewAddress form ->
                    Address.card form.model locations

                ExistingAddress id ->
                    findBy (\a -> a.id == Just id) checkoutDetails.shippingAddresses
                        |> Maybe.map (flip Address.card locations)
                        |> Maybe.withDefault (text "")
    in
        [ h1 [] [ text "Checkout" ]
        , hr [] []
        , form [ onSubmit Submit ]
            [ div [ class "row mb-3" ]
                [ addressForm
                    { cardTitle = "Shipping Details"
                    , msg = ShippingMsg
                    , selectMsg = SelectShipping
                    , defaultMsg = ToggleShippingDefault
                    , model = model.shippingAddress
                    , makeDefault = model.makeShippingDefault
                    , billingSameAsShipping = model.billingSameAsShipping
                    , prefix = "shipping"
                    , locations = locations
                    , selectAddresses = checkoutDetails.shippingAddresses
                    }
                , billingCard
                ]
            , div [ class "mb-3" ]
                [ h4 [] [ text "Order Summary" ]
                , summaryTable checkoutDetails
                ]
            , div [ class "form-group" ]
                [ label [ class "h4", for "commentsTextarea" ]
                    [ text "Additional Comments" ]
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
                [ button [ class "btn btn-primary", type_ "submit" ]
                    [ text "Place Order" ]
                ]
            ]
        ]


type alias AddressFormConfig =
    { cardTitle : String
    , msg : Address.Msg -> Msg
    , selectMsg : Int -> Msg
    , defaultMsg : Bool -> Msg
    , model : CheckoutAddress
    , makeDefault : Bool
    , billingSameAsShipping : Bool
    , prefix : String
    , locations : AddressLocations
    , selectAddresses : List Address.Model
    }


addressForm : AddressFormConfig -> Html Msg
addressForm config =
    let
        sameAsShippingCheckbox =
            if config.prefix == "billing" then
                sameAddressesCheckbox config.billingSameAsShipping
            else
                text ""

        selectHtml =
            if not (List.isEmpty config.selectAddresses) then
                addressSelect
            else
                text ""

        ( newAddress, addressId, addressHtml ) =
            case config.model of
                NewAddress addr ->
                    ( True
                    , Nothing
                    , Address.form addr config.prefix config.locations
                        |> Html.map config.msg
                        |> withDefaultCheckbox addr.model
                    )

                ExistingAddress id ->
                    ( False
                    , Just id
                    , findBy (\a -> a.id == Just id) config.selectAddresses
                        |> Maybe.map (\a -> Address.card a config.locations |> withDefaultCheckbox a)
                        |> Maybe.withDefault (text "")
                    )

        withDefaultCheckbox address content =
            let
                isNewAddressWithExistingAddresses =
                    address.id == Nothing && not (List.isEmpty config.selectAddresses)

                shouldShow =
                    isNewAddressWithExistingAddresses || not address.isDefault

                checkbox =
                    div [ class "form-check" ]
                        [ label [ class "form-check-label" ]
                            [ input
                                [ class "form-check-input"
                                , type_ "checkbox"
                                , checked config.makeDefault
                                , onCheck config.defaultMsg
                                ]
                                []
                            , text "Set as Default Address"
                            ]
                        ]
            in
                div []
                    [ content
                    , if shouldShow then
                        checkbox
                      else
                        text ""
                    ]

        addressSelect =
            config.selectAddresses
                |> List.map
                    (\a ->
                        option
                            [ value <| toString <| fromAddressId a.id
                            , isSelected a
                            ]
                            (addressDescription a)
                    )
                |> (::) (option [ selected newAddress, value "0" ] [ text "Add a New Address..." ])
                |> select [ class "form-control", onSelectInt config.selectMsg ]
                |> List.singleton
                |> div [ class "form-group" ]

        fromAddressId a =
            case a of
                Just (Address.AddressId i) ->
                    i

                _ ->
                    0

        isSelected addr =
            selected <| addr.id == addressId

        addressDescription addr =
            [ b [] [ text <| addr.firstName ++ " " ++ addr.lastName ]
            , text " - "
            , text <|
                String.join ", " <|
                    List.filter (not << String.isEmpty) <|
                        [ addr.companyName
                        , addr.street
                        , addr.addressTwo
                        , addr.city
                        , addr.zipCode ++ " " ++ addr.country
                        ]
            ]

        onSelectInt msg =
            targetValue
                |> Decode.andThen
                    (String.toInt
                        >> Result.map Decode.succeed
                        >> Result.withDefault (Decode.fail "")
                    )
                |> Decode.map msg
                |> on "change"
    in
        addressCard
            [ h4 [ class "card-title" ]
                [ span [ class "mr-4" ] [ text config.cardTitle ]
                , sameAsShippingCheckbox
                ]
            , selectHtml
            , addressHtml
            ]


addressCard : List (Html msg) -> Html msg
addressCard contents =
    div [ class "col-6" ]
        [ div [ class "card" ] [ div [ class "card-body pt-3" ] contents ] ]


sameAddressesCheckbox : Bool -> Html Msg
sameAddressesCheckbox billingSameAsShipping =
    small [ class "d-inline form-check" ]
        [ label [ class "form-check-label" ]
            [ input
                [ class "form-check-input"
                , type_ "checkbox"
                , checked billingSameAsShipping
                , onCheck BillingSameAsShipping
                ]
                []
            , text "Same as Shipping Address"
            ]
        ]


summaryTable : PageData.CheckoutDetails -> Html msg
summaryTable ({ items, charges } as checkoutDetails) =
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
            PageData.cartTotals checkoutDetails

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
