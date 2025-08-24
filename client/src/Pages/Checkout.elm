module Pages.Checkout exposing
    ( Form
    , Msg
    , OutMsg(..)
    , getAnonymousDetails
    , getCustomerDetails
    , initial
    , initialWithDefaults
    , subscriptions
    , successView
    , update
    , validateCart
    , view
    )

import Components.Address.Address as Address exposing (AddressId(..))
import Components.Alert as Alert exposing (defaultAlert)
import Components.Aria as Aria
import Components.Button as Button exposing (defaultButton)
import Components.HorizontalForm exposing (genericErrorText)
import Components.OrderDetails as OrderDetails
import Components.Shared exposing (receiptProductMobileView, receiptTotalMobileView)
import Components.Svg exposing (..)
import Data.Api as Api
import Data.Fields exposing (Cents(..), centsFromString, centsMap, centsMap2, imageToSrcSet, imgSrcFallback, lotSizeToString)
import Data.Locations as Locations exposing (AddressLocations)
import Data.PageData as PageData
    exposing
        ( CartItemError(..)
        , CartItemId(..)
        , CartItemWarning(..)
        , LineItemType(..)
        , encodeCartItemError
        , encodeCartItemWarning
        , showCartItemError
        , showCartItemWarning
        )
import Data.Product as Product exposing (productMainImage, variantPrice)
import Data.Routing.Routing as Routing exposing (Route(..), reverse)
import Data.User as User exposing (AuthStatus)
import Dict
import Html exposing (..)
import Html.Attributes as A exposing (alt, attribute, checked, class, colspan, for, href, id, name, placeholder, required, rows, src, step, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Extra exposing (viewIf)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import String.Extra exposing (toSentenceCase)
import Time
import Utils.Format as Format
import Utils.Update exposing (nothingAndNoCommand)
import Utils.View exposing (decimalInput, disableGrammarly, emailInput, icon, labelView, pageOverlay, pageTitleView, pageTitleWithSubView, rawHtml)



-- Model


type CheckoutAddress
    = ExistingAddress AddressId
    | NewAddress Address.Form


type alias Form =
    { email : String
    , shippingAddress : CheckoutAddress
    , makeShippingDefault : Bool
    , billingAddress : CheckoutAddress
    , makeBillingDefault : Bool
    , billingSameAsShipping : Bool
    , storeCredit : String
    , priorityShipping : Bool
    , couponCode : String
    , comment : String
    , errors : Api.FormErrors
    , isPaymentProcessing : Bool
    , isProcessing : Bool
    , hasNewCartInventoryNotifications : Bool
    , checkoutToken : Maybe String
    , confirmationModel : Maybe PaymentConfirmationModel
    }


initial : Form
initial =
    { email = ""
    , shippingAddress = NewAddress Address.initialForm
    , makeShippingDefault = False
    , billingAddress = NewAddress Address.initialForm
    , makeBillingDefault = False
    , billingSameAsShipping = False
    , storeCredit = ""
    , priorityShipping = False
    , couponCode = ""
    , comment = ""
    , errors = Api.initialErrors
    , isPaymentProcessing = False
    , isProcessing = False
    , hasNewCartInventoryNotifications = False
    , checkoutToken = Nothing
    , confirmationModel = Nothing
    }


initialWithDefaults : List Address.Model -> List Address.Model -> Maybe String -> Form
initialWithDefaults shippingAddresses billingAddresses restrictionsError =
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

        errors =
            restrictionsError
                |> Maybe.map (\e -> Api.addError "" e Api.initialErrors)
                |> Maybe.withDefault Api.initialErrors
    in
    { email = ""
    , shippingAddress = shippingAddress
    , makeShippingDefault = isNew shippingAddress
    , billingAddress = billingAddress
    , makeBillingDefault = isNew billingAddress
    , billingSameAsShipping = False
    , storeCredit = ""
    , priorityShipping = False
    , couponCode = ""
    , comment = ""
    , errors = errors
    , isPaymentProcessing = False
    , isProcessing = False
    , hasNewCartInventoryNotifications = False
    , checkoutToken = Nothing
    , confirmationModel = Nothing
    }



-- Update


type Msg
    = Email String
    | SelectShipping Int
    | ToggleShippingDefault Bool
    | ShippingMsg Address.Msg
    | SelectBilling Int
    | ToggleBillingDefault Bool
    | BillingMsg Address.Msg
    | BillingSameAsShipping Bool
    | StoreCredit String
    | TogglePriorityShipping Bool
    | CouponCode String
    | Comment String
    | ApplyCoupon
    | RemoveCoupon
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors (CartInventoryCheckResult CheckoutResponse)))
    | RefreshDetails (WebData (Result Api.FormErrors PageData.CheckoutDetails))
    | HelcimCheckoutTokenReceived (WebData (Result Api.FormErrors (CartInventoryCheckResult HelcimCheckoutTokenResponse)))
    | HelcimEventReceived Decode.Value
    | ConfirmPayment
    | CancelPayment
    | ValidateCart
    | ValidateCartResponse (WebData (Result Api.FormErrors PageData.CartDetails))


type alias CartInventoryNotifications =
    { warnings : List ( CartItemId, List CartItemWarning )
    , errors : List ( CartItemId, List CartItemError )
    }


emptyCartInventoryNotifications : CartInventoryNotifications
emptyCartInventoryNotifications =
    { warnings = []
    , errors = []
    }


checkoutDetailsToCartInventoryNotifications : PageData.CheckoutDetails -> CartInventoryNotifications
checkoutDetailsToCartInventoryNotifications details =
    { warnings = List.map (\cartItem -> ( cartItem.id, cartItem.warnings )) details.items
    , errors = List.map (\cartItem -> ( cartItem.id, cartItem.errors )) details.items
    }


encodeCartInventoryNotifications : CartInventoryNotifications -> Value
encodeCartInventoryNotifications notifications =
    Encode.object
        [ ( "warnings"
          , Encode.object
                (List.map
                    (\( CartItemId id, warnings ) ->
                        ( String.fromInt id, Encode.list encodeCartItemWarning warnings )
                    )
                    notifications.warnings
                )
          )
        , ( "errors"
          , Encode.object
                (List.map
                    (\( CartItemId id, errors ) ->
                        ( String.fromInt id, Encode.list encodeCartItemError errors )
                    )
                    notifications.errors
                )
          )
        ]


type CartInventoryCheckResult a
    = NoNewCartInventoryNotifications a
    | NewCartInventoryNotifications


cartInventoryCheckResultDecoder : Decode.Decoder a -> Decode.Decoder (CartInventoryCheckResult a)
cartInventoryCheckResultDecoder innerDecoder =
    Decode.field "status" Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "ok" ->
                        Decode.map NoNewCartInventoryNotifications (Decode.field "result" innerDecoder)

                    "new-notifications" ->
                        Decode.succeed NewCartInventoryNotifications

                    _ ->
                        Decode.fail ("Unexpected status: " ++ status)
            )


type alias HelcimCheckoutTokenResponse =
    { checkoutToken : String }


helcimCheckoutTokenResponseDecoder : Decode.Decoder HelcimCheckoutTokenResponse
helcimCheckoutTokenResponseDecoder =
    Decode.map HelcimCheckoutTokenResponse
        (Decode.field "token" Decode.string)


type alias CheckoutResponse =
    { guestToken : String
    , orderId : Int
    , orderLines : List PageData.OrderLineItem
    , orderProducts : List PageData.OrderProduct
    }


type alias HelcimPayInitEvent =
    { eventName : String
    , eventStatus : Bool
    }


helcimPayInitEventDecoder : Decode.Decoder HelcimPayInitEvent
helcimPayInitEventDecoder =
    Decode.map2 HelcimPayInitEvent
        (Decode.field "eventName" Decode.string)
        (Decode.field "eventStatus" Decode.bool)


type alias HelcimPayCheckoutEvent =
    { eventName : String
    , eventStatus : String
    , eventMessage : Result String HelcimEventMessage
    }


helcimPayCheckoutEventDecoder : Decode.Decoder HelcimPayCheckoutEvent
helcimPayCheckoutEventDecoder =
    Decode.map3 HelcimPayCheckoutEvent
        (Decode.field "eventName" Decode.string)
        (Decode.field "eventStatus" Decode.string)
        (Decode.field "eventMessage" Decode.string
            |> Decode.andThen
                (\jsonString ->
                    case Decode.decodeString helcimEventMessageDecoder jsonString of
                        Ok eventData ->
                            Decode.succeed (Ok eventData)

                        -- If we failed to decode an actual event, then eventMessage
                        -- contains a string with the error.
                        Err _ ->
                            Decode.succeed (Err jsonString)
                )
        )


type alias HelcimEventMessage =
    { status : Int
    , data : HelcimEventData
    }


helcimEventMessageDecoder : Decode.Decoder HelcimEventMessage
helcimEventMessageDecoder =
    Decode.map2 HelcimEventMessage
        (Decode.field "status" Decode.int)
        (Decode.field "data" helcimEventDataDecoder)


type alias HelcimEventData =
    { data : HelcimEventDataInternal
    , hash : String
    }


helcimEventDataDecoder : Decode.Decoder HelcimEventData
helcimEventDataDecoder =
    Decode.map2 HelcimEventData
        (Decode.field "data" helcimEventDataInternalDecoder)
        (Decode.field "hash" Decode.string)


type alias HelcimEventDataInternal =
    { amount : String
    , approvalCode : String
    , avsResponse : String
    , cardBatchId : String
    , cardHolderName : String
    , cardNumber : String
    , cardToken : String
    , currency : String
    , customerCode : String
    , dateCreated : String
    , invoiceNumber : String
    , status : String
    , transactionId : String
    , transactionType : String
    }


helcimEventDataInternalDecoder : Decode.Decoder HelcimEventDataInternal
helcimEventDataInternalDecoder =
    Decode.map8 HelcimEventDataInternal
        (Decode.field "amount" Decode.string)
        (Decode.field "approvalCode" Decode.string)
        (Decode.field "avsResponse" Decode.string)
        (Decode.field "cardBatchId" Decode.string)
        (Decode.field "cardHolderName" Decode.string)
        (Decode.field "cardNumber" Decode.string)
        (Decode.field "cardToken" Decode.string)
        (Decode.field "currency" Decode.string)
        |> Decode.andThen
            (\partialEventData ->
                Decode.map6
                    (\customerCode dateCreated invoiceNumber status transactionId transactionType ->
                        partialEventData customerCode dateCreated invoiceNumber status transactionId transactionType
                    )
                    (Decode.field "customerCode" Decode.string)
                    (Decode.field "dateCreated" Decode.string)
                    (Decode.field "invoiceNumber" Decode.string)
                    (Decode.field "status" Decode.string)
                    (Decode.field "transactionId" Decode.string)
                    (Decode.field "type" Decode.string)
            )


type HelcimPayEvent
    = HelcimPayInit HelcimPayInitEvent
    | HelcimPayCheckout HelcimPayCheckoutEvent


helcimPayEventDecoder : Decode.Decoder HelcimPayEvent
helcimPayEventDecoder =
    Decode.oneOf
        [ Decode.map HelcimPayInit helcimPayInitEventDecoder
        , Decode.map HelcimPayCheckout helcimPayCheckoutEventDecoder
        ]


anonymousResponseDecoder : Decode.Decoder CheckoutResponse
anonymousResponseDecoder =
    Decode.map4 CheckoutResponse
        (Decode.field "guestToken" Decode.string)
        (Decode.field "orderId" Decode.int)
        (Decode.field "lines" <| Decode.list PageData.lineItemDecoder)
        (Decode.field "products" <| Decode.list PageData.orderProductDecoder)


customerResponseDecoder : Decode.Decoder CheckoutResponse
customerResponseDecoder =
    Decode.map3 (CheckoutResponse "")
        (Decode.field "orderId" Decode.int)
        (Decode.field "lines" <| Decode.list PageData.lineItemDecoder)
        (Decode.field "products" <| Decode.list PageData.orderProductDecoder)


type OutMsg
    = AnonymousOrderCompleted Int String
    | CustomerOrderCompleted Int
    | DetailsRefreshed PageData.CheckoutDetails
    | LoggedIn AuthStatus PageData.CheckoutDetails


subscriptions : Sub Msg
subscriptions =
    Ports.helcimMessageReceived HelcimEventReceived


update :
    Msg
    -> Form
    -> AuthStatus
    -> Maybe String
    -> WebData PageData.CheckoutDetails
    -> ( Form, Maybe OutMsg, Cmd Msg )
update msg model authStatus maybeSessionToken checkoutDetails =
    case msg of
        Email email ->
            { model | email = email } |> nothingAndNoCommand

        SelectShipping addressId ->
            { model
                | shippingAddress = selectAddress addressId
                , makeShippingDefault = False
            }
                |> refreshDetails authStatus maybeSessionToken False model

        ToggleShippingDefault makeDefault ->
            { model | makeShippingDefault = makeDefault }
                |> nothingAndNoCommand

        ShippingMsg subMsg ->
            updateAddressForm model.shippingAddress
                subMsg
                model
                (\f -> { model | shippingAddress = f })
                |> refreshDetails authStatus maybeSessionToken False model

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

        StoreCredit credit ->
            { model | storeCredit = credit }
                |> nothingAndNoCommand

        TogglePriorityShipping isChecked ->
            { model | priorityShipping = isChecked }
                |> refreshDetails authStatus maybeSessionToken True model

        CouponCode code ->
            { model | couponCode = code }
                |> nothingAndNoCommand

        ApplyCoupon ->
            refreshDetails authStatus maybeSessionToken True model model

        RemoveCoupon ->
            { model | couponCode = "" }
                |> refreshDetails authStatus maybeSessionToken True model

        Comment comment ->
            { model | comment = comment } |> nothingAndNoCommand

        Submit ->
            case checkoutDetails of
                RemoteData.Success details ->
                    let
                        (Cents finalTotal) =
                            getFinalTotal details model.storeCredit

                        freeCheckout =
                            PageData.isFreeCheckout checkoutDetails || finalTotal == 0
                    in
                    validateForm model (not freeCheckout) details <|
                        if freeCheckout then
                            ( { model | isProcessing = True }
                            , Nothing
                            , placeOrder model authStatus maybeSessionToken Nothing checkoutDetails
                            )

                        else
                            ( { model | isPaymentProcessing = True }
                            , Nothing
                            , getHelcimCheckoutToken authStatus maybeSessionToken (checkoutDetailsToCartInventoryNotifications details)
                            )

                _ ->
                    ( model, Nothing, Cmd.none )

        SubmitResponse (RemoteData.Success (Ok res)) ->
            case res of
                NoNewCartInventoryNotifications response ->
                    let
                        orderId =
                            response.orderId

                        outMsg =
                            if authStatus == User.Anonymous then
                                AnonymousOrderCompleted orderId response.guestToken

                            else
                                CustomerOrderCompleted orderId

                        analyticsData =
                            encodeAnalyticsPurchase orderId response.orderLines response.orderProducts
                    in
                    ( initial, Just outMsg, Ports.logPurchase analyticsData )

                NewCartInventoryNotifications ->
                    { model
                        | isPaymentProcessing = False
                        , isProcessing = False
                        , hasNewCartInventoryNotifications = True
                    }
                        |> refreshDetails authStatus maybeSessionToken True model

        SubmitResponse (RemoteData.Success (Err errors)) ->
            let
                ( generalErrors, shippingErrors, billingErrors ) =
                    List.foldl
                        (\( field, fieldErrors ) ( generalErr, shippingErr, billingErr ) ->
                            case String.split "-" field of
                                "shipping" :: xs ->
                                    ( generalErr
                                    , ( String.join "" xs, fieldErrors ) :: shippingErr
                                    , billingErr
                                    )

                                "billing" :: xs ->
                                    ( generalErr
                                    , shippingErr
                                    , ( String.join "" xs, fieldErrors ) :: billingErr
                                    )

                                _ ->
                                    ( ( field, fieldErrors ) :: generalErr
                                    , shippingErr
                                    , billingErr
                                    )
                        )
                        ( [], [], [] )
                        (Dict.toList errors)

                updatedShippingForm =
                    case model.shippingAddress of
                        ExistingAddress _ ->
                            model.shippingAddress

                        NewAddress addrForm ->
                            NewAddress { addrForm | errors = Dict.fromList shippingErrors }

                updatedBillingForm =
                    case model.billingAddress of
                        ExistingAddress _ ->
                            model.billingAddress

                        NewAddress addrForm ->
                            NewAddress { addrForm | errors = Dict.fromList billingErrors }

                addAddressErrorIfExisting prefix address =
                    case address of
                        ExistingAddress _ ->
                            Dict.update prefix (always <| Dict.get (prefix ++ "-") errors)

                        NewAddress _ ->
                            identity
            in
            ( { model
                | shippingAddress = updatedShippingForm
                , billingAddress = updatedBillingForm
                , isProcessing = False
                , errors =
                    Dict.fromList generalErrors
                        |> addAddressErrorIfExisting "shipping" model.shippingAddress
                        |> addAddressErrorIfExisting "billing" model.billingAddress
              }
            , Nothing
            , Ports.scrollToTop
            )

        SubmitResponse (RemoteData.Failure error) ->
            ( { model | errors = Api.apiFailureToError error, isProcessing = False }
            , Nothing
            , Ports.scrollToTop
            )

        SubmitResponse _ ->
            { model | isProcessing = False } |> nothingAndNoCommand

        RefreshDetails (RemoteData.Success (Ok details)) ->
            let
                errors =
                    details.restrictionsError
                        |> Maybe.map (\e -> Api.addError "" e Api.initialErrors)
                        |> Maybe.withDefault Api.initialErrors

                cmd =
                    if Dict.isEmpty errors then
                        Cmd.none

                    else
                        Ports.scrollToErrorMessage
            in
            ( { model | errors = errors }
            , Just (DetailsRefreshed details)
            , cmd
            )

        RefreshDetails (RemoteData.Success (Err formErrors)) ->
            { model | errors = formErrors } |> nothingAndNoCommand

        RefreshDetails _ ->
            model |> nothingAndNoCommand

        HelcimCheckoutTokenReceived (RemoteData.Success (Ok res)) ->
            case res of
                NoNewCartInventoryNotifications token ->
                    ( { model | checkoutToken = Just token.checkoutToken }
                    , Nothing
                    , Cmd.batch [ Ports.appendHelcimPayIframe token.checkoutToken, Ports.subscribeToHelcimMessages () ]
                    )

                NewCartInventoryNotifications ->
                    { model
                        | isPaymentProcessing = False
                        , isProcessing = False
                        , hasNewCartInventoryNotifications = True
                    }
                        |> refreshDetails authStatus maybeSessionToken True model

        HelcimCheckoutTokenReceived (RemoteData.Success (Err errors)) ->
            { model | errors = errors, isPaymentProcessing = False } |> nothingAndNoCommand

        HelcimCheckoutTokenReceived (RemoteData.Failure error) ->
            { model | errors = Api.apiFailureToError error, isPaymentProcessing = False } |> nothingAndNoCommand

        HelcimCheckoutTokenReceived _ ->
            { model | isPaymentProcessing = False } |> nothingAndNoCommand

        HelcimEventReceived event ->
            case Decode.decodeValue helcimPayEventDecoder event of
                Err _ ->
                    { model | errors = Api.addError "" "Unexpected HelcimPay.js response." model.errors, isPaymentProcessing = False } |> nothingAndNoCommand

                Ok (HelcimPayInit _) ->
                    model |> nothingAndNoCommand

                Ok (HelcimPayCheckout checkoutEvent) ->
                    case checkoutDetails of
                        RemoteData.Success details ->
                            handleHelcimCheckoutEvent model details checkoutEvent

                        _ ->
                            ( model, Nothing, Cmd.none )

        ConfirmPayment ->
            case model.confirmationModel of
                Nothing ->
                    model |> nothingAndNoCommand

                Just confirmationModel ->
                    ( { model | confirmationModel = Nothing, isProcessing = True }
                    , Nothing
                    , placeOrder model authStatus maybeSessionToken (Just ( confirmationModel.cardToken, confirmationModel.customerCode )) checkoutDetails
                    )

        CancelPayment ->
            { model | confirmationModel = Nothing } |> nothingAndNoCommand

        ValidateCart ->
            ( model, Nothing, validateCart authStatus maybeSessionToken checkoutDetails )

        ValidateCartResponse response ->
            case response of
                RemoteData.Success (Ok _) ->
                    ( model, Nothing, Cmd.none )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors }, Nothing, Ports.scrollToErrorMessage )

                _ ->
                    ( model, Nothing, Cmd.none )


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


refreshDetails : AuthStatus -> Maybe String -> Bool -> Form -> Form -> ( Form, Maybe a, Cmd Msg )
refreshDetails authStatus maybeSessionToken forceUpdate oldModel newModel =
    let
        refreshCommand =
            case authStatus of
                User.Anonymous ->
                    applyArguments anonymousCommand

                User.Authorized _ ->
                    applyArguments
                        (\( maybeAddressForm, maybeAddressId ) ->
                            getCustomerDetails RefreshDetails
                                maybeAddressForm
                                maybeAddressId
                                newModel.couponCode
                                newModel.priorityShipping
                        )

        applyArguments cmdFunction =
            if shouldUpdate then
                cmdFunction addressArguments

            else
                Cmd.none

        anonymousCommand ( maybeAddressForm, _ ) =
            case maybeSessionToken of
                Nothing ->
                    Cmd.none

                Just token ->
                    getAnonymousDetails RefreshDetails
                        token
                        maybeAddressForm
                        newModel.couponCode
                        newModel.priorityShipping

        shouldUpdate =
            (addressChanged && hasMinimumAddress)
                || forceUpdate

        addressChanged =
            case ( oldModel.shippingAddress, newModel.shippingAddress ) of
                ( ExistingAddress id1, ExistingAddress id2 ) ->
                    id1 /= id2

                ( NewAddress oldForm, NewAddress newForm ) ->
                    if oldForm.model.country == newForm.model.country then
                        case ( oldForm.model.state, newForm.model.state ) of
                            ( Just (Locations.Custom _), Just (Locations.Custom _) ) ->
                                False

                            ( oldState, newState ) ->
                                (oldState /= newState)
                                    || (oldForm.model.street /= newForm.model.street)
                                    || (oldForm.model.zipCode /= newForm.model.zipCode)

                    else
                        True

                _ ->
                    True

        hasMinimumAddress =
            case newModel.shippingAddress of
                NewAddress addrForm ->
                    not <|
                        List.any String.isEmpty
                            [ addrForm.model.street, addrForm.model.zipCode ]

                _ ->
                    True

        addressArguments =
            case newModel.shippingAddress of
                ExistingAddress id ->
                    ( Nothing, Just id )

                NewAddress addrForm ->
                    ( Just addrForm, Nothing )
    in
    ( newModel, Nothing, refreshCommand )


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


{-| Get the final total by subtracting any store credit from the grand total.
-}
getFinalTotal : PageData.CheckoutDetails -> String -> Cents
getFinalTotal checkoutDetails storeCreditString =
    let
        storeCredit =
            storeCreditString
                |> centsFromString
                |> Maybe.map (limitStoreCredit checkoutDetails)

        grandTotal =
            checkoutDetails.charges.grandTotal
    in
    case storeCredit of
        Nothing ->
            grandTotal

        Just credit ->
            centsMap2 (\total c -> total - c) grandTotal (limitStoreCredit checkoutDetails credit)


{-| Take the Form & a set of values to return when valid, checking that the
passwords match & the state dropdowns have been selected.
-}
validateForm : Form -> Bool -> PageData.CheckoutDetails -> ( Form, Maybe OutMsg, Cmd Msg ) -> ( Form, Maybe OutMsg, Cmd Msg )
validateForm model validateBilling checkoutDetails validResult =
    let
        hasErrors =
            not <|
                Dict.isEmpty modelErrors
                    && Dict.isEmpty billingErrors
                    && Dict.isEmpty shippingErrors
                    && List.all (\i -> List.length i.errors == 0) checkoutDetails.items

        modelErrors =
            Api.initialErrors

        shippingErrors =
            checkAddressRegion model.shippingAddress

        billingErrors =
            if not model.billingSameAsShipping && validateBilling then
                checkAddressRegion model.billingAddress

            else
                Dict.empty

        checkAddressRegion addr =
            case addr of
                ExistingAddress _ ->
                    Dict.empty

                NewAddress addrForm ->
                    if addrForm.model.state == Nothing then
                        Dict.update "state"
                            (always <| Just [ "Please select a State." ])
                            addrForm.errors

                    else
                        Dict.update "state" (always Nothing) addrForm.errors

        setAddressErrors addr errors =
            case addr of
                ExistingAddress _ ->
                    addr

                NewAddress addrForm ->
                    NewAddress { addrForm | errors = errors }
    in
    if hasErrors then
        ( { model
            | errors = modelErrors
            , billingAddress = setAddressErrors model.billingAddress billingErrors
            , shippingAddress = setAddressErrors model.shippingAddress shippingErrors
          }
        , Nothing
        , Ports.scrollToTop
        )

    else
        validResult


{-| Limit the amount of store credit applied so the checkout total is never
negative.
-}
limitStoreCredit : PageData.CheckoutDetails -> Cents -> Cents
limitStoreCredit checkoutDetails =
    let
        maximumStoreCredit =
            PageData.cartTotals checkoutDetails
                |> .total
                |> centsMap2 min checkoutDetails.storeCredit
    in
    centsMap2 min maximumStoreCredit


getCustomerDetails :
    (WebData (Result Api.FormErrors PageData.CheckoutDetails) -> msg)
    -> Maybe Address.Form
    -> Maybe AddressId
    -> String
    -> Bool
    -> Cmd msg
getCustomerDetails msg maybeAddress maybeAddressId couponCode priorityShipping =
    let
        data =
            Encode.object
                [ ( "address", encodeMaybe Address.encode maybeAddress )
                , ( "addressId", encodeMaybe (\(AddressId i) -> Encode.int i) maybeAddressId )
                , ( "couponCode", encodeStringAsMaybe couponCode )
                , ( "priorityShipping", Encode.bool priorityShipping )
                ]
    in
    Api.post Api.CheckoutDetailsCustomer
        |> Api.withJsonBody data
        |> Api.withErrorHandler PageData.checkoutDetailsDecoder
        |> Api.sendRequest msg


getAnonymousDetails :
    (WebData (Result Api.FormErrors PageData.CheckoutDetails) -> msg)
    -> String
    -> Maybe Address.Form
    -> String
    -> Bool
    -> Cmd msg
getAnonymousDetails msg sessionToken maybeAddress couponCode priorityShipping =
    let
        data =
            Encode.object
                [ ( "address", encodeMaybe Address.encode maybeAddress )
                , ( "sessionToken", Encode.string sessionToken )
                , ( "couponCode", encodeStringAsMaybe couponCode )
                , ( "priorityShipping", Encode.bool priorityShipping )
                ]
    in
    Api.post Api.CheckoutDetailsAnonymous
        |> Api.withJsonBody data
        |> Api.withErrorHandler PageData.checkoutDetailsDecoder
        |> Api.sendRequest msg


{-| If a String is empty, encode it as null, otherwise encode the String.
-}
encodeStringAsMaybe : String -> Value
encodeStringAsMaybe str =
    if String.isEmpty str then
        Encode.null

    else
        Encode.string str


getHelcimCheckoutToken : AuthStatus -> Maybe String -> CartInventoryNotifications -> Cmd Msg
getHelcimCheckoutToken authStatus maybeSessionToken cartInventoryNotifications =
    case authStatus of
        User.Authorized _ ->
            let
                requestData =
                    Encode.object
                        [ ( "cartInventoryNotifications", encodeCartInventoryNotifications cartInventoryNotifications ) ]
            in
            Api.post Api.CheckoutHelcimToken
                |> Api.withJsonBody requestData
                |> Api.withErrorHandler (cartInventoryCheckResultDecoder helcimCheckoutTokenResponseDecoder)
                |> Api.sendRequest HelcimCheckoutTokenReceived

        User.Anonymous ->
            let
                requestData =
                    Encode.object
                        [ ( "cartInventoryNotifications", encodeCartInventoryNotifications cartInventoryNotifications )
                        , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                        ]
            in
            Api.post Api.CheckoutHelcimTokenAnonymous
                |> Api.withJsonBody requestData
                |> Api.withErrorHandler (cartInventoryCheckResultDecoder helcimCheckoutTokenResponseDecoder)
                |> Api.sendRequest HelcimCheckoutTokenReceived


placeOrder : Form -> AuthStatus -> Maybe String -> Maybe ( String, String ) -> WebData PageData.CheckoutDetails -> Cmd Msg
placeOrder model authStatus maybeSessionToken helcimData checkoutDetails =
    let
        customerData =
            Encode.object
                ([ ( "shippingAddress", encodedShippingAddress )
                 , ( "billingAddress", encodedBillingAddress )
                 , ( "storeCredit", encodedStoreCredit )
                 , ( "priorityShipping", Encode.bool model.priorityShipping )
                 , ( "couponCode", encodeStringAsMaybe model.couponCode )
                 , ( "comment", Encode.string model.comment )
                 , ( "cartInventoryNotifications", encodedCartInventoryNotifications )
                 ]
                    ++ (helcimData
                            |> Maybe.map
                                (\( helcimToken, helcimCustomerCode ) ->
                                    [ ( "helcimToken", Encode.string helcimToken )
                                    , ( "helcimCustomerCode", Encode.string helcimCustomerCode )
                                    ]
                                )
                            |> Maybe.withDefault []
                       )
                )

        anonymousData =
            Encode.object
                ([ ( "shippingAddress", encodedShippingAddress )
                 , ( "billingAddress", encodedBillingAddress )
                 , ( "priorityShipping", Encode.bool model.priorityShipping )
                 , ( "couponCode", encodeStringAsMaybe model.couponCode )
                 , ( "comment", Encode.string model.comment )
                 , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                 , ( "email", Encode.string model.email )
                 , ( "cartInventoryNotifications", encodedCartInventoryNotifications )
                 ]
                    ++ (helcimData
                            |> Maybe.map
                                (\( helcimToken, helcimCustomerCode ) ->
                                    [ ( "helcimToken", Encode.string helcimToken )
                                    , ( "helcimCustomerCode", Encode.string helcimCustomerCode )
                                    ]
                                )
                            |> Maybe.withDefault []
                       )
                )

        encodedStoreCredit =
            model.storeCredit
                |> centsFromString
                |> Maybe.map (\(Cents c) -> Encode.int c)
                |> Maybe.withDefault Encode.null

        encodedShippingAddress =
            encodeAddress model.shippingAddress model.makeShippingDefault

        encodedBillingAddress =
            if isFreeCheckout then
                Encode.null

            else if model.billingSameAsShipping then
                RemoteData.toMaybe checkoutDetails
                    |> Maybe.map
                        (isDefaultAddress model.shippingAddress
                            >> encodeAddress model.shippingAddress
                        )
                    |> Maybe.withDefault (encodeAddress model.shippingAddress model.makeShippingDefault)

            else
                encodeAddress model.billingAddress model.makeBillingDefault

        encodedCartInventoryNotifications =
            RemoteData.toMaybe checkoutDetails
                |> Maybe.map checkoutDetailsToCartInventoryNotifications
                |> Maybe.withDefault emptyCartInventoryNotifications
                |> encodeCartInventoryNotifications

        isFreeCheckout =
            PageData.isFreeCheckout checkoutDetails || zeroTotalWithAppliedCredit

        zeroTotalWithAppliedCredit =
            RemoteData.toMaybe checkoutDetails
                |> Maybe.map (\details -> getFinalTotal details model.storeCredit == Cents 0)
                |> Maybe.withDefault False

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

        isDefaultAddress address { shippingAddresses, billingAddresses } =
            case address of
                ExistingAddress id ->
                    findBy (\a -> a.id == Just id) (shippingAddresses ++ billingAddresses)
                        |> Maybe.map .isDefault
                        |> Maybe.withDefault True

                NewAddress addrForm ->
                    addrForm.model.isDefault || model.makeShippingDefault

        decoder =
            case authStatus of
                User.Anonymous ->
                    anonymousResponseDecoder

                User.Authorized _ ->
                    customerResponseDecoder
    in
    case authStatus of
        User.Anonymous ->
            Api.post Api.CheckoutPlaceOrderAnonymous
                |> Api.withJsonBody anonymousData
                |> Api.withErrorHandler (cartInventoryCheckResultDecoder decoder)
                |> Api.sendRequest SubmitResponse

        User.Authorized _ ->
            Api.post Api.CheckoutPlaceOrderCustomer
                |> Api.withJsonBody customerData
                |> Api.withErrorHandler (cartInventoryCheckResultDecoder decoder)
                |> Api.sendRequest SubmitResponse


handleHelcimCheckoutEvent : Form -> PageData.CheckoutDetails -> HelcimPayCheckoutEvent -> ( Form, Maybe OutMsg, Cmd Msg )
handleHelcimCheckoutEvent model checkoutDetails checkoutEvent =
    if checkoutEvent.eventName == "helcim-pay-js-" ++ (model.checkoutToken |> Maybe.withDefault "") then
        if checkoutEvent.eventStatus == "HIDE" then
            ( { model | checkoutToken = Nothing, isPaymentProcessing = False }
            , Nothing
            , Ports.removeHelcimPayIframe ()
            )

        else if checkoutEvent.eventStatus == "ABORTED" then
            case checkoutEvent.eventMessage of
                Err errors ->
                    ( { model | checkoutToken = Nothing, isPaymentProcessing = False, errors = Api.addError "" errors model.errors }
                    , Nothing
                    , Ports.removeHelcimPayIframe ()
                    )

                Ok _ ->
                    ( { model
                        | checkoutToken = Nothing
                        , isPaymentProcessing = False
                        , errors = Api.addError "" "Unexpected HelcimPay.js response for ABORTED event." model.errors
                      }
                    , Nothing
                    , Ports.removeHelcimPayIframe ()
                    )

        else
            case checkoutEvent.eventMessage of
                Err errors ->
                    { model | errors = Api.addError "" errors model.errors, isPaymentProcessing = False } |> nothingAndNoCommand

                Ok eventMessage ->
                    if eventMessage.data.data.status == "APPROVED" then
                        ( { model
                            | checkoutToken = Nothing
                            , isPaymentProcessing = False
                            , confirmationModel =
                                Just
                                    (initPaymentConfirmation
                                        eventMessage.data.data.cardNumber
                                        (getFinalTotal checkoutDetails model.storeCredit)
                                        eventMessage.data.data.cardToken
                                        eventMessage.data.data.customerCode
                                    )
                          }
                        , Nothing
                        , Ports.removeHelcimPayIframe ()
                        )

                    else
                        ( { model | checkoutToken = Nothing, isPaymentProcessing = False, errors = Api.addError "" "Helcim purchase was declined." model.errors }
                        , Nothing
                        , Ports.removeHelcimPayIframe ()
                        )

    else
        model |> nothingAndNoCommand


{-| TODO: This is probably repeated other places, stick in a Json.Utils module.
-}
encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null


{-| Encode the completed order data into the JSON requires for the Google Analytics `purchase` event.
-}
encodeAnalyticsPurchase : Int -> List PageData.OrderLineItem -> List PageData.OrderProduct -> Value
encodeAnalyticsPurchase orderId lines products =
    let
        sumCents =
            List.foldr (centsMap2 (+)) (Cents 0)

        taxTotal =
            lines
                |> List.filter (\l -> l.itemType == Tax)
                |> List.map .amount
                |> sumCents

        shippingTotal =
            lines
                |> List.filter (\l -> List.member l.itemType [ PriorityShipping, Shipping ])
                |> List.map .amount
                |> sumCents

        otherLineTotal =
            List.foldr
                (\line total ->
                    case line.itemType of
                        Shipping ->
                            total

                        PriorityShipping ->
                            total

                        Tax ->
                            total

                        Surcharge ->
                            centsMap2 (+) total line.amount

                        PageData.StoreCredit ->
                            centsMap2 (-) total line.amount

                        MemberDiscount ->
                            centsMap2 (-) total line.amount

                        CouponDiscount ->
                            centsMap2 (-) total line.amount

                        Refund ->
                            centsMap2 (-) total line.amount
                )
                (Cents 0)
                lines

        productTotal =
            products
                |> List.map (\p -> centsMap2 (*) p.price (Cents p.quantity))
                |> sumCents

        orderTotal =
            productTotal
                |> centsMap2 (+) taxTotal
                |> centsMap2 (+) shippingTotal
                |> centsMap2 (+) otherLineTotal

        encodeCentsAsDollars (Cents c) =
            Encode.float <| toFloat c / 100

        encodeProduct p =
            Encode.object
                [ ( "id", Encode.string p.sku )
                , ( "name", Encode.string <| nameWithLotSize p )
                , ( "quantity", Encode.int p.quantity )
                , ( "price", encodeCentsAsDollars p.price )
                ]

        nameWithLotSize { name, lotSize } =
            name
                ++ (lotSize
                        |> Maybe.map (\s -> ", " ++ lotSizeToString s)
                        |> Maybe.withDefault ""
                   )
    in
    Encode.object
        [ ( "transaction_id", Encode.string <| String.fromInt orderId )
        , ( "value", encodeCentsAsDollars orderTotal )
        , ( "currency", Encode.string "USD" )
        , ( "tax", encodeCentsAsDollars taxTotal )
        , ( "shipping", encodeCentsAsDollars shippingTotal )
        , ( "items", Encode.list encodeProduct products )
        ]


validateCart : AuthStatus -> Maybe String -> WebData PageData.CheckoutDetails -> Cmd Msg
validateCart authStatus maybeCartToken checkoutDetails =
    case checkoutDetails of
        RemoteData.Success details ->
            let
                items =
                    details.items

                fromCartItemId (CartItemId i) =
                    i

                encodedQuantities =
                    Encode.object <|
                        ( "quantities", Encode.object (List.map (\{ id, quantity } -> ( String.fromInt (fromCartItemId id), Encode.int quantity )) items) )
                            :: (Maybe.map (\token -> [ ( "sessionToken", Encode.string token ) ]) maybeCartToken |> Maybe.withDefault [])

                cmd =
                    case authStatus of
                        User.Authorized _ ->
                            Api.post Api.CartUpdateCustomer
                                |> Api.withJsonBody encodedQuantities
                                |> Api.withErrorHandler PageData.cartDetailsDecoder
                                |> Api.sendRequest ValidateCartResponse

                        User.Anonymous ->
                            Api.post Api.CartUpdateAnonymous
                                |> Api.withJsonBody encodedQuantities
                                |> Api.withErrorHandler PageData.cartDetailsDecoder
                                |> Api.sendRequest ValidateCartResponse
            in
            cmd

        _ ->
            Cmd.none



-- View


view : Form -> AuthStatus -> AddressLocations -> PageData.CheckoutDetails -> List (Html Msg)
view model authStatus locations checkoutDetails =
    let
        generalErrors =
            Api.getErrorHtml "" model.errors

        hasErrors =
            not <|
                Dict.isEmpty model.errors
                    && Dict.isEmpty (addrErrors model.billingAddress)
                    && Dict.isEmpty (addrErrors model.shippingAddress)

        addrErrors addr =
            case addr of
                ExistingAddress _ ->
                    Dict.empty

                NewAddress addrForm ->
                    addrForm.errors

        guestCard =
            case authStatus of
                User.Authorized _ ->
                    text ""

                User.Anonymous ->
                    guestForm model

        billingCard =
            if model.billingSameAsShipping then
                addressCard
                    [ div [ class "tw:text-[16px] tw:leading-[20px] tw:opacity-70 tw:flex tw:pb-[16px]" ]
                        [ span [ class "tw:grow" ] [ text "Billing Details" ]
                        , sameAddressesCheckbox model.billingSameAsShipping
                        ]
                    , billingAddressText
                    ]
                    freeCheckout

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
                    , generalErrors =
                        Dict.get "billing" model.errors
                            |> Maybe.withDefault []
                    , fullWidth = freeCheckout
                    }

        billingAddressText =
            case model.shippingAddress of
                NewAddress form ->
                    if form.model /= Address.initial then
                        Address.card form.model locations

                    else
                        text ""

                ExistingAddress id ->
                    findBy (\a -> a.id == Just id) checkoutDetails.shippingAddresses
                        |> Maybe.map (\a -> Address.card a locations)
                        |> Maybe.withDefault (text "")

        storeCreditForm =
            case checkoutDetails.storeCredit of
                Cents 0 ->
                    text ""

                _ ->
                    div [ class "tw:pt-[60px]" ]
                        [ h5 [] [ text "Store Credit" ]
                        , div [ class "tw:pt-[20px] tw:flex tw:gap-[8px]" ]
                            [ div [ class "tw:pt-[2px]" ] [ discountSvg "tw:fill-black" ]
                            , p [ class "" ]
                                [ text "You have "
                                , b [ class "tw:text-[#4DAA9A]" ] [ text <| Format.cents checkoutDetails.storeCredit ]
                                , text " of store credit available."
                                ]
                            ]
                        , div [ class "input-group mt-auto mb-3 tw:pt-[16px]" ]
                            [ div [ class "input-group-prepend" ]
                                [ span [ class "input-group-text" ] [ text "$" ] ]
                            , input
                                [ class "form-control"
                                , type_ "number"
                                , step "0.01"
                                , A.min "0"
                                , A.max <| Format.centsNumber maximumStoreCredit
                                , value model.storeCredit
                                , onInput StoreCredit
                                , decimalInput
                                ]
                                []
                            ]
                        , Dict.get "store-credit" model.errors
                            |> Maybe.map (\errs -> p [] [ small [] [ Api.errorHtml errs ] ])
                            |> Maybe.withDefault (text "")
                        ]

        maximumStoreCredit =
            let
                (Cents orderTotal) =
                    PageData.cartTotals checkoutDetails |> .total

                (Cents credit) =
                    checkoutDetails.storeCredit
            in
            min credit orderTotal
                |> Cents

        priorityShippingForm =
            div [ class "tw:pt-[60px]" ]
                [ h5 [] [ text "Priority Shipping & Handling" ]
                , p [ class "tw:pt-[20px]" ]
                    [ b [] [ text "Rush my order!" ]
                    , text <|
                        " Selecting this option will put your order at the top of our "
                            ++ "stack and generally guarantee your order will be shipped "
                            ++ "within 2 business days via USPS Priority Mail. This adds "
                            ++ "an additional cost to your order: $5.00 plus 5% of your "
                            ++ "order sub-total."
                    , text <|
                        Maybe.withDefault "" <|
                            Maybe.map priorityFeeCost priorityFee
                    ]
                , Maybe.withDefault priorityNotAvailable <|
                    Maybe.map (always <| text "") priorityFee
                , div [ class "form-check tw:pt-[20px]" ]
                    [ input
                        [ id "priority-shipping-input"
                        , class "form-check-input"
                        , type_ "checkbox"
                        , checked model.priorityShipping
                        , onCheck TogglePriorityShipping
                        , A.disabled <| priorityFee == Nothing
                        ]
                        []
                    , label
                        [ class "form-check-label"
                        , for "priority-shipping-input"
                        ]
                        [ text "Add Priority Shipping & Handling" ]
                    ]
                , div [ class "tw:pt-[20px]" ]
                    [ Alert.view
                        { defaultAlert
                            | content =
                                span []
                                    [ span []
                                        [ text <|
                                            "We cannot apply priority shipping and handling to seasonal items"
                                        ]
                                    , span [ class "tw:font-semibold" ]
                                        [ text <|
                                            "(potatoes, sweet potatoes, garlic, perennial onions, ginseng, & goldenseal)."
                                        ]
                                    ]
                            , style = Alert.Warning
                        }
                    ]
                ]

        priorityFeeCost fee =
            "This adds an additional cost to your order: "
                ++ formatPriorityFee fee

        priorityNotAvailable =
            p [ class "text-danger font-weight-bold" ]
                [ text <|
                    "Sorry, priority shipping is not available "
                        ++ "due to the contents of your cart."
                ]

        formatPriorityFee { flat, percent } =
            case ( flat, percent ) of
                ( Cents 0, _ ) ->
                    String.fromInt percent ++ "% of your order sub-total."

                ( _, 0 ) ->
                    Format.cents flat

                _ ->
                    String.join " "
                        [ Format.cents flat
                        , "plus"
                        , String.fromInt percent ++ "% of your order sub-total."
                        ]

        priorityFee =
            checkoutDetails.charges.shippingMethod
                |> Maybe.andThen .priorityFee

        priorityShipingEnabled =
            checkoutDetails.charges.shippingMethod
                |> Maybe.map .priorityEnabled
                |> Maybe.withDefault False

        (Cents finalTotal) =
            getFinalTotal checkoutDetails model.storeCredit

        freeCheckout =
            finalTotal <= 0

        buttonContents =
            if model.isPaymentProcessing || model.isProcessing then
                { text = "Placing Order..."
                , icon = Just <| icon "spinner fa-spin ml-2"
                }

            else if freeCheckout then
                { text = "Place Order", icon = Nothing }

            else
                { text = "Pay with Credit Card", icon = Just <| icon "arrow-right" }

        processingOverlay =
            pageOverlay model.isProcessing "Processing..."

        paymentConfirmationOverlay =
            case model.confirmationModel of
                Nothing ->
                    text ""

                Just confirmationModel ->
                    viewConfirmationOverlay confirmationModel
    in
    [ pageTitleView "Checkout"
    , if checkoutDetails.isDisabled then
        div [ class "alert alert-danger static-page" ]
            [ rawHtml checkoutDetails.disabledMessage ]

      else
        form [ onSubmit Submit ]
            [ processingOverlay
            , paymentConfirmationOverlay
            , genericErrorText hasErrors
            , guestCard
            , div [ class "mb-3" ] [ generalErrors ]
            , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[24px] tw:lg:gap-[16px]" ]
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
                    , generalErrors =
                        Dict.get "shipping" model.errors
                            |> Maybe.withDefault []
                    , fullWidth = freeCheckout
                    }
                , if freeCheckout then
                    text ""

                  else
                    div []
                        [ billingCard
                        ]
                ]
            , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-0 tw:lg:gap-[16px]" ]
                [ storeCreditForm
                , viewIf priorityShipingEnabled priorityShippingForm

                -- , mobileCouponCodeForm
                ]
            , div [ class "tw:pt-[60px]" ]
                [ h4 [] [ text "Order Summary" ]
                , viewIf model.hasNewCartInventoryNotifications
                    (div [ class "tw:pt-[20px]" ]
                        [ Alert.view
                            { defaultAlert
                                | content = text "Availability for some of the items in your cart has changed. Please review your order and proceed to the payment one more time."
                                , style = Alert.Warning
                            }
                        ]
                    )
                , summaryTableDesktop checkoutDetails
                    model.storeCredit
                    model.couponCode
                    (Dict.get "coupon" model.errors)
                    model.errors
                , summaryTableMobile checkoutDetails
                    model.storeCredit
                    model.couponCode
                    (Dict.get "coupon" model.errors)
                    model.errors
                ]
            , div [ class "tw:pt-[48px]" ]
                [ label [ class "h4", for "commentsTextarea" ]
                    [ text "Additional Comments" ]
                , div [ class "tw:pt-[20px]" ]
                    [ textarea
                        [ id "commentsTextarea"
                        , placeholder "Please include any special shipping requirements you may have."
                        , class "form-control"
                        , rows 4
                        , onInput Comment
                        , value model.comment
                        , disableGrammarly
                        ]
                        []
                    ]
                ]
            , div [ class "tw:flex tw:justify-end tw:pt-[20px]" ]
                [ Button.view
                    { defaultButton
                        | label = buttonContents.text
                        , iconEnd = buttonContents.icon
                        , padding = Button.Width "tw:w-full tw:lg:w-auto tw:px-[16px]"
                        , type_ =
                            if model.isProcessing then
                                Button.Disabled

                            else
                                Button.FormSubmit
                    }
                ]
            ]
    ]


guestForm : Form -> Html Msg
guestForm model =
    let
        -- TODO: Use Components.Admin.Form when we pull it out of the Address module.
        fieldLabel inputId content =
            label [ class "mb-0 tw:py-[4px] tw:font-semibold", for inputId ] [ text content ]

        hasErrors =
            not (Dict.isEmpty model.errors)

        inputClass errors =
            if hasErrors && List.isEmpty errors then
                class "form-control is-valid"

            else if hasErrors && not (List.isEmpty errors) then
                class "form-control is-invalid"

            else
                class "form-control"

        emailField =
            input
                [ id "emailInput"
                , inputClass emailErrors
                , name "email"
                , type_ "email"
                , placeholder "Email Address"
                , required True
                , onInput Email
                , value model.email
                , autocomplete "email"
                , emailInput
                ]
                []

        emailErrors =
            Dict.get "email" model.errors |> Maybe.withDefault []

        errorHtml errors =
            if List.isEmpty errors then
                text ""

            else
                errors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

        wrapper content =
            div [ class "form-group mb-2" ] content

        autocomplete =
            attribute "autocomplete"
    in
    div [ class "tw:p-[16px] tw:border tw:border-[rgba(77,170,154,1)] tw:bg-[rgba(77,170,154,0.03)] tw:rounded-[16px] tw:mb-[20px]" ]
        [ div [ class "tw:max-w-[420px]" ]
            [ p []
                [ text "Please provide your email so we can send your order confirmation and tracking details."
                ]
            , div [ class "form-group mb-2" ]
                [ fieldLabel "emailInput" "Email address"
                , emailField
                , errorHtml emailErrors
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
    , generalErrors : List String
    , fullWidth : Bool
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

        ( addressId, addressHtml ) =
            case config.model of
                NewAddress addr ->
                    ( Nothing
                    , Address.form addr config.prefix config.locations
                        |> Html.map config.msg
                        |> withDefaultCheckbox addr.model
                    )

                ExistingAddress id ->
                    ( Just id
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
                    div [ class "form-check tw:pt-[16px]" ]
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
            div [ class "form-group" ]
                [ labelView "" (toSentenceCase config.prefix ++ " Address") True
                , Address.select config.selectMsg addressId config.selectAddresses True
                ]
    in
    addressCard
        [ div [ class "tw:text-[16px] tw:leading-[20px] tw:opacity-70 tw:pb-[16px] tw:flex" ]
            [ span [ class "tw:grow" ] [ text config.cardTitle ]
            , sameAsShippingCheckbox
            ]
        , selectHtml
        , Api.errorHtml config.generalErrors
        , addressHtml
        ]
        config.fullWidth


addressCard : List (Html msg) -> Bool -> Html msg
addressCard contents fullWidth =
    let
        divClass =
            if fullWidth then
                "col-md-12 mb-2"

            else
                "col-md-6 mb-2"
    in
    div [ class "tw:bg-[rgba(30,12,3,0.03)] tw:p-[16px] tw:rounded-[16px]" ]
        contents


summaryTableMobile : PageData.CheckoutDetails -> String -> String -> Maybe (List String) -> Api.FormErrors -> Html Msg
summaryTableMobile ({ items, charges } as checkoutDetails) creditString couponCode couponErrors errors =
    let
        productRow p =
            let
                productImage =
                    productMainImage p.product
            in
            div [ class "tw:py-[20px] tw:border-b tw:border-[rgba(30,12,3,0.06)]" ]
                [ div [ class "tw:flex tw:gap-[16px] tw:w-full" ]
                    [ img
                        [ src <| imgSrcFallback productImage
                        , class "tw:rounded-[8px]"
                        , imageToSrcSet productImage
                        , imageSizes
                        , alt <| "Product Image for " ++ p.product.name
                        ]
                        []
                    , receiptProductMobileView
                        { nameView = Product.nameWithLotSize p.product p.variant
                        , sku = p.product.baseSKU ++ p.variant.skuSuffix
                        , quantity = p.quantity
                        , price = variantPrice p.variant
                        }
                    ]
                , viewIf (List.length p.errors > 0)
                    (div []
                        [ div [ class "tw:pt-[12px] text-danger" ]
                            [ text <| String.join ", " (List.map showCartItemError p.errors) ]
                        ]
                    )
                , viewIf (List.length p.warnings > 0)
                    (div []
                        [ div [ class "tw:pt-[12px] text-warning" ]
                            [ text <| String.join ", " (List.map showCartItemWarning p.warnings) ]
                        ]
                    )
                ]

        imageSizes =
            A.attribute "sizes" <|
                String.join ", "
                    [ "(max-width: 375px) 80px"
                    , "100px"
                    ]

        tableFooterMobile =
            receiptTotalMobileView
                { subTotal = totals.subTotal
                , total = finalTotal
                , tax = Just charges.tax
                , maybeAppliedCredit = Maybe.map (\amount -> centsMap negate amount) maybeAppliedCredit
                , memberDiscount = Nothing
                , couponDiscount = Maybe.map (\item -> { item | amount = centsMap negate item.amount }) charges.couponDiscount
                , shippingMethod = Maybe.map .charge charges.shippingMethod
                , priorityShipping = charges.priorityShipping
                , surcharges = charges.surcharges
                }
                (Just RemoveCoupon)

        maybeAppliedCredit =
            centsFromString creditString
                |> Maybe.map (limitStoreCredit checkoutDetails)

        totals =
            PageData.cartTotals checkoutDetails

        finalTotal =
            case maybeAppliedCredit of
                Nothing ->
                    totals.total

                Just credit ->
                    credit
                        |> centsMap2 (\total c -> total - c) totals.total
    in
    div [ class "tw:block tw:lg:hidden tw:pt-[20px]" ]
        [ div [] <| List.map productRow items
        , if charges.couponDiscount == Nothing then
            couponCodeForm couponCode couponErrors

          else
            text ""
        , div [ class "tw:pt-[24px]" ]
            [ tableFooterMobile
            ]
        ]


sameAddressesCheckbox : Bool -> Html Msg
sameAddressesCheckbox billingSameAsShipping =
    small [ class "d-inline-block form-check" ]
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


couponCodeForm couponCode couponErrors =
    div [ class "tw:flex tw:flex-col tw:pt-[20px]" ]
        [ div [ class "tw:flex tw:gap-[10px]" ]
            [ div [ class "tw:w-full tw:lg:w-[156px]" ]
                [ input
                    [ class "form-control tw:h-[42px]!"
                    , type_ "text"
                    , onInput CouponCode
                    , value couponCode
                    , A.placeholder "Coupon Code"
                    , Aria.label "Coupon Code"
                    ]
                    []
                ]
            , div []
                [ Button.view { defaultButton | label = "Apply", type_ = Button.TriggerMsg ApplyCoupon, style = Button.Outline }
                ]
            ]
        , couponErrors
            |> Maybe.map (\errs -> small [] [ Api.errorHtml errs ])
            |> Maybe.withDefault (text "")
        ]


summaryTableDesktop : PageData.CheckoutDetails -> String -> String -> Maybe (List String) -> Api.FormErrors -> Html Msg
summaryTableDesktop ({ items, charges } as checkoutDetails) creditString couponCode couponErrors errors =
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

        productRow p =
            let
                productImage =
                    productMainImage p.product
            in
            [ tr []
                [ td [ class "align-middle text-center" ]
                    [ img
                        [ src <| imgSrcFallback productImage
                        , class "tw:rounded-[8px]"
                        , imageToSrcSet productImage
                        , imageSizes
                        , alt <| "Product Image for " ++ p.product.name
                        ]
                        []
                    ]
                , td []
                    [ div [ class "font-weight-bold" ] [ Product.nameWithLotSize p.product p.variant ]
                    , small [ class "text-muted" ]
                        [ text <| "Item #" ++ p.product.baseSKU ++ p.variant.skuSuffix ]
                    ]
                , td [ class "text-center" ] [ text <| String.fromInt p.quantity ]
                , td [ class "text-right" ] [ text <| Format.cents <| variantPrice p.variant ]
                , td [ class "text-right" ]
                    [ text <| Format.cents <| centsMap ((*) p.quantity) <| variantPrice p.variant ]
                ]
            , viewIf (List.length p.errors > 0)
                (tr []
                    [ td [ colspan 5, class "text-danger" ]
                        [ text <| String.join ", " (List.map showCartItemError p.errors) ]
                    ]
                )
            , viewIf (List.length p.warnings > 0)
                (tr []
                    [ td [ colspan 5, class "text-warning" ]
                        [ text <| String.join ", " (List.map showCartItemWarning p.warnings) ]
                    ]
                )
            ]

        imageSizes =
            A.attribute "sizes" <|
                String.join ", "
                    [ "(max-width: 375px) 80px"
                    , "100px"
                    ]

        tableFooter =
            tfoot [] <|
                subTotalRow
                    :: maybeChargeRow (Maybe.map .charge charges.shippingMethod)
                    :: maybeChargeRow charges.priorityShipping
                    :: List.map chargeRow charges.surcharges
                    ++ [ taxRow
                       , storeCreditRow
                       , couponDiscountRow
                       , totalRow
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

        storeCreditRow =
            case maybeAppliedCredit of
                Nothing ->
                    text ""

                Just (Cents 0) ->
                    text ""

                Just credit ->
                    footerRow "Store Credit" (centsMap negate credit) ""

        totalRow =
            if charges.couponDiscount == Nothing then
                splitFooterRow "total-row-coupon-input"
                    "text-right font-weight-bold"
                    "Total"
                    finalTotal
                <|
                    div [ class "tw:flex tw:gap-[10px] tw:items-center" ]
                        [ couponCodeForm couponCode couponErrors
                        ]

            else
                footerRow "Total" finalTotal "font-weight-bold"

        maybeAppliedCredit =
            centsFromString creditString
                |> Maybe.map (limitStoreCredit checkoutDetails)

        couponDiscountRow =
            case charges.couponDiscount of
                Nothing ->
                    text ""

                Just { description, amount } ->
                    tr [ class "checkout-coupon-line" ]
                        [ td [ colspan 4, class "text-right clearfix" ]
                            [ button
                                [ class "btn btn-sm btn-link float-left py-0 tw:hover:underline"
                                , type_ "button"
                                , onClick RemoveCoupon
                                ]
                                [ small [] [ text "[Remove]" ] ]
                            , text <| description ++ ":"
                            ]
                        , td [ class "text-right" ]
                            [ text <| Format.cents <| centsMap negate amount ]
                        ]

        footerRow content amount rowClass =
            tr [ class rowClass ]
                [ td [ colspan 4, class "text-right" ] [ text <| content ++ ":" ]
                , td [ class "text-right" ] [ text <| Format.cents amount ]
                ]

        -- Similar to footerRow, but more customizable with the left cell split
        -- in half.
        splitFooterRow rowClass rightClass content amount splitContent =
            tr [ class rowClass ]
                [ td [ colspan 2 ]
                    [ splitContent ]
                , td [ colspan 2, class rightClass ]
                    [ text <| content ++ ":" ]
                , td [ class rightClass ]
                    [ text <| Format.cents amount ]
                ]

        totals =
            PageData.cartTotals checkoutDetails

        finalTotal =
            case maybeAppliedCredit of
                Nothing ->
                    totals.total

                Just credit ->
                    credit
                        |> centsMap2 (\total c -> total - c) totals.total
    in
    div [ class "tw:hidden tw:lg:block tw:pt-[20px]" ]
        [ table [ class "se-table" ]
            [ tableHeader
            , tbody [] <| List.concat (List.map productRow items)
            , tableFooter
            ]
        ]


successView : Time.Zone -> Int -> Bool -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
successView zone orderId guest locations orderDetails =
    let
        newAccountAlert =
            if not guest then
                text ""

            else
                div [ class "tw:pt-[20px]" ]
                    [ Alert.view
                        { defaultAlert
                            | content =
                                div [ class "tw:flex tw:gap-[8px] tw:items-center tw:justify-between tw:w-full" ]
                                    [ span [ class "tw:font-semibold" ]
                                        [ text <|
                                            String.join " "
                                                [ "To enhance your experience, consider registering on our website"
                                                , "to view order history."
                                                ]
                                        ]
                                    , Button.view { defaultButton | label = "Create an Account", type_ = Button.Link <| reverse CreateAccount, style = Button.Outline }
                                    ]
                        }
                    ]

        commentHtml =
            if not (String.isEmpty orderDetails.order.comment) then
                p [ class "tw:pl-[8px] tw:pt-[20px]" ]
                    [ b [] [ text "Comment: " ]
                    , text orderDetails.order.comment
                    ]

            else
                text ""

        orderDate =
            orderDetails.order.createdAt

        thankYouView =
            div [ class "tw:flex tw:p-[16px] tw:rounded-[16px] tw:border-[2px] tw:border-[rgba(77,170,154,0.4)] tw:gap-[12px]" ]
                [ handHeartSvg
                , div []
                    [ p [ class "tw:font-semibold tw:pb-[12px]" ] [ text "Thanks for your order!" ]
                    , p [] [ text "We will review your order today & will get in touch with you if we need any clarifications." ]
                    , p [ class "tw:font-semibold tw:opacity-70" ]
                        [ text "We will email you a tracking number once your order has shipped."
                        ]
                    ]
                ]
    in
    [ pageTitleWithSubView ("Order #" ++ String.fromInt orderId) (Format.date zone orderDate)
    , div [ class "tw:pt-[20px]" ]
        [ thankYouView
        ]
    , newAccountAlert
    , div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:gap-[20px] tw:pt-[20px]" ] <|
        case orderDetails.billingAddress of
            Just billingAddress ->
                [ div [ class "" ]
                    [ OrderDetails.addressCard locations
                        "Shipping Details"
                        orderDetails.shippingAddress
                    ]
                , div [ class "" ]
                    [ OrderDetails.addressCard locations
                        "Billing Details"
                        billingAddress
                    ]
                ]

            Nothing ->
                [ div [ class "" ]
                    [ OrderDetails.addressCard locations
                        "Shipping Details"
                        orderDetails.shippingAddress
                    ]
                ]
    , commentHtml
    , div [ class "tw:pt-[20px]" ] []
    , OrderDetails.orderTable orderDetails
    ]


type alias PaymentConfirmationModel =
    { showConfirmation : Bool
    , amount : Cents
    , f4l6Card : String
    , cardToken : String
    , customerCode : String
    }


mockConfirmationModel : PaymentConfirmationModel
mockConfirmationModel =
    -- Mainly use for testing purpose
    { showConfirmation = True
    , amount = Cents 1000
    , f4l6Card = "abc"
    , cardToken = "def"
    , customerCode = "111"
    }


initPaymentConfirmation : String -> Cents -> String -> String -> PaymentConfirmationModel
initPaymentConfirmation f4l6Card amount cardToken customerCode =
    { showConfirmation = False
    , amount = amount
    , f4l6Card = f4l6Card
    , cardToken = cardToken
    , customerCode = customerCode
    }


viewConfirmationOverlay : PaymentConfirmationModel -> Html Msg
viewConfirmationOverlay model =
    div [ class "card-page-overlay" ]
        [ div [ class "tw:bg-white tw:p-[32px] tw:rounded-[12px]" ]
            [ h4 [ class "tw:pb-[16px]" ] [ text "Confirm Payment" ]
            , [ text "Please confirm your payment details before proceeding:"
              , text ("Amount: " ++ Format.cents model.amount)
              , text ("Payment method ending in '" ++ String.slice 6 10 model.f4l6Card ++ "'")
              ]
                |> List.intersperse (br [] [])
                |> Html.address []
            , div [ class "tw:pt-[20px] tw:grid tw:grid-cols-2 tw:gap-[12px]" ]
                [ Button.view { defaultButton | label = "Cancel", style = Button.Outline, type_ = Button.TriggerMsg CancelPayment }
                , Button.view { defaultButton | label = "Confirm", type_ = Button.TriggerMsg ConfirmPayment }
                ]
            ]
        ]
