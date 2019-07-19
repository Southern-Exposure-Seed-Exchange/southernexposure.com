module Checkout exposing
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
    , view
    )

import Address exposing (AddressId(..))
import Api
import Dict
import Html exposing (..)
import Html.Attributes as A exposing (attribute, checked, class, colspan, for, href, id, minlength, name, required, rows, selected, src, step, target, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Locations exposing (AddressLocations, Region)
import Models.Fields exposing (Cents(..), centsFromString, centsMap, centsMap2, milligramsToString)
import OrderDetails
import PageData
import Ports
import RemoteData exposing (WebData)
import Time
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.Format as Format
import Views.Images as Images



-- Model


type CheckoutAddress
    = ExistingAddress AddressId
    | NewAddress Address.Form


type alias Form =
    { email : String
    , password : String
    , passwordConfirm : String
    , shippingAddress : CheckoutAddress
    , makeShippingDefault : Bool
    , billingAddress : CheckoutAddress
    , makeBillingDefault : Bool
    , billingSameAsShipping : Bool
    , storeCredit : String
    , memberNumber : Maybe String
    , couponCode : String
    , comment : String
    , errors : Api.FormErrors
    }


initial : Form
initial =
    { email = ""
    , password = ""
    , passwordConfirm = ""
    , shippingAddress = NewAddress Address.initialForm
    , makeShippingDefault = False
    , billingAddress = NewAddress Address.initialForm
    , makeBillingDefault = False
    , billingSameAsShipping = False
    , storeCredit = ""
    , memberNumber = Nothing
    , couponCode = ""
    , comment = ""
    , errors = Api.initialErrors
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
    { email = ""
    , password = ""
    , passwordConfirm = ""
    , shippingAddress = shippingAddress
    , makeShippingDefault = isNew shippingAddress
    , billingAddress = billingAddress
    , makeBillingDefault = isNew billingAddress
    , billingSameAsShipping = False
    , storeCredit = ""
    , memberNumber = Nothing
    , couponCode = ""
    , comment = ""
    , errors = Api.initialErrors
    }



-- Update


type Msg
    = Email String
    | Password String
    | PasswordConfirm String
    | SelectShipping Int
    | ToggleShippingDefault Bool
    | ShippingMsg Address.Msg
    | SelectBilling Int
    | ToggleBillingDefault Bool
    | BillingMsg Address.Msg
    | BillingSameAsShipping Bool
    | StoreCredit String
    | MemberNumber String
    | CouponCode String
    | Comment String
    | ApplyCoupon
    | RemoveCoupon
    | Submit
    | TokenReceived String
    | SubmitResponse (WebData (Result Api.FormErrors ( Int, AuthStatus )))
    | RefreshDetails (WebData (Result Api.FormErrors PageData.CheckoutDetails))


type OutMsg
    = AnonymousOrderCompleted Int AuthStatus
    | CustomerOrderCompleted Int
    | DetailsRefreshed PageData.CheckoutDetails


subscriptions : Sub Msg
subscriptions =
    Ports.stripeTokenReceived TokenReceived


update : Msg -> Form -> AuthStatus -> Maybe String -> WebData PageData.CheckoutDetails -> ( Form, Maybe OutMsg, Cmd Msg )
update msg model authStatus maybeSessionToken checkoutDetails =
    case msg of
        Email email ->
            { model | email = email } |> nothingAndNoCommand

        Password password ->
            { model | password = password } |> nothingAndNoCommand

        PasswordConfirm passwordConfirm ->
            { model | passwordConfirm = passwordConfirm } |> nothingAndNoCommand

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

        MemberNumber memberNumber ->
            { model | memberNumber = Just memberNumber }
                |> refreshDetails authStatus maybeSessionToken False model

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
            if model.password /= model.passwordConfirm then
                ( { model
                    | errors =
                        Dict.update "passwordConfirm"
                            (always <| Just [ "Your passwords do not match." ])
                            model.errors
                  }
                , Nothing
                , Ports.scrollToTop
                )

            else
                case checkoutDetails of
                    RemoteData.Success details ->
                        let
                            totals =
                                PageData.cartTotals details

                            storeCredit =
                                model.storeCredit
                                    |> centsFromString
                                    |> Maybe.map (limitStoreCredit details)

                            (Cents finalTotal) =
                                case storeCredit of
                                    Nothing ->
                                        totals.total

                                    Just credit ->
                                        centsMap2 (\total c -> total - c) totals.total credit

                            customerEmail =
                                case authStatus of
                                    User.Authorized user ->
                                        user.email

                                    User.Anonymous ->
                                        model.email
                        in
                        ( model, Nothing, Ports.collectStripeToken ( customerEmail, finalTotal ) )

                    _ ->
                        ( model, Nothing, Cmd.none )

        TokenReceived stripeTokenId ->
            ( model, Nothing, placeOrder model authStatus maybeSessionToken stripeTokenId checkoutDetails )

        SubmitResponse (RemoteData.Success (Ok ( orderId, newAuthStatus ))) ->
            let
                outMsg =
                    if authStatus == User.Anonymous && newAuthStatus /= authStatus then
                        AnonymousOrderCompleted orderId newAuthStatus

                    else
                        CustomerOrderCompleted orderId
            in
            ( initial, Just outMsg, Cmd.none )

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

                addAddresErrorIfExisting prefix address =
                    case address of
                        ExistingAddress _ ->
                            Dict.update prefix (always <| Dict.get (prefix ++ "-") errors)

                        NewAddress _ ->
                            identity
            in
            ( { model
                | shippingAddress = updatedShippingForm
                , billingAddress = updatedBillingForm
                , errors =
                    Dict.fromList generalErrors
                        |> addAddresErrorIfExisting "shipping" model.shippingAddress
                        |> addAddresErrorIfExisting "billing" model.billingAddress
              }
            , Nothing
            , Ports.scrollToTop
            )

        SubmitResponse _ ->
            model |> nothingAndNoCommand

        RefreshDetails (RemoteData.Success (Ok details)) ->
            ( model
            , Just (DetailsRefreshed details)
            , Cmd.none
            )

        RefreshDetails (RemoteData.Success (Err formErrors)) ->
            { model | errors = formErrors } |> nothingAndNoCommand

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


refreshDetails : AuthStatus -> Maybe String -> Bool -> Form -> Form -> ( Form, Maybe a, Cmd Msg )
refreshDetails authStatus maybeSessionToken forceUpdate oldModel newModel =
    let
        refreshCommand =
            case authStatus of
                User.Anonymous ->
                    applyArguments anonymousCommand

                User.Authorized user ->
                    applyArguments
                        (\( maybeCountry, maybeRegion, maybeAddressId ) ->
                            getCustomerDetails RefreshDetails
                                user.authToken
                                maybeCountry
                                maybeRegion
                                maybeAddressId
                                newModel.memberNumber
                                newModel.couponCode
                        )

        applyArguments cmdFunction =
            if shouldUpdate then
                cmdFunction addressArguments

            else
                Cmd.none

        anonymousCommand ( maybeCountry, maybeRegion, _ ) =
            case maybeSessionToken of
                Nothing ->
                    Cmd.none

                Just token ->
                    getAnonymousDetails RefreshDetails
                        token
                        maybeCountry
                        maybeRegion
                        (Maybe.withDefault "" newModel.memberNumber)
                        newModel.couponCode

        shouldUpdate =
            addressChanged || memberNumberChanged || forceUpdate

        addressChanged =
            case ( oldModel.shippingAddress, newModel.shippingAddress ) of
                ( ExistingAddress id1, ExistingAddress id2 ) ->
                    id1 /= id2

                ( NewAddress oldForm, NewAddress newForm ) ->
                    if oldForm.model.country == newForm.model.country then
                        case ( oldForm.model.state, newForm.model.state ) of
                            ( Locations.Custom _, Locations.Custom _ ) ->
                                False

                            ( oldState, newState ) ->
                                oldState /= newState

                    else
                        True

                _ ->
                    True

        memberNumberChanged =
            case ( oldModel.memberNumber, newModel.memberNumber ) of
                ( Just oldNumber, Just newNumber ) ->
                    (String.length oldNumber > 3 && String.length newNumber <= 3)
                        || (String.length newNumber > 3 && String.length oldNumber <= 3)

                ( old, new ) ->
                    old /= new

        addressArguments =
            case newModel.shippingAddress of
                ExistingAddress id ->
                    ( Nothing, Nothing, Just id )

                NewAddress addrForm ->
                    ( Just addrForm.model.country, Just addrForm.model.state, Nothing )
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
    -> String
    -> Maybe String
    -> Maybe Region
    -> Maybe AddressId
    -> Maybe String
    -> String
    -> Cmd msg
getCustomerDetails msg token maybeCountry maybeRegion maybeAddressId maybeMemberNumber couponCode =
    let
        data =
            Encode.object
                [ ( "region", encodeMaybe Locations.regionEncoder maybeRegion )
                , ( "country", encodeMaybe Encode.string maybeCountry )
                , ( "addressId", encodeMaybe (\(AddressId i) -> Encode.int i) maybeAddressId )
                , ( "memberNumber", encodedMemberNumber )
                , ( "couponCode", encodeStringAsMaybe couponCode )
                ]

        encodedMemberNumber =
            maybeMemberNumber
                |> Maybe.andThen
                    (\num ->
                        if String.length num > 3 then
                            Just num

                        else
                            Nothing
                    )
                |> encodeMaybe Encode.string

        encodedCouponCode =
            if String.isEmpty couponCode then
                Encode.null

            else
                Encode.string couponCode
    in
    Api.post Api.CheckoutDetailsCustomer
        |> Api.withJsonBody data
        |> Api.withErrorHandler PageData.checkoutDetailsDecoder
        |> Api.withToken token
        |> Api.sendRequest msg


getAnonymousDetails :
    (WebData (Result Api.FormErrors PageData.CheckoutDetails) -> msg)
    -> String
    -> Maybe String
    -> Maybe Region
    -> String
    -> String
    -> Cmd msg
getAnonymousDetails msg sessionToken maybeCountry maybeRegion memberNumber couponCode =
    let
        data =
            Encode.object
                [ ( "region", encodeMaybe Locations.regionEncoder maybeRegion )
                , ( "country", encodeMaybe Encode.string maybeCountry )
                , ( "memberNumber", Encode.string memberNumber )
                , ( "sessionToken", Encode.string sessionToken )
                , ( "couponCode", encodeStringAsMaybe couponCode )
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


placeOrder : Form -> AuthStatus -> Maybe String -> String -> WebData PageData.CheckoutDetails -> Cmd Msg
placeOrder model authStatus maybeSessionToken stripeTokenId checkoutDetails =
    let
        customerData =
            Encode.object
                [ ( "shippingAddress", encodedShippingAddress )
                , ( "billingAddress", encodedBillingAddress )
                , ( "storeCredit", encodedStoreCredit )
                , ( "memberNumber", encodedMemberNumber )
                , ( "couponCode", encodeStringAsMaybe model.couponCode )
                , ( "comment", Encode.string model.comment )
                , ( "stripeToken", Encode.string stripeTokenId )
                ]

        anonymousData =
            Encode.object
                [ ( "shippingAddress", encodedShippingAddress )
                , ( "billingAddress", encodedBillingAddress )
                , ( "memberNumber", encodedMemberNumber )
                , ( "couponCode", encodeStringAsMaybe model.couponCode )
                , ( "comment", Encode.string model.comment )
                , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                , ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
                , ( "stripeToken", Encode.string stripeTokenId )
                ]

        encodedStoreCredit =
            model.storeCredit
                |> centsFromString
                |> Maybe.map (\(Cents c) -> Encode.int c)
                |> Maybe.withDefault Encode.null

        encodedMemberNumber =
            Encode.string <|
                case model.memberNumber of
                    Nothing ->
                        RemoteData.toMaybe checkoutDetails
                            |> Maybe.map .memberNumber
                            |> Maybe.withDefault ""

                    Just str ->
                        str

        encodedShippingAddress =
            encodeAddress model.shippingAddress model.makeShippingDefault

        encodedBillingAddress =
            if model.billingSameAsShipping then
                RemoteData.toMaybe checkoutDetails
                    |> Maybe.map
                        (isDefaultAddress model.shippingAddress
                            >> encodeAddress model.shippingAddress
                        )
                    |> Maybe.withDefault (encodeAddress model.shippingAddress model.makeShippingDefault)

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
                    Decode.map2 Tuple.pair
                        (Decode.field "orderId" Decode.int)
                        (Decode.field "authData" User.decoder)

                User.Authorized _ ->
                    Decode.map (\orderId -> ( orderId, authStatus ))
                        (Decode.field "orderId" Decode.int)
    in
    case authStatus of
        User.Anonymous ->
            Api.post Api.CheckoutPlaceOrderAnonymous
                |> Api.withJsonBody anonymousData
                |> Api.withErrorHandler decoder
                |> Api.sendRequest SubmitResponse

        User.Authorized user ->
            Api.post Api.CheckoutPlaceOrderCustomer
                |> Api.withToken user.authToken
                |> Api.withJsonBody customerData
                |> Api.withErrorHandler decoder
                |> Api.sendRequest SubmitResponse


{-| TODO: This is probably repeated other places, stick in a Json.Utils module.
-}
encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null



-- View


view : Form -> AuthStatus -> AddressLocations -> PageData.CheckoutDetails -> List (Html Msg)
view model authStatus locations checkoutDetails =
    let
        generalErrors =
            Api.getErrorHtml "" model.errors

        registrationCard =
            case authStatus of
                User.Authorized _ ->
                    text ""

                User.Anonymous ->
                    div [ class "mb-3" ]
                        [ div [ class "" ]
                            [ h4 [] [ text "Login Details" ]
                            , registrationForm model
                            ]
                        ]

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
                    , generalErrors =
                        Dict.get "billing" model.errors
                            |> Maybe.withDefault []
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
                    div [ class "col-6 d-flex flex-column" ]
                        [ h5 [] [ text "Store Credit" ]
                        , p []
                            [ text "You have "
                            , b [] [ text <| Format.cents checkoutDetails.storeCredit ]
                            , text " of store credit available."
                            ]
                        , div [ class "input-group mt-auto mb-3" ]
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
                                ]
                                []
                            ]
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

        memberNumberForm =
            div [ class "col-6" ]
                [ h5 [] [ text "Member Number" ]
                , p []
                    [ text <|
                        "Returning customers can receive a 5% discount by "
                            ++ "entering their member number. You can find your member "
                            ++ "number on your catalog mailing label and on previous order "
                            ++ "invoices."
                    ]
                , div [ class "form-group" ]
                    [ input
                        [ class "form-control"
                        , type_ "text"
                        , minlength 4
                        , onInput MemberNumber
                        , value <|
                            Maybe.withDefault checkoutDetails.memberNumber model.memberNumber
                        ]
                        []
                    ]
                ]
    in
    [ h1 [] [ text "Checkout" ]
    , hr [] []
    , form [ onSubmit Submit ]
        [ generalErrors
        , registrationCard
        , div [ class "row mb-3" ]
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
                }
            , billingCard
            ]
        , div [ class "row mb-3" ]
            [ storeCreditForm, memberNumberForm ]
        , div [ class "mb-3" ]
            [ h4 [] [ text "Order Summary" ]
            , summaryTable checkoutDetails
                model.storeCredit
                model.couponCode
                (Dict.get "coupon" model.errors)
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
                [ text "Pay with Credit Card" ]
            ]
        ]
    ]


registrationForm : Form -> Html Msg
registrationForm model =
    let
        -- TODO: Use Views.Form when we pull it out of the Address module.
        fieldLabel inputId content =
            label [ class "mb-0", for inputId ] [ text content ]

        hasErrors =
            not (Dict.isEmpty model.errors)

        inputClass errors =
            if hasErrors && List.isEmpty errors then
                class "form-control is-valid"

            else if hasErrors && not (List.isEmpty errors) then
                class "form-control is-invalid"

            else
                class "form-control"

        emailInput =
            input
                [ id "emailInput"
                , inputClass emailErrors
                , name "email"
                , type_ "email"
                , required True
                , onInput Email
                , value model.email
                , autocomplete "email"
                ]
                []

        emailErrors =
            Dict.get "email" model.errors |> Maybe.withDefault []

        passwordInput =
            input
                [ id "passwordInput"
                , inputClass passwordErrors
                , name "password"
                , type_ "password"
                , required True
                , onInput Password
                , value model.password
                , autocomplete "new-password"
                , minlength 8
                ]
                []

        passwordErrors =
            Dict.get "password" model.errors |> Maybe.withDefault []

        passwordConfirmInput =
            input
                [ id "passwordConfirmInput"
                , inputClass confirmErrors
                , name "passwordConfirm"
                , type_ "password"
                , required True
                , onInput PasswordConfirm
                , value model.passwordConfirm
                , autocomplete "new-password"
                , minlength 8
                ]
                []

        confirmErrors =
            Dict.get "passwordConfirm" model.errors |> Maybe.withDefault []

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
    div [ class "row" ]
        [ div [ class "col-md-4" ]
            [ wrapper
                [ fieldLabel "emailInput" "Email"
                , emailInput
                , errorHtml emailErrors
                ]
            ]
        , div [ class "col-md-4" ]
            [ wrapper
                [ fieldLabel "passwordInput" "Password"
                , passwordInput
                , errorHtml passwordErrors
                ]
            ]
        , div [ class "col-md-4" ]
            [ wrapper
                [ fieldLabel "passwordConfirmInput" "Confirm Password"
                , passwordConfirmInput
                , errorHtml confirmErrors
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
            Address.select config.selectMsg addressId config.selectAddresses True
                |> List.singleton
                |> div [ class "form-group" ]
    in
    addressCard
        [ h4 [ class "card-title" ]
            [ span [ class "mr-4" ] [ text config.cardTitle ]
            , sameAsShippingCheckbox
            ]
        , selectHtml
        , Api.errorHtml config.generalErrors
        , addressHtml
        ]


addressCard : List (Html msg) -> Html msg
addressCard contents =
    div [ class "col-md-6" ]
        [ div [ class "card" ] [ div [ class "card-body pt-3" ] contents ] ]


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


summaryTable : PageData.CheckoutDetails -> String -> String -> Maybe (List String) -> Html Msg
summaryTable ({ items, charges } as checkoutDetails) creditString couponCode couponErrors =
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
                , td [ class "text-center" ] [ text <| String.fromInt quantity ]
                , td [ class "text-right" ] [ text <| Format.cents variant.price ]
                , td [ class "text-right" ] [ text <| Format.cents (centsMap ((*) quantity) variant.price) ]
                ]

        tableFooter =
            tfoot [] <|
                subTotalRow
                    :: maybeChargeRow charges.shippingMethod
                    :: List.map chargeRow charges.surcharges
                    ++ [ taxRow
                       , storeCreditRow
                       , memberDiscountRow
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
                    div [ class "form-group form-inline" ]
                        [ input
                            [ class "form-control form-control-sm"
                            , type_ "text"
                            , onInput CouponCode
                            , value couponCode
                            , A.placeholder "Coupon Code"
                            ]
                            []
                        , button
                            [ class "btn btn-sm btn-link ml-2"
                            , type_ "button"
                            , onClick ApplyCoupon
                            ]
                            [ text "Apply" ]
                        , couponErrors
                            |> Maybe.map (\errs -> small [] [ Api.errorHtml errs ])
                            |> Maybe.withDefault (text "")
                        ]

            else
                footerRow "Total" finalTotal "font-weight-bold"

        maybeAppliedCredit =
            centsFromString creditString
                |> Maybe.map (limitStoreCredit checkoutDetails)

        memberDiscountRow =
            charges.memberDiscount
                |> Maybe.map negateChargeAmount
                |> maybeChargeRow

        couponDiscountRow =
            case charges.couponDiscount of
                Nothing ->
                    text ""

                Just { description, amount } ->
                    tr [ class "checkout-coupon-line" ]
                        [ td [ colspan 4, class "text-right clearfix" ]
                            [ button
                                [ class "btn btn-sm btn-link float-left py-0"
                                , type_ "button"
                                , onClick RemoveCoupon
                                ]
                                [ small [] [ text "[Remove]" ] ]
                            , text <| description ++ ":"
                            ]
                        , td [ class "text-right" ]
                            [ text <| Format.cents <| centsMap negate amount ]
                        ]

        negateChargeAmount c =
            { c | amount = centsMap negate c.amount }

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
    table [ class "table table-striped table-sm checkout-products-table" ]
        [ tableHeader
        , tbody [] <| List.map productRow items
        , tableFooter
        ]


successView : Time.Zone -> msg -> Int -> Bool -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
successView zone logoutMsg orderId newAccountCreated locations orderDetails =
    let
        newAccountAlert =
            if not newAccountCreated then
                text ""

            else
                div [ class "alert alert-info clearfix" ]
                    [ button [ class "btn btn-primary float-right ml-3", onClick logoutMsg ]
                        [ text "Log Out" ]
                    , text <|
                        "You have been automatically logged into "
                            ++ "your new account. If you are using a public computer, "
                            ++ "please log out to keep your account details private."
                    ]

        contactLink =
            a
                [ href <|
                    "mailto:gardens@southernexposure.com"
                        ++ "?subject=SESE+Website+Contact(Order+#"
                        ++ String.fromInt orderId
                        ++ ")"
                , target "_blank"
                ]
                [ text "contact us" ]

        commentHtml =
            if not (String.isEmpty orderDetails.order.comment) then
                p []
                    [ b [] [ text "Comment: " ]
                    , text orderDetails.order.comment
                    ]

            else
                text ""

        orderDate =
            orderDetails.order.createdAt
    in
    [ h1 [] [ text "Order Complete" ]
    , hr [] []
    , h2 [] [ text "Thanks for your order!" ]
    , newAccountAlert
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
        [ text <| "Order #" ++ String.fromInt orderId
        , small [] [ text <| " " ++ Format.date zone orderDate ]
        ]
    , div [ class "row mb-3" ]
        [ div [ class "col-6" ]
            [ OrderDetails.addressCard locations
                "Shipping Details"
                orderDetails.shippingAddress
            ]
        , div [ class "col-6" ]
            [ OrderDetails.addressCard locations
                "Billing Details"
                orderDetails.billingAddress
            ]
        ]
    , commentHtml
    , OrderDetails.orderTable orderDetails
    ]
