module Checkout
    exposing
        ( Form
        , initial
        , initialWithDefaults
        , Msg
        , OutMsg(..)
        , update
        , getCustomerDetails
        , getAnonymousDetails
        , view
        , successView
        )

import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, type_, colspan, src, for, id, rows, value, href, target, selected, checked, name, required, minlength)
import Html.Events exposing (onSubmit, onInput, onCheck, onClick, targetValue, on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)
import Address exposing (AddressId(..))
import Api
import Locations exposing (AddressLocations, Region)
import Models.Fields exposing (Cents(..), centsToString, centsMap2, centsMap, milligramsToString)
import PageData
import Ports
import Update.Utils exposing (nothingAndNoCommand)
import User exposing (AuthStatus)
import Views.Images as Images
import Views.Format as Format


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
    | Comment String
    | Submit
    | SubmitResponse (WebData (Result Api.FormErrors ( Int, AuthStatus )))
    | RefreshDetails (WebData PageData.CheckoutDetails)


type OutMsg
    = AnonymousOrderCompleted Int AuthStatus
    | CustomerOrderCompleted Int
    | DetailsRefreshed PageData.CheckoutDetails


update : Msg -> Form -> AuthStatus -> Maybe String -> ( Form, Maybe OutMsg, Cmd Msg )
update msg model authStatus maybeSessionToken =
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
                |> refreshDetails authStatus maybeSessionToken model

        ToggleShippingDefault makeDefault ->
            { model | makeShippingDefault = makeDefault }
                |> nothingAndNoCommand

        ShippingMsg subMsg ->
            updateAddressForm model.shippingAddress
                subMsg
                model
                (\f -> { model | shippingAddress = f })
                |> refreshDetails authStatus maybeSessionToken model

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
                ( model, Nothing, placeOrder model authStatus maybeSessionToken )

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
                        (\( field, fieldErrors ) ( generalErrors, shippingErrors, billingErrors ) ->
                            case String.split "-" field of
                                "shipping" :: xs ->
                                    ( generalErrors
                                    , ( String.join "" xs, fieldErrors ) :: shippingErrors
                                    , billingErrors
                                    )

                                "billing" :: xs ->
                                    ( generalErrors
                                    , shippingErrors
                                    , ( String.join "" xs, fieldErrors ) :: billingErrors
                                    )

                                _ ->
                                    ( ( field, fieldErrors ) :: generalErrors
                                    , shippingErrors
                                    , billingErrors
                                    )
                        )
                        ( [], [], [] )
                        (Dict.toList errors)

                updatedShippingForm =
                    case model.shippingAddress of
                        ExistingAddress _ ->
                            model.shippingAddress

                        NewAddress addressForm ->
                            NewAddress { addressForm | errors = Dict.fromList shippingErrors }

                updatedBillingForm =
                    case model.billingAddress of
                        ExistingAddress _ ->
                            model.billingAddress

                        NewAddress addressForm ->
                            NewAddress { addressForm | errors = Dict.fromList billingErrors }

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


refreshDetails : AuthStatus -> Maybe String -> Form -> Form -> ( Form, Maybe a, Cmd Msg )
refreshDetails authStatus maybeSessionToken oldModel newModel =
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
                        )

        applyArguments cmdFunction =
            case commandArguments of
                Just args ->
                    cmdFunction args

                Nothing ->
                    Cmd.none

        anonymousCommand ( maybeCountry, maybeRegion, _ ) =
            case maybeSessionToken of
                Nothing ->
                    Cmd.none

                Just token ->
                    getAnonymousDetails RefreshDetails token maybeCountry maybeRegion

        commandArguments =
            case ( oldModel.shippingAddress, newModel.shippingAddress ) of
                ( ExistingAddress id1, ExistingAddress id2 ) ->
                    if id1 /= id2 then
                        Just ( Nothing, Nothing, Just id2 )
                    else
                        Nothing

                ( NewAddress oldForm, NewAddress newForm ) ->
                    if oldForm.model.country == newForm.model.country then
                        case ( oldForm.model.state, newForm.model.state ) of
                            ( Locations.Custom _, Locations.Custom _ ) ->
                                Nothing

                            ( oldState, newState ) ->
                                if oldState /= newState then
                                    Just
                                        ( Just newForm.model.country
                                        , Just newState
                                        , Nothing
                                        )
                                else
                                    Nothing
                    else
                        Just
                            ( Just newForm.model.country
                            , Just newForm.model.state
                            , Nothing
                            )

                ( _, ExistingAddress id ) ->
                    Just ( Nothing, Nothing, Just id )

                ( _, NewAddress form ) ->
                    Just
                        ( Just form.model.country
                        , Just form.model.state
                        , Nothing
                        )
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
    in
        Api.post Api.CheckoutDetailsCustomer
            |> Api.withJsonBody data
            |> Api.withJsonResponse PageData.checkoutDetailsDecoder
            |> Api.withToken token
            |> Api.sendRequest msg


getAnonymousDetails :
    (WebData PageData.CheckoutDetails -> msg)
    -> String
    -> Maybe String
    -> Maybe Region
    -> Cmd msg
getAnonymousDetails msg sessionToken maybeCountry maybeRegion =
    let
        data =
            Encode.object
                [ ( "region", encodeMaybe Locations.regionEncoder maybeRegion )
                , ( "country", encodeMaybe Encode.string maybeCountry )
                , ( "sessionToken", Encode.string sessionToken )
                ]
    in
        Api.post Api.CheckoutDetailsAnonymous
            |> Api.withJsonBody data
            |> Api.withJsonResponse PageData.checkoutDetailsDecoder
            |> Api.sendRequest msg


placeOrder : Form -> AuthStatus -> Maybe String -> Cmd Msg
placeOrder model authStatus maybeSessionToken =
    let
        customerData =
            Encode.object
                [ ( "shippingAddress", encodedShippingAddress )
                , ( "billingAddress", encodedBillingAddress )
                , ( "comment", Encode.string model.comment )
                ]

        anonymousData =
            Encode.object
                [ ( "shippingAddress", encodedShippingAddress )
                , ( "billingAddress", encodedBillingAddress )
                , ( "comment", Encode.string model.comment )
                , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                , ( "email", Encode.string model.email )
                , ( "password", Encode.string model.password )
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
            case authStatus of
                User.Anonymous ->
                    Decode.map2 (,)
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
                    |> Api.withJsonResponse decoder
                    |> Api.withErrorHandler SubmitResponse

            User.Authorized user ->
                Api.post Api.CheckoutPlaceOrderCustomer
                    |> Api.withToken user.authToken
                    |> Api.withJsonBody customerData
                    |> Api.withJsonResponse decoder
                    |> Api.withErrorHandler SubmitResponse


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
                        |> Maybe.map (flip Address.card locations)
                        |> Maybe.withDefault (text "")
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

        errorHtml =
            if List.isEmpty config.generalErrors then
                text ""
            else
                config.generalErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "text-danger" ]

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
            , errorHtml
            , addressHtml
            ]


addressCard : List (Html msg) -> Html msg
addressCard contents =
    div [ class "col-md-6" ]
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


successView : msg -> Int -> Bool -> AddressLocations -> PageData.OrderDetails -> List (Html msg)
successView logoutMsg orderId newAccountCreated locations orderDetails =
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
            [ text <| "Order #" ++ toString orderId
            , small [] [ text <| " " ++ Format.date orderDate ]
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
