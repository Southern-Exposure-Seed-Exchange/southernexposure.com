module Pages.EditAddress exposing
    ( Form
    , Msg
    , initial
    , update
    , view
    )

import Components.Address.Address as Address exposing (AddressId(..))
import Components.Button as Button exposing (defaultButton)
import Components.Svg exposing (binSvg)
import Data.Api as Api
import Data.Locations as Locations exposing (AddressLocations)
import Data.PageData as PageData
import Data.Routing.Routing as Routing exposing (Route(..))
import Data.User as User exposing (AuthStatus)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Events exposing (onCheck, onClick, onSubmit)
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Utils.Update exposing (noCommand)
import Utils.View exposing (pageTitleView)



-- Model


type MobileTab
    = ShippingAddressTab
    | BillingAddressTab


type alias Form =
    { selectedAddress : Maybe AddressId
    , forms : Dict Int Address.Form
    , changeToDefault : Bool
    , mobileTab : MobileTab
    }


initial : Form
initial =
    { selectedAddress = Nothing
    , forms = Dict.empty
    , changeToDefault = False
    , mobileTab = ShippingAddressTab
    }



-- Update


type Msg
    = SelectShipping AddressId
    | SelectBilling AddressId
    | FormMsg AddressId Address.Msg
    | IsDefault Bool
    | Delete
    | DeleteResponse (WebData ())
    | Update
    | UpdateResponse AddressId (WebData (Result Api.FormErrors ()))
    | SetMobileTab MobileTab


update : Routing.Key -> Msg -> Form -> AuthStatus -> WebData PageData.AddressDetails -> ( Form, Cmd Msg )
update key msg model authStatus details =
    case msg of
        SelectShipping addressId ->
            details
                |> RemoteData.map (.shippingAddresses >> selectAddress addressId model)
                |> RemoteData.toMaybe
                |> Maybe.withDefault model
                |> noCommand

        SelectBilling addressId ->
            details
                |> RemoteData.map (.billingAddresses >> selectAddress addressId model)
                |> RemoteData.toMaybe
                |> Maybe.withDefault model
                |> noCommand

        FormMsg (AddressId id) subMsg ->
            { model
                | forms =
                    Dict.update id (Maybe.map <| Address.update subMsg) model.forms
            }
                |> noCommand

        IsDefault isDefault ->
            { model | changeToDefault = isDefault } |> noCommand

        Delete ->
            model.selectedAddress
                |> Maybe.map (deleteAddress authStatus)
                |> Maybe.withDefault Cmd.none
                |> (\cmd -> ( model, cmd ))

        DeleteResponse response ->
            case response of
                RemoteData.Success _ ->
                    ( initial, Routing.newUrl key MyAccount )

                _ ->
                    ( model, Cmd.none )

        Update ->
            case model.selectedAddress of
                Just (AddressId id) ->
                    Dict.get id model.forms
                        |> Maybe.map (updateAddress authStatus model.changeToDefault)
                        |> Maybe.withDefault Cmd.none
                        |> (\cmd -> ( model, cmd ))

                Nothing ->
                    ( model, Cmd.none )

        UpdateResponse (AddressId addressId) response ->
            case response of
                RemoteData.Success (Ok _) ->
                    ( initial, Routing.newUrl key MyAccount )

                RemoteData.Success (Err errors) ->
                    { model
                        | forms =
                            Dict.update addressId
                                (Maybe.map <| \form -> { form | errors = errors })
                                model.forms
                    }
                        |> noCommand

                RemoteData.Failure error ->
                    { model
                        | forms =
                            Dict.update addressId
                                (Maybe.map <| \form -> { form | errors = Api.apiFailureToError error })
                                model.forms
                    }
                        |> noCommand

                _ ->
                    ( model, Cmd.none )

        SetMobileTab value ->
            { initial | mobileTab = value } |> noCommand


selectAddress : AddressId -> Form -> List Address.Model -> Form
selectAddress ((AddressId i) as id) model addresses =
    let
        maybeAddress =
            findAddress id addresses

        insertForm address =
            Dict.insert i (Address.fromModel address) model.forms
    in
    if i == 0 then
        { model | selectedAddress = Nothing }

    else
        { model
            | selectedAddress = Just id
            , forms = Maybe.map insertForm maybeAddress |> Maybe.withDefault model.forms
        }


findAddress : AddressId -> List Address.Model -> Maybe Address.Model
findAddress id addresses =
    case addresses of
        [] ->
            Nothing

        addr :: addrs ->
            if addr.id == Just id then
                Just addr

            else
                findAddress id addrs


deleteAddress : AuthStatus -> AddressId -> Cmd Msg
deleteAddress authStatus (AddressId id) =
    case authStatus of
        User.Authorized _ ->
            Api.delete (Api.CustomerDeleteAddress id)
                |> Api.withJsonResponse (Decode.succeed ())
                |> Api.sendRequest DeleteResponse

        User.Anonymous ->
            Cmd.none


updateAddress : AuthStatus -> Bool -> Address.Form -> Cmd Msg
updateAddress authStatus isDefault ({ model } as addressForm) =
    let
        modelWithDefault =
            { model | isDefault = isDefault }

        withNewDefault =
            if isDefault then
                { addressForm | model = modelWithDefault }

            else
                addressForm
    in
    case ( authStatus, addressForm.model.id ) of
        ( User.Authorized _, Just (AddressId addressId) ) ->
            Api.post (Api.CustomerEditAddress addressId)
                |> Api.withJsonBody (Address.encode withNewDefault)
                |> Api.withErrorHandler (Decode.succeed ())
                |> Api.sendRequest (UpdateResponse <| AddressId addressId)

        _ ->
            Cmd.none



-- View


view : Form -> AddressLocations -> PageData.AddressDetails -> List (Html Msg)
view model locations { shippingAddresses, billingAddresses } =
    let
        addressSelect titleText msg addresses =
            div [ class "" ]
                [ label [ class "tw:text-[14px] tw:leading-[20px] tw:font-semibold" ]
                    [ text titleText ]
                , Address.select (AddressId >> msg)
                    model.selectedAddress
                    addresses
                    False
                ]

        addressForm =
            maybeSelectedIdAndForm
                |> Maybe.map
                    (\( id, f ) ->
                        ([ div [ class "tw:flex tw:justify-between" ]
                            [ p [ class "tw:opacity-70" ] [ text "Details" ]
                            , button
                                [ class "tw:flex tw:items-center tw:gap-[4px] tw:group tw:text-[rgba(30,12,3,0.4)] tw:hover:text-[rgba(214,34,70,1)] "
                                , disabled (maybeSelectedAddress == Nothing)
                                , type_ "button"
                                , onClick Delete
                                ]
                                [ binSvg "tw:fill-[rgba(30,12,3,0.4)] tw:group-hover:fill-[rgba(214,34,70,1)]"
                                , span [] [ text "Delete" ]
                                ]
                            ]
                         ]
                            ++ (List.map (\h -> Html.map (FormMsg (AddressId id)) h) <| Address.horizontalForm f locations)
                        )
                            |> div [ class "tw:p-[16px] tw:bg-[rgba(30,12,3,0.03)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px] tw:mb-[16px]" ]
                    )
                |> Maybe.withDefault (text "")

        defaultCheckbox =
            maybeSelectedAddress
                |> Maybe.map renderCheckbox
                |> Maybe.withDefault (text "")

        renderCheckbox address =
            if not address.isDefault then
                div [ class "form-group form-row align-items-center" ]
                    [ div [ class "col" ]
                        [ div [ class "form-check" ]
                            [ label [ class "form-check-label" ]
                                [ input
                                    [ class "form-check-input"
                                    , type_ "checkbox"
                                    , onCheck IsDefault
                                    ]
                                    []
                                , text <| " Make Default Address"
                                ]
                            ]
                        ]
                    ]

            else
                text ""

        maybeSelectedIdAndForm =
            model.selectedAddress
                |> Maybe.andThen
                    (\(AddressId id) ->
                        Dict.get id model.forms |> Maybe.map (Tuple.pair id)
                    )

        maybeSelectedAddress =
            model.selectedAddress
                |> Maybe.andThen
                    (\i ->
                        findAddress i <| shippingAddresses ++ billingAddresses
                    )

        buttons =
            div [ class "tw:flex tw:pt-[28px] tw:justify-center" ]
                [ Button.view
                    { defaultButton
                        | label = "Save"
                        , padding = Button.Width "tw:w-full tw:lg:w-[160px]"
                        , type_ =
                            if updateIsDisabled then
                                Button.Disabled

                            else
                                Button.FormSubmit
                    }
                ]

        updateIsDisabled =
            case ( maybeSelectedIdAndForm, maybeSelectedAddress ) of
                ( Just ( _, form ), Just address ) ->
                    if form.model == address && not model.changeToDefault then
                        True

                    else
                        False

                _ ->
                    True

        shippingAddressForm =
            addressSelect "Shipping Addresses" SelectShipping shippingAddresses

        billingAddressForm =
            addressSelect "Billing Addresses" SelectBilling billingAddresses

        mobileTabItemView : MobileTab -> Html Msg
        mobileTabItemView tab =
            let
                isSelected =
                    tab == model.mobileTab

                selectedClass =
                    if isSelected then
                        "tw:bg-white tw:rounded-[7px]!"

                    else
                        ""

                label =
                    case tab of
                        ShippingAddressTab ->
                            "Shipping addresses"

                        BillingAddressTab ->
                            "Billing addresses"
            in
            button
                [ class <| selectedClass ++ " tw:py-[6px] tw:px-[8px]"
                , onClick <| SetMobileTab tab
                ]
                [ span [] [ text label ] ]

        mobileTabView =
            div [ class "tw:bg-[rgba(30,12,3,0.06)] tw:p-[2px] tw:text-[13px] tw:leading-[16px] tw:grid tw:grid-cols-2 tw:rounded-[8px]" ]
                [ mobileTabItemView ShippingAddressTab
                , mobileTabItemView BillingAddressTab
                ]
    in
    [ pageTitleView "Edit Addresses"
    , div [ class "tw:pb-[20px] tw:block tw:lg:hidden" ]
        [ mobileTabView
        ]
    , Html.form [ onSubmit Update ]
        [ case model.mobileTab of
            ShippingAddressTab ->
                div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:px-0 tw:lg:px-[16px] tw:gap-[16px] tw:pb-[20px]" ]
                    [ shippingAddressForm
                    , div [ class "tw:hidden tw:lg:block" ]
                        [ billingAddressForm
                        ]
                    ]

            BillingAddressTab ->
                div [ class "tw:grid tw:grid-cols-1 tw:lg:grid-cols-2 tw:px-0 tw:lg:px-[16px] tw:gap-[16px] tw:pb-[20px]" ]
                    [ billingAddressForm
                    ]
        , addressForm
        , defaultCheckbox
        , buttons
        ]
    ]
