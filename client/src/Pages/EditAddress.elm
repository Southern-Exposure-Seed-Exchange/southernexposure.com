module Pages.EditAddress exposing
    ( Form
    , Msg
    , initial
    , update
    , view
    )

import Address exposing (AddressId(..))
import Api
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onSubmit)
import Json.Decode as Decode
import Locations exposing (AddressLocations)
import PageData
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Update.Utils exposing (noCommand)
import User exposing (AuthStatus)



-- Model


type alias Form =
    { selectedAddress : Maybe AddressId
    , forms : Dict Int Address.Form
    , changeToDefault : Bool
    }


initial : Form
initial =
    { selectedAddress = Nothing
    , forms = Dict.empty
    , changeToDefault = False
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
            div [ class "text-center form-group" ]
                [ label []
                    [ span [ class "font-weight-bold" ] [ text titleText ]
                    , Address.select (AddressId >> msg)
                        model.selectedAddress
                        addresses
                        False
                    ]
                ]

        addressForm =
            maybeSelectedIdAndForm
                |> Maybe.map
                    (\( id, f ) ->
                        Address.horizontalForm f locations
                            |> div []
                            |> Html.map (FormMsg (AddressId id))
                    )
                |> Maybe.withDefault (text "")

        defaultCheckbox =
            maybeSelectedAddress
                |> Maybe.map renderCheckbox
                |> Maybe.withDefault (text "")

        renderCheckbox address =
            if not address.isDefault then
                div [ class "form-group form-row align-items-center" ]
                    [ div [ class "col-sm-3" ] []
                    , div [ class "col" ]
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
            div [ class "form-group text-right" ]
                [ button
                    [ class "btn btn-success mr-2"
                    , disabled updateIsDisabled
                    , type_ "submit"
                    ]
                    [ text "Update" ]
                , button
                    [ class "btn btn-danger"
                    , disabled (maybeSelectedAddress == Nothing)
                    , type_ "button"
                    , onClick Delete
                    ]
                    [ text "Delete" ]
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
    in
    [ h1 [] [ text "Edit Addresses" ]
    , hr [] []
    , Html.form [ onSubmit Update ]
        [ div [ class "row" ]
            [ div [ class "col text-right" ]
                [ addressSelect "Shipping Addresses" SelectShipping shippingAddresses ]
            , div [ class "col text-left" ]
                [ addressSelect "Billing Addresses" SelectBilling billingAddresses ]
            ]
        , addressForm
        , defaultCheckbox
        , buttons
        ]
    ]
