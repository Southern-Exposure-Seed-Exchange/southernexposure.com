module Components.AddToCart.AddToCart exposing (..)

import Api.Handlers exposing (addToAnonymousCart, addToCustomerCart, getAnonymousCartDetails, getCartDetails, setCartAmount, setCartAmountAnonymous)
import Components.AddToCart.Type exposing (..)
import Components.Form as Form
import Components.Svg exposing (..)
import Data.PageData as PageData
import Data.Product exposing (ProductVariantId(..))
import Data.Shared exposing (Shared)
import Data.User as User
import Dict exposing (get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import RemoteData
import Utils.View exposing (icon)


update : Shared pmsg -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        TriggerAdd vId ->
            ( { model | requestStatus = RemoteData.Loading, clickStatus = True }
            , case shared.currentUser of
                User.Authorized _ ->
                    addToCustomerCart 1 vId (CallGetCartDetailEndpoint vId 1)

                User.Anonymous ->
                    addToAnonymousCart shared.maybeSessionToken 1 vId (CallGetCartDetailEndpoint vId 1)
            )

        CallGetCartDetailEndpoint vId amountChange response ->
            case response of
                RemoteData.Success (Ok sessionToken) ->
                    case shared.currentUser of
                        User.Authorized _ ->
                            ( model, getCartDetails (\res -> UpdateAmountBaseOnCartDetail vId amountChange sessionToken (RemoteData.map (\c -> Ok c) res)) )

                        User.Anonymous ->
                            ( model
                            , getAnonymousCartDetails shared.maybeSessionToken
                                (\res -> UpdateAmountBaseOnCartDetail vId amountChange sessionToken (RemoteData.map (\c -> Ok c) res))
                            )

                _ ->
                    ( { model | requestStatus = toUnitResponse response }, Cmd.none )

        TriggerMinus vId ->
            let
                -- We don't have to call `GetCartDetailEndpoint` to get the current amount, because we know
                -- `Minus` is only accessible after adding once, and after adding, the amount value is ensured to be correct
                newAmount =
                    model.amount - 1
            in
            if newAmount >= 0 then
                ( { model | requestStatus = RemoteData.Loading }
                , case shared.currentUser of
                    User.Authorized _ ->
                        getCartDetails (CallSetCartEndpoint vId newAmount -1)

                    User.Anonymous ->
                        getAnonymousCartDetails shared.maybeSessionToken (CallSetCartEndpoint vId newAmount -1)
                )

            else
                ( model, Cmd.none )

        CallSetCartEndpoint vId newAmount amountChange res ->
            case res of
                RemoteData.Success cartDetails ->
                    let
                        cartIdMaybe =
                            PageData.getCartIdBaseOnVariant vId cartDetails
                    in
                    ( model
                    , case ( shared.currentUser, cartIdMaybe ) of
                        ( User.Authorized _, Just cartId ) ->
                            setCartAmount newAmount cartId (UpdateAmountBaseOnCartDetail vId amountChange "")

                        ( User.Anonymous, Just cartId ) ->
                            setCartAmountAnonymous shared.maybeSessionToken newAmount cartId (UpdateAmountBaseOnCartDetail vId amountChange "")

                        _ ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateAmountBaseOnCartDetail vId _ _ res ->
            case res of
                RemoteData.Success (Ok cartDetails) ->
                    let
                        newAmount =
                            PageData.getAmountBaseOnVariant vId cartDetails
                    in
                    ( { model
                        | amount = newAmount
                        , requestStatus = RemoteData.Success <| Ok ()
                        , manualInput = False
                        , originalAmount = Nothing
                        , clickStatus =
                            if newAmount == 0 then
                                False

                            else
                                True
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | requestStatus = toUnitResponse res }, Cmd.none )

        ManualAmountInputHandler val ->
            ( { model
                | amount = val
                , originalAmount =
                    if model.manualInput == False then
                        Just model.amount

                    else
                        model.originalAmount
                , manualInput = True
              }
            , Cmd.none
            )

        ResetAmount ->
            case model.originalAmount of
                Just a ->
                    ( { model | amount = a, manualInput = False, originalAmount = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SubmitManualAmount vId ->
            let
                amountChange =
                    case model.originalAmount of
                        Just a ->
                            model.amount - a

                        Nothing ->
                            0
            in
            ( { model | requestStatus = RemoteData.Loading }
            , case shared.currentUser of
                User.Authorized _ ->
                    getCartDetails (CallSetCartEndpoint vId model.amount amountChange)

                User.Anonymous ->
                    getAnonymousCartDetails shared.maybeSessionToken (CallSetCartEndpoint vId model.amount amountChange)
            )



---------------------------------------------------------------
-- Util
---------------------------------------------------------------


toUnitResponse : RemoteData.RemoteData e (Result x b) -> RemoteData.RemoteData e (Result x ())
toUnitResponse res =
    RemoteData.map (Result.map (always ())) res



---------------------------------------------------------------
-- View
---------------------------------------------------------------


type ProductFormStyle
    = Card
    | Detail
    | Checkout


loadingIcon =
    icon "spinner fa-spin"


customNumberView : ProductFormStyle -> Model -> ProductVariantId -> Html Msg
customNumberView style model vId =
    let
        { class_, fillClass, buttonSizeClass, formClass } =
            case style of
                Card ->
                    { class_ = "tw:bg-[rgba(29,127,110,1)] tw:text-white"
                    , fillClass = "tw:fill-white"
                    , buttonSizeClass = "tw:h-[40px] tw:w-[40px] tw:hover:bg-[rgb(17,75,65)]"
                    , formClass = "tw:w-[46px] text-center tw:border-white"
                    }

                Detail ->
                    { class_ = "tw:bg-white tw:border tw:border-[rgba(29,127,110,1)]"
                    , fillClass = "tw:fill-black"
                    , buttonSizeClass = "tw:w-[48px] tw:h-[48px] tw:hover:bg-[rgb(219,219,219)]"
                    , formClass = "tw:w-[46px] tw:text-center tw:border-[rgba(29,127,110,1)]"
                    }

                Checkout ->
                    { class_ = "tw:bg-white tw:border tw:border-[rgba(29,127,110,1)]"
                    , fillClass = "tw:fill-black"
                    , buttonSizeClass = "tw:w-[48px] tw:h-[40px] tw:hover:bg-[rgb(219,219,219)]"
                    , formClass = "tw:w-[46px] tw:text-center tw:border-[rgba(29,127,110,1)]"
                    }
    in
    div [ class <| class_ ++ " tw:flex tw:shrink-0 tw:rounded-[8px] tw:overflow-hidden" ]
        [ if model.manualInput then
            button
                [ type_ "button"
                , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
                , onClick
                    (if model.requestStatus == RemoteData.Loading then
                        None

                     else
                        ResetAmount
                    )
                ]
                [ icon "undo-alt"
                ]

          else
            button
                [ type_ "button"
                , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
                , onClick
                    (if model.requestStatus == RemoteData.Loading then
                        None

                     else
                        TriggerMinus vId
                    )
                ]
                [ minusSvg fillClass
                ]
        , div [ class "tw:flex tw:items-center tw:justify-center tw:px-[4px] tw:grow" ]
            [ case model.requestStatus of
                RemoteData.Loading ->
                    loadingIcon

                _ ->
                    Form.numberView
                        (formClass ++ " tw:block no-arrow tw:py-[2px] tw:pl-[6px] tw:hover:border tw:focus:border tw:rounded-[4px]")
                        model.amount
                        ManualAmountInputHandler
            ]
        , if model.manualInput then
            button
                [ type_ "button"
                , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
                , onClick
                    (if model.requestStatus == RemoteData.Loading then
                        None

                     else
                        SubmitManualAmount vId
                    )
                ]
                [ icon "check"
                ]

          else
            button
                [ type_ "button"
                , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
                , onClick
                    (if model.requestStatus == RemoteData.Loading then
                        None

                     else
                        TriggerAdd vId
                    )
                ]
                [ plusSvg fillClass
                ]
        ]


view : Config -> ProductVariantId -> Model -> Html Msg
view _ vId model =
    if not model.clickStatus then
        button
            [ type_ "button"
            , class "tw:w-[160px] tw:bg-[rgba(77,170,154,1)] tw:text-white tw:flex tw:px-[16px] tw:py-[8px] tw:rounded-[8px]! tw:gap-[8px] tw:items-center tw:justify-center tw:leading-[24px]"
            , onClick (TriggerAdd vId)
            ]
        <|
            case model.requestStatus of
                RemoteData.Loading ->
                    [ loadingIcon ]

                _ ->
                    [ shoppingCartSvgSmall
                    , div [] [ text "Add to cart" ]
                    ]

    else
        div [ class "tw:w-[160px]" ]
            [ customNumberView Card model vId
            ]


statusView : Model -> Html Msg
statusView model =
    case model.requestStatus of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "tw:py-[8px] text-warning font-weight-bold small" ]
                [ icon "spinner fa-spin mr-1"
                , text "Adding to Cart"
                ]

        RemoteData.Success (Ok _) ->
            div [ class "tw:py-[8px] tw:text-[rgb(77,170,154)] font-weight-bold small" ]
                [ icon "check-circle mr-1"
                , text "Added to Cart!"
                ]

        RemoteData.Success (Err errors) ->
            div [ class "tw:py-[8px] text-danger font-weight-bold small" ]
                [ icon "times mr-1"
                , text "Error Adding To Cart: "
                , br [] []
                , case get "variant" errors of
                    Just variantValidation ->
                        text <| " " ++ String.join "\n" variantValidation

                    Nothing ->
                        text ""
                , case get "quantity" errors of
                    Just quantityValidation ->
                        text <| " " ++ String.join "\n" quantityValidation

                    Nothing ->
                        text ""
                ]

        RemoteData.Failure _ ->
            div [ class "tw:py-[8px] text-danger font-weight-bold small" ]
                [ icon "times mr-1"
                , text "Error Adding To Cart!"
                ]
