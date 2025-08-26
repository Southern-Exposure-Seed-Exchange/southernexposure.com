module Pages.Cart.Cart exposing
    ( fromCartDetails
    , update
    , view
    )

import Components.AddToCart.AddToCart as AddToCart
import Components.AddToCart.Type as AddToCart
import Components.Aria as Aria
import Components.Button as Button exposing (defaultButton)
import Components.Product.Views as ProductView
import Components.Svg exposing (..)
import Data.Api as Api
import Data.Fields exposing (Cents(..), centsMap)
import Data.PageData as PageData exposing (CartDetails, CartItemId(..), showCartItemError, showCartItemWarning)
import Data.Product as Product exposing (InventoryPolicy(..), variantPrice)
import Data.Routing.Routing exposing (Route(..), reverse)
import Data.Shared exposing (Shared)
import Data.User as User exposing (AuthStatus, unauthorized)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Json.Encode as Encode
import Pages.Cart.Type exposing (..)
import RemoteData
import Utils.Format as Format
import Utils.View exposing (htmlOrBlank, pageTitleView, rawHtml, routeLinkAttributes)



-- MODEL


fromCartDetails : CartDetails -> Model
fromCartDetails { items } =
    List.foldl
        (\item acc ->
            Dict.insert ((\(CartItemId i) -> i) item.id)
                { addToCart =
                    let
                        addToCart =
                            AddToCart.init
                    in
                    { addToCart
                        | amount = item.quantity
                        , clickStatus = True
                        , detailWarnings = item.warnings
                        , detailErrors = item.errors
                    }
                }
                acc
        )
        Dict.empty
        items
        |> Model


update :
    Shared pmsg
    -> Msg
    -> Model
    -> CartDetails
    -> ( Model, Maybe CartDetails, Cmd Msg )
update shared msg model _ =
    case msg of
        Remove itemId ->
            ( model, Nothing, removeItem shared.currentUser shared.maybeSessionToken itemId )

        UpdateResponse response ->
            case response of
                RemoteData.Success (Ok cartDetails) ->
                    ( initial, Just cartDetails, Cmd.none )

                _ ->
                    ( model, Nothing, Cmd.none )

        AddToCartMsg (CartItemId cartId) subMsg ->
            let
                formItem =
                    Maybe.withDefault formItemInit <| Dict.get cartId model.formItems

                ( newAddToCart, addToCartCmd ) =
                    AddToCart.update shared subMsg formItem.addToCart

                maybeCartDetails =
                    case subMsg of
                        AddToCart.UpdateAmountBaseOnCartDetail _ _ _ (RemoteData.Success (Ok cartDetails)) ->
                            Just cartDetails

                        _ ->
                            Nothing

                newFormItem =
                    { formItem | addToCart = newAddToCart }
            in
            ( { model | formItems = Dict.insert cartId newFormItem model.formItems }
            , maybeCartDetails
            , Cmd.map (AddToCartMsg (CartItemId cartId)) addToCartCmd
            )


removeItem : AuthStatus -> Maybe String -> CartItemId -> Cmd Msg
removeItem authStatus maybeCartToken itemId =
    let
        encodedDelete =
            Encode.object <|
                ( "quantities"
                , Encode.object
                    [ ( String.fromInt <| fromCartItemId itemId, Encode.int 0 ) ]
                )
                    :: User.encodedCartToken maybeCartToken
    in
    case authStatus of
        User.Anonymous ->
            anonymousUpdateRequest encodedDelete

        User.Authorized _ ->
            customerUpdateRequest encodedDelete


anonymousUpdateRequest : Encode.Value -> Cmd Msg
anonymousUpdateRequest body =
    Api.post Api.CartUpdateAnonymous
        |> Api.withJsonBody body
        |> Api.withErrorHandler PageData.cartDetailsDecoder
        |> Api.sendRequest UpdateResponse


customerUpdateRequest : Encode.Value -> Cmd Msg
customerUpdateRequest body =
    Api.post Api.CartUpdateCustomer
        |> Api.withJsonBody body
        |> Api.withErrorHandler PageData.cartDetailsDecoder
        |> Api.sendRequest UpdateResponse



-- VIEW


view : AuthStatus -> Model -> CartDetails -> List (Html Msg)
view authStatus { formItems } ({ items, charges } as cartDetails) =
    let
        itemCount =
            List.foldl (.quantity >> (+)) 0 items

        checkoutEnabled =
            List.all
                (\item ->
                    if item.variant.inventoryPolicy == RequireStock then
                        item.quantity <= item.variant.quantity

                    else
                        True
                )
                items

        -- TODO: Add commas to format
        cartTable =
            div [ class "tw:grow" ]
                [ div [ class "tw:flex tw:flex-col" ] <|
                    List.map
                        (\i ->
                            div []
                                [ productRowMobile i
                                , productRowDesktop i
                                ]
                        )
                        items
                ]

        totalView =
            div [ class "tw:bg-[rgba(167,215,197,0.3)] tw:p-[20px] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[12px]" ]
                [ p [ class "tw:text-[18px] tw:leading-[24px] tw:font-semibold" ] [ text "Your cart" ]
                , div [ class "tw:flex" ]
                    [ span [ class "tw:grow" ] [ text "Items:" ]
                    , span [] [ text <| String.fromInt itemCount ]
                    ]
                , tableFooter
                , div
                    [ class "tw:pt-[12px] tw:flex tw:flex-col tw:gap-[12px]" ]
                    [ Button.view
                        { defaultButton
                            | label =
                                if authStatus == unauthorized then
                                    " Checkout as a guest"

                                else
                                    " Checkout"
                            , type_ =
                                if not checkoutEnabled then
                                    Button.Disabled

                                else
                                    Button.Link <| reverse Checkout
                            , iconEnd = Just arrowRightSvg
                        }
                    ]
                ]

        buttons =
            div [ class "tw:w-full tw:lg:w-[283px] tw:shrink-0 tw:pb-[500px] tw:lg:pb-0" ]
                [ totalView
                , viewIf (authStatus == unauthorized) <|
                    div [ class "tw:py-[10px]" ]
                        [ a
                            (class "btn-link btn tw:font-semibold! p-0 text-right col ml"
                                :: routeLinkAttributes (Login (Just <| reverse <| Checkout) True)
                            )
                            [ text "Already have an Account?" ]
                        ]
                ]

        binButton id product =
            button
                [ class "tw:p-[6px] tw:cursor-pointer tw:group"
                , onClick <| Remove id
                , Aria.label <| "Remove " ++ product.name ++ " From Cart"
                ]
                [ binSvg "tw:fill-[rgba(30,12,3,0.4)] tw:group-hover:fill-[rgba(214,34,70,1)]" ]

        productRowMobile { id, product, variant, quantity, errors, warnings } =
            div [ class "tw:block tw:lg:hidden tw:border-b tw:border-[rgba(30,12,3,0.06)] tw:py-[20px]" ]
                [ div [ class "tw:flex tw:gap-[16px]" ]
                    [ div [ class "tw:shrink-0" ]
                        [ ProductView.productImageLinkView "tw:w-[84px] tw:h-[65px]" product (Just variant.id)
                        ]
                    , div [ class "tw:grow tw:flex tw:flex-col" ]
                        [ a ([ class "tw:font-semibold" ] ++ (routeLinkAttributes <| ProductDetails product.slug <| Just variant.id))
                            [ Product.nameWithLotSize product variant ]
                        , ProductView.renderItemNumber (ProductView.getItemNumber product (Just variant))
                        , div [ class "tw:flex tw:items-center tw:gap-[8px] tw:pt-[6px]" ]
                            [ p [ class "tw:text-[20px] tw:leading-[28px] tw:font-semibold" ]
                                [ text <| Format.cents <| centsMap ((*) quantity) <| variantPrice variant
                                ]
                            , if quantity > 1 then
                                p [ class "tw:text-[12px] tw:leading-[16px] tw:opacity-50 tw:pt-[4px]" ]
                                    [ text <|
                                        "("
                                            ++ (Format.cents <| variantPrice variant)
                                            ++ " / pc"
                                            ++ ")"
                                    ]

                              else
                                p [] []
                            ]
                        ]
                    ]
                , Html.map (AddToCartMsg id) <|
                    AddToCart.statusView
                        (Maybe.withDefault AddToCart.init <|
                            Maybe.map .addToCart <|
                                Dict.get (fromCartItemId id) formItems
                        )
                , div [ class "tw:flex tw:justify-between tw:pt-[20px]" ]
                    [ div [ class "tw:flex tw:flex-col tw:items-start" ]
                        [ Html.map (AddToCartMsg id) <|
                            AddToCart.view ()
                                variant.id
                                (Maybe.withDefault AddToCart.init <|
                                    Maybe.map .addToCart <|
                                        Dict.get (fromCartItemId id) formItems
                                )
                        ]
                    , binButton id product
                    ]
                ]

        productRowDesktop { id, product, variant, quantity, errors, warnings } =
            div [ class "tw:hidden tw:lg:block tw:border-b tw:border-[rgba(30,12,3,0.06)] tw:py-[20px]" ]
                [ div [ class "tw:flex" ]
                    [ div [ class "tw:shrink-0" ]
                        [ ProductView.productImageLinkView "tw:w-[169px] tw:h-[130px]" product (Just variant.id)
                        ]
                    , div [ class "tw:pl-[28px] tw:pr-[10px] tw:grow tw:flex tw:flex-col" ]
                        [ a ([ class "tw:font-semibold" ] ++ (routeLinkAttributes <| ProductDetails product.slug <| Just variant.id))
                            [ Product.nameWithLotSize product variant ]
                        , ProductView.renderItemNumber (ProductView.getItemNumber product (Just variant))
                        , div [ class "tw:grow" ] []
                        , div [ class "tw:flex tw:flex-col tw:items-start" ]
                            [ Html.map (AddToCartMsg id) <|
                                AddToCart.view ()
                                    variant.id
                                    (Maybe.withDefault AddToCart.init <|
                                        Maybe.map .addToCart <|
                                            Dict.get (fromCartItemId id) formItems
                                    )
                            ]
                        ]
                    , div [ class "tw:w-[155px] tw:shrink-0" ]
                        [ div [ class "tw:flex" ]
                            [ div [ class "tw:grow tw:flex tw:flex-col tw:items-center" ]
                                [ p [ class "tw:pt-[3px] tw:text-[20px] tw:leading-[28px] tw:font-semibold" ]
                                    [ text <| Format.cents <| centsMap ((*) quantity) <| variantPrice variant
                                    ]
                                , if quantity > 1 then
                                    p [ class "tw:text-[12px] tw:leading-[16px] tw:opacity-80" ] [ text <| (Format.cents <| variantPrice variant) ++ " / pc" ]

                                  else
                                    p [] []
                                ]
                            , binButton id product
                            ]
                        ]
                    ]
                , Html.map (AddToCartMsg id) <|
                    AddToCart.statusView
                        (Maybe.withDefault AddToCart.init <|
                            Maybe.map .addToCart <|
                                Dict.get (fromCartItemId id) formItems
                        )
                ]

        tableFooter =
            div [ class "tw:flex tw:flex-col tw:gap-[12px]" ] <|
                footerRow "font-weight-bold" "Sub-Total" totals.subTotal
                    :: List.map chargeRow charges.surcharges
                    ++ [ htmlOrBlank chargeRow <|
                            Maybe.map .charge charges.shippingMethod
                       , taxRow
                       , totalRow
                       ]

        totals =
            PageData.cartTotals cartDetails

        taxRow =
            if charges.tax.amount == Cents 0 then
                text ""

            else
                chargeRow charges.tax

        footerRow rowClass content amount =
            div [ class <| rowClass ++ " tw:flex" ]
                [ div [ class "tw:grow" ] [ text <| content ++ ":" ]
                , div [ class "" ] [ text <| Format.cents amount ]
                ]

        chargeRow charge =
            footerRow "" charge.description charge.amount

        totalRow =
            if totals.total /= totals.subTotal then
                footerRow "font-weight-bold" "Total" totals.total

            else
                text ""

        titleView =
            pageTitleView "Your cart"
    in
    if not (List.isEmpty items) then
        [ titleView
        , viewIf cartDetails.isDisabled <|
            div [ class "alert alert-danger static-page" ]
                [ rawHtml cartDetails.disabledMessage ]
        , div [ class "tw:pl-0 tw:lg:pl-[16px] tw:flex tw:gap-[20px] tw:flex-col tw:lg:flex-row" ]
            [ cartTable
            , buttons
            ]
        ]

    else
        [ titleView
        , p [ class "tw:px-0 tw:lg:px-[16px]" ] [ text "You haven't added anything to your Shopping Cart yet!" ]
        ]



-- Note: Can't implement this button due to no remove all endpoint
-- clearCartView =
--     div [ class "tw:flex tw:pb-[12px]" ]
--         [ span [ class "tw:text-[rgba(30,12,3,0.7)]" ] [ text <| "Items: " ++ String.fromInt itemCount ]
--         , span [ class "tw:grow" ] []
--         , button
--             [ class "tw:hover:text-[rgba(214,34,70,1)] tw:text-[rgba(30,12,3,0.4)] tw:hover:text-underline"
--             -- , onClick RemoveAll
--             , Aria.label <| "Remove all from Cart"
--             ]
--             [ text "Clear shopping cart" ]
--         ]
-- UTILS


fromCartItemId : CartItemId -> Int
fromCartItemId (CartItemId i) =
    i
