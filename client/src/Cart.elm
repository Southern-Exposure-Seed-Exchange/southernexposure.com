module Cart
    exposing
        ( Form
        , initial
        , fromCartDetails
        , update
        , view
        )

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (class, colspan, src, type_, value, step, href, disabled)
import Html.Events exposing (onSubmit)
import Html.Events.Extra exposing (onClickPreventDefault)
import Json.Encode as Encode
import RemoteData
import Api
import Messages exposing (Msg(EditCartMsg), EditCartMessage(..))
import Models.Fields exposing (Cents(..), centsToString, centsMap)
import PageData exposing (CartDetails, CartItemId(..))
import Routing exposing (Route(ProductDetails, Checkout))
import User exposing (AuthStatus, User)
import Views.Images as Images
import Views.Utils exposing (icon, routeLinkAttributes, onIntInput, htmlOrBlank)


-- MODEL


type alias Form =
    { quantities : Dict Int Int
    }


initial : Form
initial =
    Form Dict.empty


fromCartDetails : CartDetails -> Form
fromCartDetails { items } =
    List.foldl
        (\item acc -> Dict.insert ((\(CartItemId i) -> i) item.id) item.quantity acc)
        Dict.empty
        items
        |> Form



-- UPDATE


update :
    EditCartMessage
    -> AuthStatus
    -> Maybe String
    -> Form
    -> CartDetails
    -> ( Form, Maybe CartDetails, Cmd Msg )
update msg authStatus maybeCartToken model details =
    case msg of
        Quantity itemId quantity ->
            ( { model
                | quantities =
                    Dict.update (fromCartItemId itemId)
                        (always <| Just quantity)
                        model.quantities
              }
            , Nothing
            , Cmd.none
            )

        Remove itemId ->
            ( model, Nothing, removeItem authStatus maybeCartToken model itemId )

        Submit ->
            ( model, Nothing, updateCart authStatus maybeCartToken model details )

        UpdateResponse response ->
            case response of
                RemoteData.Success details ->
                    ( initial, Just details, Cmd.none )

                _ ->
                    ( model, Nothing, Cmd.none )


updateCart : AuthStatus -> Maybe String -> Form -> CartDetails -> Cmd Msg
updateCart authStatus maybeCartToken { quantities } { items } =
    let
        changed =
            changedQuantities quantities items

        encodedQuantities =
            Encode.object <|
                [ ( "quantities", Encode.object changed ) ]
                    ++ encodedCartToken maybeCartToken
    in
        if List.isEmpty changed then
            Cmd.none
        else
            case authStatus of
                User.Anonymous ->
                    anonymousUpdateRequest encodedQuantities

                User.Authorized user ->
                    customerUpdateRequest user encodedQuantities


removeItem : AuthStatus -> Maybe String -> Form -> CartItemId -> Cmd Msg
removeItem authStatus maybeCartToken model itemId =
    let
        encodedDelete =
            Encode.object <|
                [ ( "quantities"
                  , Encode.object
                        [ ( toString <| fromCartItemId itemId, Encode.int 0 ) ]
                  )
                ]
                    ++ encodedCartToken maybeCartToken
    in
        case authStatus of
            User.Anonymous ->
                anonymousUpdateRequest encodedDelete

            User.Authorized user ->
                customerUpdateRequest user encodedDelete


anonymousUpdateRequest : Encode.Value -> Cmd Msg
anonymousUpdateRequest body =
    Api.post Api.CartUpdateAnonymous
        |> Api.withJsonBody body
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest (EditCartMsg << UpdateResponse)


customerUpdateRequest : User -> Encode.Value -> Cmd Msg
customerUpdateRequest user body =
    Api.post Api.CartUpdateCustomer
        |> Api.withToken user.authToken
        |> Api.withJsonBody body
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest (EditCartMsg << UpdateResponse)



-- VIEW


view : Form -> CartDetails -> List (Html Msg)
view { quantities } ({ items, charges } as cartDetails) =
    let
        itemCount =
            List.foldl (.quantity >> (+)) 0 items

        -- TODO: Add commas to format
        cartTable =
            table [ class "table table-striped table-sm cart-table" ]
                [ tableHeader
                , tbody [] <| List.map productRow items
                , tableFooter
                ]

        buttons =
            div [ class "form-group text-right" ]
                [ button [ class "btn btn-success", type_ "submit", disabled formIsUnchanged ]
                    [ icon "refresh", text " Update" ]
                , a (class "btn btn-primary ml-2" :: routeLinkAttributes Checkout)
                    [ icon "shopping-cart", text " Checkout" ]
                ]

        tableHeader =
            thead []
                [ tr []
                    [ th [ class "text-center" ] [ text "Quantity" ]
                    , th [] [ text "" ]
                    , th [] [ text "Name" ]
                    , th [ class "text-right" ] [ text "Item Price" ]
                    , th [ class "text-right" ] [ text "Item Total" ]
                    , th [ class "text-center" ] [ text "Remove" ]
                    ]
                ]

        productRow { id, product, variant, quantity } =
            tr []
                [ td [ class "text-center align-middle" ]
                    [ input
                        [ class "cart-quantity form-control mx-auto"
                        , type_ "number"
                        , value <| toString <| Maybe.withDefault 1 <| Dict.get (fromCartItemId id) quantities
                        , onIntInput <| EditCartMsg << Quantity id
                        , A.min "1"
                        , A.step "1"
                        ]
                        []
                    ]
                , td [ class "align-middle" ]
                    [ a (routeLinkAttributes <| ProductDetails product.slug)
                        [ img
                            [ src << Images.media <| "products/" ++ product.imageURL
                            , class "cart-product-image"
                            ]
                            []
                        ]
                    ]
                , td [ class "align-middle" ]
                    [ div [ class "font-weight-bold" ]
                        [ a (routeLinkAttributes <| ProductDetails product.slug)
                            [ text product.name ]
                        ]
                    , small
                        [ class "text-muted" ]
                        [ text <| "Item #" ++ product.baseSKU ++ variant.skuSuffix ]
                    ]
                , td [ class "text-right align-middle" ]
                    [ text <| "$" ++ centsToString variant.price ]
                , td [ class "text-right align-middle" ]
                    [ text <| "$" ++ centsToString (centsMap ((*) quantity) variant.price) ]
                , td [ class "text-center align-middle" ]
                    [ a [ class "text-danger", href "#", onClickPreventDefault <| EditCartMsg <| Remove id ]
                        [ icon "times" ]
                    ]
                ]

        tableFooter =
            tfoot [] <|
                [ footerRow "font-weight-bold" "Sub-Total" totals.subTotal ]
                    ++ List.map chargeRow charges.surcharges
                    ++ [ htmlOrBlank chargeRow charges.shippingMethod
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
            tr [ class rowClass ]
                [ td [ colspan 4, class "text-right" ] [ text <| content ++ ":" ]
                , td [ class "text-right" ] [ text <| "$" ++ centsToString amount ]
                , td [] []
                ]

        chargeRow charge =
            footerRow "" charge.description charge.amount

        totalRow =
            if totals.total /= totals.subTotal then
                footerRow "font-weight-bold" "Total" totals.total
            else
                text ""

        formIsUnchanged =
            changedQuantities quantities items
                |> List.isEmpty
    in
        if not (List.isEmpty items) then
            [ h1 [] [ text "Shopping Cart" ]
            , hr [] []
            , p [ class "text-center font-weight-bold" ]
                [ text <| "Total Items: " ++ toString itemCount ++ " Amount: $" ++ centsToString totals.total
                ]
            , form [ onSubmit <| EditCartMsg Submit ]
                [ cartTable
                , buttons
                ]
            ]
        else
            [ h1 [] [ text "Shopping Cart" ]
            , hr [] []
            , p [] [ text "You haven't added anything to your Shopping Cart yet!" ]
            ]



-- UTILS


fromCartItemId : CartItemId -> Int
fromCartItemId (CartItemId i) =
    i


changedQuantities : Dict Int Int -> List PageData.CartItem -> List ( String, Encode.Value )
changedQuantities quantities =
    List.foldl
        (\{ id, quantity } acc ->
            case Dict.get (fromCartItemId id) quantities of
                Nothing ->
                    acc

                Just formQuantity ->
                    if formQuantity /= quantity then
                        ( toString <| fromCartItemId id, Encode.int formQuantity )
                            :: acc
                    else
                        acc
        )
        []


encodedCartToken : Maybe String -> List ( String, Encode.Value )
encodedCartToken =
    Maybe.map (\token -> [ ( "sessionToken", Encode.string token ) ])
        >> Maybe.withDefault []
