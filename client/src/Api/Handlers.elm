module Api.Handlers exposing (..)

import Components.Svg exposing (..)
import Data.Api as Api
import Data.PageData as PageData
import Data.Product exposing (ProductVariantId(..))
import Data.User exposing (encodedCartToken)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData)


addToCustomerCart : Int -> ProductVariantId -> (WebData (Result Api.FormErrors String) -> msg) -> Cmd msg
addToCustomerCart quantity (ProductVariantId variantId) msg =
    let
        body =
            Encode.object
                [ ( "variant", Encode.int variantId )
                , ( "quantity", Encode.int quantity )
                ]
    in
    Api.post Api.CartAddCustomer
        |> Api.withJsonBody body
        |> Api.withErrorHandler (Decode.succeed "")
        |> Api.sendRequest msg


addToAnonymousCart : Maybe String -> Int -> ProductVariantId -> (WebData (Result Api.FormErrors String) -> msg) -> Cmd msg
addToAnonymousCart maybeSessionToken quantity (ProductVariantId variantId) msg =
    let
        body =
            Encode.object
                [ ( "variant", Encode.int variantId )
                , ( "quantity", Encode.int quantity )
                , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                ]

        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    Api.post Api.CartAddAnonymous
        |> Api.withJsonBody body
        |> Api.withStringErrorHandler
        |> Api.sendRequest msg


setCartAmount : Int -> PageData.CartItemId -> (WebData (Result Api.FormErrors PageData.CartDetails) -> msg) -> Cmd msg
setCartAmount quantity (PageData.CartItemId cartId) msg =
    let
        changed =
            [ ( String.fromInt cartId, Encode.int quantity ) ]

        body =
            Encode.object [ ( "quantities", Encode.object changed ) ]
    in
    Api.post Api.CartUpdateCustomer
        |> Api.withJsonBody body
        |> Api.withErrorHandler PageData.cartDetailsDecoder
        |> Api.sendRequest msg


setCartAmountAnonymous : Maybe String -> Int -> PageData.CartItemId -> (WebData (Result Api.FormErrors PageData.CartDetails) -> msg) -> Cmd msg
setCartAmountAnonymous maybeSessionToken quantity (PageData.CartItemId cartId) msg =
    let
        changed =
            [ ( String.fromInt cartId, Encode.int quantity ) ]

        body =
            Encode.object <|
                ( "quantities", Encode.object changed )
                    :: encodedCartToken maybeSessionToken
    in
    Api.post Api.CartUpdateAnonymous
        |> Api.withJsonBody body
        |> Api.withErrorHandler PageData.cartDetailsDecoder
        |> Api.sendRequest msg


getCartDetails : (WebData PageData.CartDetails -> msg) -> Cmd msg
getCartDetails msg =
    Api.get Api.CartDetailsCustomer
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest msg


getAnonymousCartDetails : Maybe String -> (WebData PageData.CartDetails -> msg) -> Cmd msg
getAnonymousCartDetails maybeCartToken msg =
    let
        parameters =
            Encode.object
                [ ( "sessionToken", Encode.string <| Maybe.withDefault "" maybeCartToken ) ]
    in
    Api.post Api.CartDetailsAnonymous
        |> Api.withJsonBody parameters
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest msg
