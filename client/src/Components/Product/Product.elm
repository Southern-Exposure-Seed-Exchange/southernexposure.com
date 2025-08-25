module Components.Product.Product exposing (..)

{-| TODO: Refactor pagedata messages into separate msg & update
-}

import Components.Product.Type exposing (Model, Msg(..))
import Data.Api as Api
import Data.Product exposing (ProductId(..), ProductVariantId(..))
import Data.User as User exposing (AuthStatus)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import RemoteData exposing (WebData)
import Task
import Utils.Update exposing (noCommand)


type UpdateCartFormParam
    = Exact Int
    | Increase
    | Decrease


updateCartVariant : ProductVariantId -> Model -> Model
updateCartVariant variantId model =
    { model | variant = Just variantId }


updateCartFormQuantity : UpdateCartFormParam -> Model -> Model
updateCartFormQuantity param model =
    let
        defaultQuantity =
            1
    in
    { model
        | quantity =
            case param of
                Exact quantity ->
                    quantity

                Increase ->
                    model.quantity + 1

                Decrease ->
                    if model.quantity == defaultQuantity then
                        model.quantity

                    else
                        model.quantity - 1
    }


setCartFormToLoading : Model -> Cmd Msg -> ( Model, Cmd Msg )
setCartFormToLoading model cmd =
    ( { model | requestStatus = RemoteData.Loading }, cmd )


addToCustomerCart : Model -> ProductId -> Int -> ProductVariantId -> ( Model, Cmd Msg )
addToCustomerCart productModel pId quantity ((ProductVariantId variantId) as vId) =
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
        |> Api.sendRequest (SubmitAddToCartResponse quantity)
        |> setCartFormToLoading productModel


addToAnonymousCart : Maybe String -> Model -> ProductId -> Int -> ProductVariantId -> ( Model, Cmd Msg )
addToAnonymousCart maybeSessionToken productModel pId quantity ((ProductVariantId variantId) as vId) =
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
        |> Api.sendRequest (SubmitAddToCartResponse quantity)
        |> setCartFormToLoading productModel


updateCartFormRequestStatus : WebData (Result Api.FormErrors a) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateCartFormRequestStatus response ( model, cmd ) =
    let
        unitResponse =
            RemoteData.map (Result.map (always ())) response

        resetStatusCmd =
            case response of
                RemoteData.Loading ->
                    Cmd.none

                RemoteData.NotAsked ->
                    Cmd.none

                _ ->
                    Process.sleep (10 * 1000)
                        |> Task.andThen (always <| Task.succeed <| ())
                        |> Task.perform (\_ -> ResetCartFormStatus)
    in
    ( { model | requestStatus = unitResponse }, Cmd.batch [ cmd, resetStatusCmd ] )


update : AuthStatus -> Maybe String -> Msg -> ProductId -> Model -> ( Model, Cmd Msg )
update currentUser maybeSessionToken msg ((ProductId productId) as pId) model =
    case msg of
        ChangeCartFormVariantId variantId ->
            model
                |> updateCartVariant variantId
                |> noCommand

        ChangeCartFormQuantity quantity ->
            model
                |> updateCartFormQuantity (Exact quantity)
                |> noCommand

        IncreaseCartFormQuantity ->
            model
                |> updateCartFormQuantity Increase
                |> noCommand

        DecreaseCartFormQuantity ->
            model
                |> updateCartFormQuantity Decrease
                |> noCommand

        SubmitAddToCart defaultVariant ->
            let
                ( variantId, quantity ) =
                    ( Maybe.withDefault defaultVariant <| model.variant, model.quantity )

                performRequest : (Model -> ProductId -> Int -> ProductVariantId -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
                performRequest f =
                    f model pId quantity variantId
            in
            case currentUser of
                User.Authorized _ ->
                    performRequest addToCustomerCart

                User.Anonymous ->
                    -- ( model, Cmd.none )
                    performRequest (addToAnonymousCart maybeSessionToken)

        SubmitAddToCartResponse _ response ->
            model
                |> noCommand
                |> updateCartFormRequestStatus response

        ResetCartFormStatus ->
            ( { model | requestStatus = RemoteData.NotAsked }, Cmd.none )
