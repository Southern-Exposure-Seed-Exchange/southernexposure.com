module Components.Product.Product exposing (..)

{-| TODO: Refactor pagedata messages into separate msg & update
-}

import Components.AddToCart.AddToCart as AddToCart
import Components.AddToCart.Type as AddToCart
import Components.Product.Type exposing (Model, Msg(..))
import Data.Product exposing (ProductId(..), ProductVariantId(..))
import Data.Shared exposing (Shared)
import Utils.Update exposing (noCommand)


update : Shared pmsg -> Msg -> ProductId -> Model -> ( Model, Cmd Msg )
update shared msg _ model =
    case msg of
        ChangeCartFormVariantId variantId ->
            model
                |> updateCartVariant variantId
                |> noCommand

        AddToCartMsg subMsg ->
            let
                ( newAddToCart, cmd ) =
                    AddToCart.update shared subMsg model.addToCart
            in
            ( { model | addToCart = newAddToCart }
            , Cmd.map AddToCartMsg cmd
            )



--------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------


updateCartVariant : ProductVariantId -> Model -> Model
updateCartVariant variantId model =
    { model | variant = Just variantId, addToCart = AddToCart.init }
