module Components.Product.Product exposing (..)

{-| TODO: Refactor pagedata messages into separate msg & update
-}

import Components.AddToCart.AddToCart as AddToCart
import Components.AddToCart.Type as AddToCart
import Components.ImageSlider.ImageSlider as ImageSlider
import Components.Product.Type exposing (Model, Msg(..))
import Data.PageData exposing (CartDetails)
import Data.Product exposing (ProductId(..), ProductVariantId(..))
import Data.Shared exposing (Shared)
import RemoteData exposing (WebData)
import Utils.Update exposing (noCommand)


update : Shared pmsg -> WebData CartDetails -> Msg -> ProductId -> Model -> ( Model, Cmd Msg )
update shared cartDetailRd msg _ model =
    case msg of
        ChangeCartFormVariantId variantId ->
            ( { model
                | variant = Just variantId
                , addToCart = AddToCart.addToCartFromVariantId cartDetailRd (Just variantId)
              }
            , Cmd.none
            )

        AddToCartMsg subMsg ->
            let
                ( newAddToCart, cmd ) =
                    AddToCart.update shared subMsg model.addToCart
            in
            ( { model | addToCart = newAddToCart }
            , Cmd.map AddToCartMsg cmd
            )

        ImageSliderMsg subMsg ->
            let
                ( newImageSlider, cmd ) =
                    ImageSlider.update shared subMsg model.imageSlider
            in
            ( { model | imageSlider = newImageSlider }
            , Cmd.map ImageSliderMsg cmd
            )
