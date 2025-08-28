module Components.Product.Type exposing (..)

import Array
import Components.AddToCart.Type as AddToCart
import Components.ImageSlider.Type as ImageSlider
import Data.Fields exposing (blankImage)
import Data.PageData exposing (CartItemId(..))
import Data.Product exposing (ProductVariantId)
import Html exposing (..)


type alias Model =
    { variant : Maybe ProductVariantId
    , addToCart : AddToCart.Model
    , imageSlider : ImageSlider.Model
    }


initProductModel : Model
initProductModel =
    { variant = Nothing
    , addToCart = AddToCart.init
    , imageSlider = ImageSlider.mkModel "" Array.empty
    }


type Msg
    = ChangeCartFormVariantId ProductVariantId
    | AddToCartMsg AddToCart.Msg
    | ImageSliderMsg ImageSlider.Msg
