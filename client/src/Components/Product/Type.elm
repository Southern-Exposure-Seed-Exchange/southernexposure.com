module Components.Product.Type exposing (..)

import Components.AddToCart.Type as AddToCart
import Data.PageData exposing (CartItemId(..))
import Data.Product exposing (ProductVariantId)
import Html exposing (..)


type alias Model =
    { variant : Maybe ProductVariantId
    , addToCart : AddToCart.Model
    }


initProductModel : Model
initProductModel =
    { variant = Nothing
    , addToCart = AddToCart.init
    }


type Msg
    = ChangeCartFormVariantId ProductVariantId
    | AddToCartMsg AddToCart.Msg
