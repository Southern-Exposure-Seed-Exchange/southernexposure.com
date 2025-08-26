module Pages.Cart.Type exposing (..)

import Components.AddToCart.Type as AddToCart
import Data.Api exposing (FormErrors)
import Data.PageData as PageData exposing (CartItemId(..))
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData


type alias CartFormItem =
    { addToCart : AddToCart.Model
    }


formItemInit : CartFormItem
formItemInit =
    { addToCart = AddToCart.init
    }


type alias Model =
    { formItems : Dict Int CartFormItem
    }


initial : Model
initial =
    Model Dict.empty



-- UPDATE


type Msg
    = Remove PageData.CartItemId
    | UpdateResponse (RemoteData.WebData (Result FormErrors PageData.CartDetails))
    | AddToCartMsg PageData.CartItemId AddToCart.Msg
