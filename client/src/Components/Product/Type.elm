module Components.Product.Type exposing (..)

import Components.AddToCart as AddToCart
import Data.Api as Api
import Data.PageData exposing (CartItemId(..))
import Data.Product exposing (ProductId, ProductVariantId)
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData exposing (WebData)


type alias Model =
    { variant : Maybe ProductVariantId
    , quantity : Int
    , requestStatus : WebData (Result Api.FormErrors ())
    }


initProductModel : Model
initProductModel =
    { variant = Nothing, quantity = 1, requestStatus = RemoteData.NotAsked }


type Msg
    = ChangeCartFormVariantId ProductVariantId
    | ChangeCartFormQuantity Int
    | IncreaseCartFormQuantity
    | DecreaseCartFormQuantity
    | SubmitAddToCart ProductVariantId
    | SubmitAddToCartResponse Int (WebData (Result Api.FormErrors String))
    | ResetCartFormStatus
