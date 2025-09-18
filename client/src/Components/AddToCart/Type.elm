module Components.AddToCart.Type exposing (..)

import Components.Svg exposing (..)
import Data.Api as Api
import Data.PageData as PageData exposing (getAmountBaseOnVariant)
import Data.Product exposing (ProductVariantId(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { clickStatus : Bool
    , amount : Int
    , requestStatus : WebData (Result Api.FormErrors ())
    , manualInput : Bool
    , originalAmount : Maybe Int
    , detailErrors : List PageData.CartItemError
    , detailWarnings : List PageData.CartItemWarning
    }


initAddToCart : Model
initAddToCart =
    { clickStatus = False
    , amount = 0
    , requestStatus = RemoteData.NotAsked
    , manualInput = False
    , originalAmount = Nothing
    , detailErrors = []
    , detailWarnings = []
    }


{-| Construct initial state base on the initial amount
-}
addToCartFromAmount : Int -> Model
addToCartFromAmount initialAmount =
    { initAddToCart
        | amount = initialAmount
        , clickStatus =
            if initialAmount == 0 then
                False

            else
                True
    }


addToCartFromVariantId : WebData PageData.CartDetails -> Maybe ProductVariantId -> Model
addToCartFromVariantId cartDetailRd variantIdMb =
    let
        firstVariantAmount =
            case ( cartDetailRd, variantIdMb ) of
                ( Success cartDetails, Just vId ) ->
                    getAmountBaseOnVariant vId cartDetails

                _ ->
                    0
    in
    addToCartFromAmount firstVariantAmount


{-| Amount change and session token meant to pass data to Main
-}
type alias AmountChange =
    Int


type Msg
    = None
      -- Add
    | TriggerAdd ProductVariantId
    | CallGetCartDetailEndpoint ProductVariantId AmountChange (WebData (Result Api.FormErrors String))
      -- Minus
    | TriggerMinus ProductVariantId
      -- Shared
    | CallSetCartEndpoint ProductVariantId Int AmountChange (WebData PageData.CartDetails)
      -- note: AmountChange is used so that the parent can listen to the amount change and update cart amount accordingly
    | UpdateAmountBaseOnCartDetail ProductVariantId AmountChange String (WebData (Result Api.FormErrors PageData.CartDetails))
      -- Manual
    | ManualAmountInputHandler Int
    | ResetAmount
    | SubmitManualAmount ProductVariantId
