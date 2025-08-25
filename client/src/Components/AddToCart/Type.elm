module Components.AddToCart.Type exposing (..)

import Components.Svg exposing (..)
import Data.Api as Api
import Data.PageData as PageData
import Data.Product exposing (ProductVariantId(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)


type alias Config =
    ()


type alias Model =
    { clickStatus : Bool
    , amount : Int
    , requestStatus : WebData (Result Api.FormErrors ())
    , manualInput : Bool
    , originalAmount : Maybe Int
    }


init : Model
init =
    { clickStatus = False
    , amount = 0
    , requestStatus = RemoteData.NotAsked
    , manualInput = False
    , originalAmount = Nothing
    }


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
