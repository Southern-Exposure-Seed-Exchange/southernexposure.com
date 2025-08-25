module Pages.Cart.Type exposing (..)

import Data.Api exposing (FormErrors)
import Data.PageData as PageData exposing (CartItemId(..))
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData


type alias Form =
    { quantities : Dict Int Int
    }


initial : Form
initial =
    Form Dict.empty



-- UPDATE


type Msg
    = SetFormQuantity PageData.CartItemId Int
    | IncreaseFormQuantity PageData.CartItemId
    | DecreaseFormQuantity PageData.CartItemId
    | Remove PageData.CartItemId
    | Submit
    | UpdateResponse (RemoteData.WebData (Result FormErrors PageData.CartDetails))
