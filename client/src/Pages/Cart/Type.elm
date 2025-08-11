module Pages.Cart.Type exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import RemoteData
import PageData exposing (CartItemId(..))
import Product exposing (ProductVariantId)
import RemoteData exposing (WebData)


type alias CartForms =
    Dict Int
        { variant : Maybe ProductVariantId
        , quantity : Int
        , requestStatus : WebData ()
        }

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
    | UpdateResponse (RemoteData.WebData PageData.CartDetails)

