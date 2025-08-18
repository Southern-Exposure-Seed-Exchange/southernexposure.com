module Pages.Cart.Type exposing (..)

import Data.Api as Api exposing (FormErrors)
import Data.PageData as PageData exposing (CartItemId(..))
import Data.Product as Product exposing (ProductVariantId)
import Dict exposing (Dict)
import Html exposing (..)
import RemoteData exposing (WebData)


type alias CartForms =
    Dict
        Int
        { variant : Maybe ProductVariantId
        , quantity : Int
        , requestStatus : WebData (Result Api.FormErrors ())
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
    | UpdateResponse (RemoteData.WebData (Result FormErrors PageData.CartDetails))
