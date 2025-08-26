module Data.Shared exposing (..)

import BootstrapGallery as Gallery
import Components.Product.Type as Product
import Components.Tooltip as Tooltip
import Data.Fields exposing (ImageData)
import Data.Product exposing (ProductId)
import Data.Routing.Routing exposing (Route)
import Data.User exposing (AuthStatus)



-- Shared Model meant to be used across all `Component`


type alias Shared pmsg =
    { -- shared data
      maybeSessionToken : Maybe String
    , currentUser : AuthStatus

    -- shared component msg
    , tooltips : Tooltip.Model
    , tooltipMsg : Tooltip.Msg -> pmsg
    , navigateToMsg : Route -> pmsg

    -- specific component msg
    , lightboxMsg : Gallery.Msg ImageData -> pmsg
    , productMsg : ProductId -> Product.Msg -> pmsg
    }
