module Data.Shared exposing (..)

import BootstrapGallery as Gallery
import Components.Product.Type as Product
import Components.Tooltip as Tooltip
import Data.Fields exposing (ImageData)
import Data.Msg exposing (Msg(..))
import Data.Product exposing (ProductId)
import Data.Routing.Routing exposing (Route)



-- Shared Model meant to be used across all `Component`


type alias Shared pmsg =
    { tooltips : Tooltip.Model
    , tooltipMsg : Tooltip.Msg -> pmsg
    , navigateToMsg : Route -> pmsg

    -- component specific
    , lightboxMsg : Gallery.Msg ImageData -> pmsg
    , productMsg : ProductId -> Product.Msg -> pmsg
    }
