module Components.ImageSlider.Type exposing (..)

import Array exposing (Array)
import Browser.Dom as Dom
import Data.Fields exposing (ImageData, blankImage, imageToSrcSet, imgSrcFallback)
import Html exposing (..)
import Html.Attributes exposing (..)



-- TODO: pass list of image to this component


type alias Model =
    { name : String
    , images : Array ImageData
    , currentImage : Int
    }


mkModel : String -> Array ImageData -> Model
mkModel name images =
    { name = name
    , images = images
    , currentImage = 0
    }


type Msg
    = None
    | NextImage
    | PrevImage
    | SetCurrentImage Int
    | GetContainerDomResponse Int (Result Dom.Error Dom.Element)
    | GetItemDomResponse Dom.Element (Result Dom.Error Dom.Element)
