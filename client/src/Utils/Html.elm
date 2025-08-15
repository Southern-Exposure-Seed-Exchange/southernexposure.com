module Utils.Html exposing (..)
import Data.Routing.Routing as Routing exposing (Route)


type ClickType msg
    = TriggerHref Route
    | TriggerOnClick msg