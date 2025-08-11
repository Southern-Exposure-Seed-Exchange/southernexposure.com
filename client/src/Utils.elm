module Utils exposing (..)
import Routing exposing (Route)

type ClickType msg
    = TriggerHref Route
    | TriggerOnClick msg