module Components.IconButton exposing (..)

import Html exposing (a, div, img)
import Html.Attributes exposing (alt, class, href, src)


view svgView =
    let
        svgFillClass =
            "tw:transition-all tw:fill-[#1E0C03]! tw:group-hover:fill-[#34C3AB]! tw:group-active:fill-[#1D7F6E]!"
    in
    svgView svgFillClass
