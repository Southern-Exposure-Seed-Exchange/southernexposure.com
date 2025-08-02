module Components.Button exposing (..)

import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, href, name, type_)
import List exposing (length)
import Views.Utils exposing (icon)


type Style
    = Outline
    | Solid


type Padding
    = Default
    | Expand


type ButtonType
    = Link String -- a herf
    | FormSubmit -- button form submit


type alias Config =
    { label : String
    , style : Style
    , type_ : ButtonType
    , icon : String
    , padding : Padding
    }


defaultButton : Config
defaultButton =
    { label = ""
    , style = Solid
    , type_ = Link ""
    , icon = ""
    , padding = Default
    }


view : Config -> Html msg
view config =
    let
        styleClass =
            case config.style of
                Outline ->
                    "tw:border! tw:bg-white! tw:border-[#4DAA9A]! tw:text-[#1E0C03]! tw:hover:border-[#34C3AB]! tw:active:border-[#1D7F6E]!"

                Solid ->
                    "tw:bg-[#4DAA9A]! tw:text-white! tw:hover:bg-[#34C3AB]! tw:active:bg-[#1D7F6E]!"

        paddingClass =
            case config.padding of
                Default ->
                    "tw:py-[8px] tw:px-[16px]"

                Expand ->
                    "tw:py-[8px] tw:w-full"

        allClass =
            "tw:block tw:cursor-pointer tw:text-[16px] tw:leading-[24px] tw:rounded-[8px]! tw:no-underline! tw:flex tw:gap-[8px] tw:items-center tw:justify-center"
                ++ " "
                ++ styleClass
                ++ " "
                ++ paddingClass

        buttonContent =
            (if String.length config.icon == 0 then
                []

             else
                [ icon config.icon ]
            )
                ++ [ text config.label
                   ]
    in
    case config.type_ of
        Link hrefLink ->
            a
                [ href hrefLink
                , class allClass
                ]
                buttonContent

        FormSubmit ->
            button
                [ type_ "submit"
                , name "submit"
                , class allClass
                ]
                buttonContent
