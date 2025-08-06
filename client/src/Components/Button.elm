module Components.Button exposing (..)

import Html exposing (Html, a, button, text)
import Html.Attributes exposing (class, disabled, href, name, type_)


type Style
    = Outline
    | Solid


type Padding
    = Default
    | Expand


type ButtonType
    = Link String -- a herf
    | FormSubmit -- button form submit
    | Disabled


type alias Config msg =
    { label : String
    , style : Style
    , type_ : ButtonType
    , icon : Maybe (Html msg)
    , padding : Padding
    }


defaultButton : Config msg
defaultButton =
    { label = ""
    , style = Solid
    , type_ = Link ""
    , icon = Nothing
    , padding = Default
    }


view : Config msg -> Html msg
view config =
    let
        cursorClass =
            case config.type_ of
                Disabled ->
                    "tw:cursor-not-allowed"

                _ ->
                    "tw:cursor-pointer"

        styleClass =
            case config.style of
                Outline ->
                    "tw:border! tw:bg-white! tw:border-[#4DAA9A]! tw:text-[#1E0C03]! tw:hover:border-[#34C3AB]! tw:active:border-[#1D7F6E]!"

                Solid ->
                    case config.type_ of
                        Disabled ->
                            "tw:bg-[#BFBFBF]! tw:text-white! "

                        _ ->
                            "tw:bg-[#4DAA9A]! tw:text-white! tw:hover:bg-[#34C3AB]! tw:active:bg-[#1D7F6E]!"

        paddingClass =
            case config.padding of
                Default ->
                    "tw:py-[8px] tw:px-[16px]"

                Expand ->
                    "tw:py-[8px] tw:w-full"

        allClass =
            "tw:block tw:text-[16px] tw:leading-[24px] tw:rounded-[8px]! tw:no-underline! tw:flex tw:gap-[8px] tw:items-center tw:justify-center"
                ++ " "
                ++ styleClass
                ++ " "
                ++ paddingClass
                ++ " "
                ++ cursorClass

        iconContent =
            case config.icon of
                Just iconView ->
                    [ iconView ]

                Nothing ->
                    []

        buttonContent =
            iconContent
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

        Disabled ->
            button
                [ type_ "button"
                , disabled True
                , class allClass
                ]
                buttonContent
