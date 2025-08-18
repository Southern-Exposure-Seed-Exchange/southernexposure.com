module Components.Tooltip exposing (..)

import Browser.Dom as Dom
import Components.Svg exposing (..)
import Data.ViewKey exposing (ViewKey)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Task


type alias Config parentMsg =
    { key : ViewKey
    , triggerEl : Html parentMsg
    , text : String
    , mkParentMsg : Msg -> parentMsg
    , widthClass : Maybe String
    }


type alias TooltipModel =
    { status : Bool

    -- width and triggerWidth is needed to center the tooltip
    , width : Maybe Float
    , triggerWidth : Maybe Float
    }


defaultTooltip : TooltipModel
defaultTooltip =
    { status = False, width = Nothing, triggerWidth = Nothing }


type alias TooltipKey =
    String


type alias Model =
    Dict TooltipKey TooltipModel


type Msg
    = None
    | SetStatus TooltipKey Bool
    | SetWidth TooltipKey (Result Dom.Error Dom.Element)
    | SetTriggerWidth TooltipKey (Result Dom.Error Dom.Element)


init : Model
init =
    Dict.empty


mkTriggerKey : ViewKey -> ViewKey
mkTriggerKey key =
    key ++ "_trigger"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        SetStatus key status ->
            ( Dict.update key
                (\mbTooltip ->
                    case mbTooltip of
                        Just t ->
                            Just <| { t | status = status }

                        Nothing ->
                            Just <| { defaultTooltip | status = status }
                )
                model
            , if status then
                Cmd.batch
                    [ Dom.getElement key |> Task.attempt (SetWidth key)
                    , Dom.getElement (mkTriggerKey key) |> Task.attempt (SetTriggerWidth key)
                    ]

              else
                Cmd.none
            )

        SetWidth key domE ->
            case domE of
                Ok dom ->
                    ( Dict.update key
                        (\mbTooltip ->
                            case mbTooltip of
                                Just t ->
                                    Just <| { t | width = Just dom.element.width }

                                Nothing ->
                                    Just <| { defaultTooltip | width = Just dom.element.width }
                        )
                        model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        SetTriggerWidth key domE ->
            case domE of
                Ok dom ->
                    ( Dict.update key
                        (\mbTooltip ->
                            case mbTooltip of
                                Just t ->
                                    Just <| { t | triggerWidth = Just dom.element.width }

                                Nothing ->
                                    Just <| { defaultTooltip | triggerWidth = Just dom.element.width }
                        )
                        model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Config parentMsg -> Model -> Html parentMsg
view config model =
    let
        tooltip =
            Dict.get config.key model |> Maybe.withDefault defaultTooltip

        leftPxMb =
            case ( tooltip.width, tooltip.triggerWidth ) of
                ( Just width, Just triggerWidth ) ->
                    Just <| round <| negate (width / 2) + (triggerWidth / 2)

                _ ->
                    Nothing

        ( visibleClass, pointerEventClass, leftPx ) =
            case ( tooltip.status, leftPxMb ) of
                ( True, Just l ) ->
                    ( "tw:opacity-100", "", l )

                ( False, Just l ) ->
                    ( "tw:opacity-0 tw:pointer-events-none", "tw:pointer-events-none", l )

                -- when leftPxMb is Nothing, hide the tooltip regardless of the status
                _ ->
                    ( "tw:opacity-0 tw:pointer-events-none", "tw:pointer-events-none", 0 )

        widthClass =
            case config.widthClass of
                Just c ->
                    c

                Nothing ->
                    "tw:whitespace-nowrap"

        tooltipView =
            div
                [ id <| config.key
                , class <|
                    visibleClass
                        ++ " "
                        ++ widthClass
                        ++ " "
                        ++ "tw:transition-opacity tw:bg-black tw:text-white tw:py-[6px] tw:px-[8px] tw:rounded-[4px] tw:text-center tw:text-[14px] tw:leading-[20px]"
                ]
                [ text <| config.text
                ]
    in
    div []
        [ div
            [ class "tw:relative" ]
            [ div
                [ class <| pointerEventClass ++ " tw:z-50 tw:absolute tw:bottom-[8px]"
                , style "left" (String.fromInt leftPx ++ "px")
                ]
                [ tooltipView
                ]
            ]
        , div
            [ id <| mkTriggerKey config.key
            , onMouseEnter <| config.mkParentMsg <| SetStatus config.key True
            , onMouseLeave <| config.mkParentMsg <| SetStatus config.key False
            ]
            [ config.triggerEl
            ]
        ]
