module Components.Tooltip exposing (..)

import Components.Svg exposing (..)
import Data.ViewKey exposing (ViewKey)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Events.Extra exposing (onClickPreventDefault)


type alias Config parentMsg =
    { key : ViewKey
    , triggerEl : Html parentMsg
    , text : String
    }


type alias TooltipModel =
    { status : Bool
    }


defaultTooltip : TooltipModel
defaultTooltip =
    { status = False }


type alias TooltipKey =
    String


type alias Model =
    Dict TooltipKey TooltipModel


type Msg
    = None
    | SetStatus TooltipKey Bool


init : Model
init =
    Dict.empty


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model

        SetStatus key status ->
            Dict.update key
                (\mbTooltip ->
                    case mbTooltip of
                        Just t ->
                            Just <| { t | status = status }

                        Nothing ->
                            Just <| { status = status }
                )
                model


view : Config parentMsg -> { model : Model, toParentMsg : Msg -> parentMsg } -> Html parentMsg
view config { model, toParentMsg } =
    div []
        [ div
            [ class "tw:cursor-pointer tw:select-none"
            , onMouseEnter <| toParentMsg <| SetStatus config.key True
            , onMouseLeave <| toParentMsg <| SetStatus config.key False
            ]
            [ config.triggerEl
            ]
        , div
            [ class "tw:relative" ]
            [ div
                [ class "tw:absolute tw:z-50 tw:right-0 tw:top-[16px]" ]
                [ if (Dict.get config.key model |> Maybe.withDefault defaultTooltip).status then
                    div [ class "tw:bg-black tw:text-white" ]
                        [ text config.text
                        ]

                  else
                    div [] []
                ]
            ]
        ]
