module Components.Tooltip exposing (..)

import Components.Svg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Events.Extra exposing (onClickPreventDefault)


type alias Config parentMsg =
    { triggerEl : Html parentMsg
    , tooltipContent : String
    }


type alias Model =
    { status : Bool
    }


type Msg
    = None
    | SetStatus Bool


init : Model
init =
    { status = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model

        SetStatus status ->
            { model | status = status }


view : (Msg -> parentMsg) -> Config parentMsg -> Model -> Html parentMsg
view mkParentMsg config model =
    div []
        [ div
            [ class "tw:cursor-pointer tw:select-none"
            , onMouseEnter <| mkParentMsg <| SetStatus True
            , onMouseLeave <| mkParentMsg <| SetStatus False
            ]
            [ config.triggerEl
            ]
        , div
            [ class "tw:relative" ]
            [ div
                [ class "tw:absolute tw:z-50 tw:right-0 tw:top-[16px]" ]
                [ if model.status == True then
                    div [ class "tw:bg-black tw:text-white" ]
                        [ text config.tooltipContent
                        ]

                  else
                    div [] []
                ]
            ]
        ]
