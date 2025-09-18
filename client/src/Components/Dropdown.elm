module Components.Dropdown exposing (..)

import Components.Svg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickPreventDefault)


type alias Config parentMsg =
    { triggerEl : Html parentMsg
    , dropdownEl : Html parentMsg
    , closeOnClick : Bool
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        SetStatus status ->
            ( { model | status = status }, Cmd.none )



--   const dropdownBackgroundView = () => {
--     return (
--       <div
--         className='absolute top-0 left-0 right-0 bottom-0 bg-transparent z-[20]'
--         onClick={(e) => {
--           e.stopPropagation()
--           setStatus(false)
--         }}
--       ></div>
--     )
--   }


backgroundView closeMsg status =
    if status == True then
        div
            [ class "tw:fixed tw:top-0 tw:left-0 tw:right-0 tw:bottom-0 tw:bg-transparent tw:z-[20]"
            , onClickPreventDefault closeMsg
            ]
            []

    else
        div [] []


view : (Msg -> parentMsg) -> Config parentMsg -> Model -> Html parentMsg
view mkParentMsg config model =
    let
        closeMsg =
            mkParentMsg <| SetStatus False
    in
    div []
        [ backgroundView closeMsg model.status
        , div [ class "tw:cursor-pointer tw:select-none", onClick <| mkParentMsg <| SetStatus (not model.status) ]
            [ config.triggerEl
            ]
        , div
            [ class "tw:relative" ]
            [ div
                ([ class "tw:absolute tw:z-50 tw:right-0 tw:top-[16px]" ]
                    ++ (if config.closeOnClick then
                            [ onClick closeMsg ]

                        else
                            []
                       )
                )
                [ if model.status == True then
                    config.dropdownEl

                  else
                    div [] []
                ]
            ]
        ]
