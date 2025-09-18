module Components.ProfileNavbar exposing (..)

import Components.Dropdown as Dropdown
import Components.Svg exposing (..)
import Data.Routing.Routing exposing (Route(..), reverse)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onClickPreventDefault)
import Utils.Html exposing (ClickType(..))


type alias Model =
    { status : Bool
    , dropdown : Dropdown.Model
    }


type Msg
    = None
    | DropdownMsg Dropdown.Msg
    | SetMsgToParent MsgToParent


type MsgToParent
    = LogOut


init : Model
init =
    { status = True
    , dropdown = Dropdown.init
    }


update : Msg -> Model -> ( Model, Maybe MsgToParent, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Nothing, Cmd.none )

        SetMsgToParent m ->
            ( model, Just m, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( dropdown, cmd ) =
                    Dropdown.update subMsg model.dropdown
            in
            ( { model | dropdown = dropdown }
            , Nothing
            , Cmd.map DropdownMsg cmd
            )


view : Model -> Html Msg
view model =
    let
        triggerView =
            div
                [ class "tw:flex tw:gap-[4px] tw:items-center" ]
                [ div [ class "tw:w-[40px] tw:h-[40px] tw:rounded-full tw:bg-[#4DAA9A] tw:flex tw:items-center tw:justify-center" ]
                    [ userSvg
                    ]
                , div [] [ chevronDownSvg ]
                ]

        dropdownItemView : ClickType msg -> Html msg -> String -> Html msg
        dropdownItemView clickType iconSvg label =
            let
                ( htmlTag, attr ) =
                    case clickType of
                        TriggerHref r ->
                            ( a, [ href <| reverse r ] )

                        TriggerOnClick onClickMsg ->
                            ( div, [ onClickPreventDefault onClickMsg ] )
            in
            htmlTag
                (attr ++ [ class "tw:py-[12px] tw:px-[20px] tw:whitespace-nowrap tw:group tw:cursor-pointer tw:select-none tw:flex tw:items-center tw:gap-[12px]" ])
            <|
                [ div [] [ iconSvg ]
                , span [ class "tw:group-hover:underline" ] [ text label ]
                ]
    in
    Dropdown.view
        DropdownMsg
        { triggerEl = div [] [ triggerView ]
        , dropdownEl =
            div [ class "tw:flex tw:flex-col tw:bg-white tw:rounded-[16px] tw:shrink-0 tw:shadow-lg" ]
                [ dropdownItemView (TriggerHref MyAccount) orderSvg "Orders"
                , dropdownItemView (TriggerHref EditLogin) passwordSvg "Edit login details"
                , dropdownItemView (TriggerHref EditAddress) deliverySvg "Edit addresses"
                , dropdownItemView (TriggerOnClick <| SetMsgToParent LogOut) signOutSvg "Log out"
                ]
        , closeOnClick = True
        }
        model.dropdown
