module Components.ImageSlider.ImageSlider exposing (..)

import Array
import Browser.Dom as Dom
import Components.ImageSlider.Type exposing (..)
import Components.Microdata as Microdata
import Components.Svg exposing (chevronLeftSvg, chevronRightSvg)
import Data.Fields exposing (ImageData, blankImage, imageToSrcSet, imgSrcFallback)
import Data.Shared exposing (Shared)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import List.Extra as List
import Ports exposing (scrollLeftSmooth)
import Task



----------------------------------------------------------
-- Constants
----------------------------------------------------------


containerId =
    "image-slider-container"


itemId i =
    "image-slider-item-" ++ String.fromInt i



----------------------------------------------------------
-- Update
----------------------------------------------------------


update : Shared pmsg -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        NextImage ->
            let
                i =
                    model.currentImage

                ( newCurrentImage, cmd ) =
                    if i + 1 >= Array.length model.images then
                        let
                            newI =
                                0
                        in
                        ( newI
                        , getContainerDomCmd newI
                        )

                    else
                        let
                            newI =
                                i + 1
                        in
                        ( newI
                        , getContainerDomCmd newI
                        )
            in
            ( { model
                | currentImage = newCurrentImage
              }
            , cmd
            )

        PrevImage ->
            let
                i =
                    model.currentImage

                ( newCurrentImage, cmd ) =
                    if i - 1 < 0 then
                        let
                            newI =
                                Array.length model.images - 1
                        in
                        ( newI
                        , getContainerDomCmd newI
                        )

                    else
                        let
                            newI =
                                i - 1
                        in
                        ( newI
                        , getContainerDomCmd newI
                        )
            in
            ( { model
                | currentImage = newCurrentImage
              }
            , cmd
            )

        SetCurrentImage i ->
            ( { model | currentImage = i }, getContainerDomCmd i )

        GetContainerDomResponse i result ->
            case result of
                Ok dom ->
                    ( model, getItemDomCmd i dom )

                _ ->
                    ( model, Cmd.none )

        GetItemDomResponse containerDom result ->
            case result of
                Ok dom ->
                    ( model
                    , case checkHorizontalVisibility dom containerDom of
                        FullyVisible ->
                            Cmd.none

                        LeftCutOff px ->
                            scrollLeftSmooth { id = containerId, amount = negate px }

                        RightCutOff px ->
                            scrollLeftSmooth { id = containerId, amount = px }
                    )

                _ ->
                    ( model, Cmd.none )



----------------------------------------------------------
-- Cmd
----------------------------------------------------------


{-| The start of many actions

1.  get container dom
2.  get item dom
3.  check if item dom is fully visible, if not trigger a scroll to it

-}
getContainerDomCmd : Int -> Cmd Msg
getContainerDomCmd i =
    Dom.getElement containerId
        |> Task.attempt
            (GetContainerDomResponse i)


getItemDomCmd : Int -> Dom.Element -> Cmd Msg
getItemDomCmd i containerDom =
    Dom.getElement (itemId i)
        |> Task.attempt
            (GetItemDomResponse containerDom)



----------------------------------------------------------
-- Util
----------------------------------------------------------


type HorizontalVisibility
    = FullyVisible
    | LeftCutOff Float
    | RightCutOff Float


checkHorizontalVisibility : Dom.Element -> Dom.Element -> HorizontalVisibility
checkHorizontalVisibility el container =
    let
        -- padding of the image such that the cuff of calculate is base on the image size + padding
        padding =
            40

        elLeft =
            el.element.x - padding

        elRight =
            el.element.x + el.element.width + padding

        containerLeft =
            container.element.x

        containerRight =
            container.element.x + container.element.width

        cutoffLeft =
            if elLeft < containerLeft then
                Just (containerLeft - elLeft)

            else
                Nothing

        cutoffRight =
            if elRight > containerRight then
                Just (elRight - containerRight)

            else
                Nothing
    in
    case ( cutoffLeft, cutoffRight ) of
        ( Nothing, Nothing ) ->
            FullyVisible

        ( Just px, _ ) ->
            LeftCutOff px

        ( _, Just px ) ->
            RightCutOff px



----------------------------------------------------------
-- View
----------------------------------------------------------


view : (ImageData -> Attribute pmsg) -> (Msg -> pmsg) -> Model -> Html pmsg
view onClickOpenGallery mkParentMsg model =
    let
        imageListView =
            List.indexedMap
                (\i m ->
                    let
                        ( containerClass, imgClass, msg ) =
                            if i == model.currentImage then
                                ( "tw:border-[2px] tw:border-[#1E0C03]"
                                , "tw:transition-all tw:aspect-square tw:object-cover tw:w-[76px] tw:h-[76px] tw:rounded-[4px]"
                                , None
                                )

                            else
                                ( ""
                                , "tw:transition-all tw:w-[90px] tw:h-[90px] tw:aspect-square tw:object-cover tw:rounded-[8px] clickable-image"
                                , SetCurrentImage i
                                )
                    in
                    div
                        [ class <| "tw:w-[90px] tw:h-[90px] tw:rounded-[8px] tw:shrink-0 tw:flex tw:items-center tw:justify-center " ++ containerClass
                        , id <| itemId i
                        ]
                        [ img
                            [ src <| imgSrcFallback m
                            , imageToSrcSet m
                            , class imgClass
                            , onClick <| mkParentMsg msg
                            ]
                            []
                        ]
                )
                (Array.toList model.images)

        dotItemViews =
            List.indexedMap
                (\i m ->
                    let
                        ( containerClass, dotClass, msg ) =
                            if i == model.currentImage then
                                ( ""
                                , "tw:opacity-100"
                                , None
                                )

                            else
                                ( "tw:cursor-pointer"
                                , "tw:opacity-80 tw:group-hover:opacity-100"
                                , SetCurrentImage i
                                )
                    in
                    div [ class <| containerClass ++ " tw:p-[4px] tw:group", onClick <| mkParentMsg msg ]
                        [ div [ class <| dotClass ++ " tw:bg-white  tw:w-[8px] tw:h-[8px] tw:rounded-full tw:shadow-md" ]
                            []
                        ]
                )
                (Array.toList model.images)

        dotListView =
            div [ class "tw:relative" ]
                [ div [ class "tw:absolute tw:bottom-[16px] tw:w-full" ]
                    [ div [ class "tw:flex tw:items-center tw:justify-center" ] <| dotItemViews
                    ]
                ]

        backButton =
            div [ class "tw:relative" ]
                [ div [ class "tw:left-[10px] tw:lg:left-[-20px] tw:top-[-20px] tw:absolute tw:z-4" ]
                    [ button
                        [ type_ "button"
                        , onClick <|
                            mkParentMsg PrevImage
                        , class
                            "tw:transition-all tw:opacity-0 tw:group-hover:opacity-100 tw:w-[40px] tw:h-[40px] tw:bg-white tw:rounded-full! tw:overflow-hidden tw:hover:shadow-lg tw:shadow-md tw:flex tw:items-center tw:justify-center"
                        ]
                        [ chevronLeftSvg
                        ]
                    ]
                ]

        nextButton =
            div [ class "tw:relative" ]
                [ div [ class "tw:right-[10px] tw:lg:right-[-20px] tw:top-[-20px] tw:absolute tw:z-4" ]
                    [ button
                        [ type_ "button"
                        , onClick <| mkParentMsg NextImage
                        , class "tw:transition-all tw:opacity-0 tw:group-hover:opacity-100 tw:w-[40px] tw:h-[40px] tw:bg-white tw:rounded-full! tw:overflow-hidden tw:hover:shadow-lg tw:shadow-md tw:flex tw:items-center tw:justify-center"
                        ]
                        [ chevronRightSvg
                        ]
                    ]
                ]

        currentImage =
            Array.get model.currentImage model.images
                |> Maybe.withDefault blankImage
    in
    div [ class "tw:flex tw:flex-col tw:gap-[20px] tw:w-full tw:lg:w-[360px] tw:shrink-0" ]
        [ div []
            [ div [ class "tw:flex tw:items-center tw:group" ]
                [ viewIf (Array.length model.images > 1) backButton
                , img
                    [ src <| imgSrcFallback currentImage
                    , imageToSrcSet currentImage
                    , Microdata.image
                    , alt <| ("Product Image for " ++ model.name)
                    , class "tw:w-full tw:lg:w-[360px] tw:aspect-square tw:object-cover tw:rounded-[16px] clickable-image"
                    , onClickOpenGallery currentImage
                    ]
                    []
                , viewIf (Array.length model.images > 1) nextButton
                ]
            , viewIf (Array.length model.images > 1)
                dotListView
            ]
        , div [ id containerId, class "tw:flex tw:overflow-auto tw:gap-[10px] custom-scrollbar tw:pb-[6px]" ] <|
            imageListView
        ]
