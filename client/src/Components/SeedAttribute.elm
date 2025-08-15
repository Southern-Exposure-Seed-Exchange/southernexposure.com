module Components.SeedAttribute exposing (..)

-- import Messages exposing (Msg(..))

import Components.Svg exposing (heirLoomSvg, organicSvg, smallFarmSvg, sunSvg)
import Components.Tooltip as Tooltip
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (all)
import Messages exposing (Msg(..))
import SeedAttribute
import Views.Images as Images


iconUrl : SeedAttribute.Attribute -> String
iconUrl attribute =
    let
        url =
            case attribute of
                SeedAttribute.Organic ->
                    "organic-certified.png"

                SeedAttribute.Heirloom ->
                    "heirloom.png"

                SeedAttribute.Regional ->
                    "southeast.png"

                SeedAttribute.SmallGrower ->
                    "small-growers.png"
    in
    Images.static <| "icons/" ++ url


legend : ( Tooltip.Model, Tooltip.Msg -> msg ) -> Html msg
legend ( tooltips, fromTooltipMsg ) =
    SeedAttribute.all
        |> List.map
            (\attribute ->
                li [ class "media tw:flex tw:gap-[16px] tw:text-left" ]
                    [ attributeToSvg ( tooltips, fromTooltipMsg ) attribute
                    , div [ class "media-body" ] [ text <| SeedAttribute.toDescription attribute ]
                    ]
            )
        |> (\items ->
                div [ class "text-center mt-4 mb-2 tw:pt-[200px]" ]
                    [ h5 [ class "tw:pb-[16px]" ] [ text "Icon Legend" ]
                    , ul [ class "list-unstyled d-inline-block mb-0" ] items
                    ]
           )


attributeToSvg : ( Tooltip.Model, Tooltip.Msg -> msg ) -> SeedAttribute.Attribute -> Html msg
attributeToSvg ( tooltips, fromTooltipMsg ) attribute =
    case attribute of
        SeedAttribute.Organic ->
            div []
                [ organicSvg
                ]
            -- Tooltip.view "organic"
            --     fromTooltipMsg
            --     { triggerEl =
            --         div []
            --             [ organicSvg
            --             ]
            --     , text = "abc"
            --     }
            --     tooltips

        -- (Dict.get "organic" tooltipDict |> Maybe.withDefault Tooltip.init)
        SeedAttribute.Heirloom ->
            div []
                [ heirLoomSvg
                ]

        SeedAttribute.Regional ->
            div []
                [ sunSvg
                ]

        SeedAttribute.SmallGrower ->
            div []
                [ smallFarmSvg
                ]


icons : ( Tooltip.Model, Tooltip.Msg -> msg ) -> SeedAttribute.SeedAttribute -> Html msg
icons ( tooltips, fromTooltipMsg ) { isOrganic, isHeirloom, isRegional, isSmallGrower } =
    [ ( isOrganic, SeedAttribute.Organic )
    , ( isHeirloom, SeedAttribute.Heirloom )
    , ( isRegional, SeedAttribute.Regional )
    , ( isSmallGrower, SeedAttribute.SmallGrower )
    ]
        |> List.filter Tuple.first
        |> List.map
            (Tuple.second
                >> (\attribute ->
                        attributeToSvg ( tooltips, fromTooltipMsg ) attribute
                   )
            )
        |> div [ class "tw:flex tw:gap-[10px]" ]


icons2 : SeedAttribute.SeedAttribute -> Html msg
icons2 { isOrganic, isHeirloom, isRegional, isSmallGrower } =
    [ ( isOrganic, SeedAttribute.Organic )
    , ( isHeirloom, SeedAttribute.Heirloom )
    , ( isRegional, SeedAttribute.Regional )
    , ( isSmallGrower, SeedAttribute.SmallGrower )
    ]
        |> List.filter Tuple.first
        |> List.map
            (Tuple.second
                >> (\attribute ->
                        img
                            [ class "mt-1 mb-auto"
                            , title <| SeedAttribute.toDescription attribute
                            , alt <| SeedAttribute.toString attribute
                            , src <| iconUrl attribute
                            ]
                            []
                   )
            )
        |> span [ class "d-inline-flex" ]
