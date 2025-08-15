module Components.SeedAttribute exposing (..)

-- import Data.Msg exposing (Msg(..))

import Components.Svg exposing (heirLoomSvg, organicSvg, smallFarmSvg, sunSvg)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Utils.Images as Images
import Data.SeedAttribute as SeedAttribute
import List exposing (all)


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


legend : Html msg
legend =
    SeedAttribute.all
        |> List.map
            (\attribute ->
                li [ class "media tw:flex tw:gap-[16px] tw:text-left" ]
                    [ attributeToSvg attribute
                    , div [ class "media-body" ] [ text <| SeedAttribute.toDescription attribute ]
                    ]
            )
        |> (\items ->
                div [ class "text-center mt-4 mb-2 tw:pt-[200px]" ]
                    [ h5 [ class "tw:pb-[16px]" ] [ text "Icon Legend" ]
                    , ul [ class "list-unstyled d-inline-block mb-0" ] items
                    ]
           )


attributeToSvg : SeedAttribute.Attribute -> Html msg
attributeToSvg attribute =
    case attribute of
        SeedAttribute.Organic ->
            div []
                [ organicSvg
                ]

        -- Tooltip.view
        --     (TooltipMsg "organic")
        --     { triggerEl =
        --         div []
        --             [ organicSvg
        --             ]
        --     , text = "abc"
        --     }
        --     (Dict.get "organic" tooltipDict |> Maybe.withDefault Tooltip.init)
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


icons : SeedAttribute.SeedAttribute -> Html msg
icons { isOrganic, isHeirloom, isRegional, isSmallGrower } =
    [ ( isOrganic, SeedAttribute.Organic )
    , ( isHeirloom, SeedAttribute.Heirloom )
    , ( isRegional, SeedAttribute.Regional )
    , ( isSmallGrower, SeedAttribute.SmallGrower )
    ]
        |> List.filter Tuple.first
        |> List.map
            (Tuple.second
                >> (\attribute ->
                        attributeToSvg attribute
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
