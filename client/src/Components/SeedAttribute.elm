module Components.SeedAttribute exposing (..)

import Components.Svg exposing (heirLoomSvg, organicSvg, smallFarmSvg, sunSvg)
import Components.Tooltip as Tooltip
import Data.SeedAttribute as SeedAttribute
import Data.Shared exposing (Shared)
import Data.ViewKey exposing (ViewKey)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Utils.Images as Images


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
            organicSvg

        SeedAttribute.Heirloom ->
            heirLoomSvg

        SeedAttribute.Regional ->
            sunSvg

        SeedAttribute.SmallGrower ->
            smallFarmSvg


icons : Shared -> ViewKey -> (Tooltip.Msg -> msg) -> SeedAttribute.SeedAttribute -> Html msg
icons shared parentKey mkParentMsg { isOrganic, isHeirloom, isRegional, isSmallGrower } =
    [ ( isOrganic, SeedAttribute.Organic )
    , ( isHeirloom, SeedAttribute.Heirloom )
    , ( isRegional, SeedAttribute.Regional )
    , ( isSmallGrower, SeedAttribute.SmallGrower )
    ]
        |> List.filter Tuple.first
        |> List.map
            (Tuple.second
                >> (\attribute ->
                        Tooltip.view
                            { key = parentKey ++ SeedAttribute.toString attribute
                            , triggerEl = attributeToSvg attribute
                            , text = SeedAttribute.toDescription attribute
                            , mkParentMsg = mkParentMsg
                            , widthClass = Just "tw:w-[200px]"
                            , align = Tooltip.Right
                            }
                            shared.tooltips
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
