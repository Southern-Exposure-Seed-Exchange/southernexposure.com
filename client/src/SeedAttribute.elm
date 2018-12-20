module SeedAttribute exposing
    ( Attribute(..)
    , SeedAttribute
    , SeedAttributeId(..)
    , decoder
    , iconUrl
    , icons
    , legend
    )

import Html exposing (Html, div, img, li, span, text, ul)
import Html.Attributes exposing (alt, class, src, title)
import Json.Decode as Decode exposing (Decoder)
import Product exposing (ProductId(..))
import Views.Images as Images


type SeedAttributeId
    = SeedAttributeId Int


type alias SeedAttribute =
    { id : SeedAttributeId
    , product : ProductId
    , isOrganic : Bool
    , isHeirloom : Bool
    , isEcological : Bool
    , isRegional : Bool
    }


decoder : Decoder SeedAttribute
decoder =
    Decode.map6 SeedAttribute
        (Decode.field "id" <| Decode.map SeedAttributeId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "isOrganic" Decode.bool)
        (Decode.field "isHeirloom" Decode.bool)
        (Decode.field "isEcological" Decode.bool)
        (Decode.field "isRegional" Decode.bool)


type Attribute
    = Organic
    | Heirloom
    | Regional
    | Ecological


all : List Attribute
all =
    [ Organic, Heirloom, Regional, Ecological ]


iconUrl : Attribute -> String
iconUrl attribute =
    let
        url =
            case attribute of
                Organic ->
                    "organic-certified.png"

                Heirloom ->
                    "heirloom.png"

                Regional ->
                    "southeast.png"

                Ecological ->
                    "ecologically-grown.png"
    in
    Images.static <| "icons/" ++ url


toString : Attribute -> String
toString attribute =
    case attribute of
        Organic ->
            "Certified Organic"

        Heirloom ->
            "Heirloom"

        Regional ->
            "Especially well-suited to the Southeast"

        Ecological ->
            "Ecologically Grown"


toDescription : Attribute -> String
toDescription attribute =
    case attribute of
        Organic ->
            "Certified Organic by Quality Certification Services"

        Heirloom ->
            "Heirlooms introduced before 1940"

        Regional ->
            "Varieties well-suited to the Mid-Atlantic & further South"

        Ecological ->
            "Grown by small ecological farmers"


legend : Html msg
legend =
    all
        |> List.map
            (\attribute ->
                li [ class "media mb-2 text-left" ]
                    [ img
                        [ class "mr-3"
                        , src <| iconUrl attribute
                        , title <| toDescription attribute
                        , alt <| toString attribute
                        ]
                        []
                    , div [ class "media-body" ] [ text <| toDescription attribute ]
                    ]
            )
        |> (\items ->
                div [ class "text-center" ]
                    [ ul [ class "list-unstyled d-inline-block mb-0" ] items ]
           )


icons : SeedAttribute -> Html msg
icons { isOrganic, isHeirloom, isRegional, isEcological } =
    [ ( isOrganic, Organic )
    , ( isHeirloom, Heirloom )
    , ( isRegional, Regional )
    , ( isEcological, Ecological )
    ]
        |> List.filter Tuple.first
        |> List.map
            (Tuple.second
                >> (\attribute ->
                        img
                            [ class "my-auto"
                            , title <| toDescription attribute
                            , alt <| toString attribute
                            , src <| iconUrl attribute
                            ]
                            []
                   )
            )
        |> span [ class "d-inline-block ml-2" ]
