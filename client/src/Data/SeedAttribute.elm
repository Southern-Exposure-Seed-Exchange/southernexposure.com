module Data.SeedAttribute exposing
    ( Attribute(..)
    , SeedAttribute
    , SeedAttributeId(..)
    , all
    , decoder
    , toDescription
    , toString
    )

import Data.Product as Product exposing (ProductId(..))
import Json.Decode as Decode exposing (Decoder)



-- import Utils.Images as Images


type SeedAttributeId
    = SeedAttributeId Int


type alias SeedAttribute =
    { id : SeedAttributeId
    , product : ProductId
    , isOrganic : Bool
    , isHeirloom : Bool
    , isSmallGrower : Bool
    , isRegional : Bool
    }


decoder : Decoder SeedAttribute
decoder =
    Decode.map6 SeedAttribute
        (Decode.field "id" <| Decode.map SeedAttributeId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "isOrganic" Decode.bool)
        (Decode.field "isHeirloom" Decode.bool)
        (Decode.field "isSmallGrower" Decode.bool)
        (Decode.field "isRegional" Decode.bool)


type Attribute
    = Organic
    | Heirloom
    | Regional
    | SmallGrower


all : List Attribute
all =
    [ Organic, Heirloom, Regional, SmallGrower ]


toString : Attribute -> String
toString attribute =
    case attribute of
        Organic ->
            "Certified Organic"

        Heirloom ->
            "Heirloom"

        Regional ->
            "Especially well-suited to the Southeast"

        SmallGrower ->
            "From Small Farms"


toDescription : Attribute -> String
toDescription attribute =
    case attribute of
        Organic ->
            "Certified Organic by Quality Certification Services"

        Heirloom ->
            "Heirlooms introduced before 1940"

        Regional ->
            "Varieties well-suited to the Mid-Atlantic & further South"

        SmallGrower ->
            "Seed from small farms in our Seed Grower Network"
