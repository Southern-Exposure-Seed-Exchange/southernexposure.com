module Product exposing
    ( Product
    , ProductId(..)
    , ProductVariant
    , ProductVariantId(..)
    , decoder
    , isLimitedAvailablity
    , isOutOfStock
    , variantDecoder
    , variantPrice
    )

import Json.Decode as Decode exposing (Decoder)
import Models.Fields exposing (Cents(..), Milligrams(..))


type ProductId
    = ProductId Int


type alias Product =
    { id : ProductId
    , name : String
    , slug : String
    , baseSKU : String
    , shortDescription : String
    , longDescription : String
    , imageURL : String
    }


decoder : Decoder Product
decoder =
    Decode.map7 Product
        (Decode.field "id" <| Decode.map ProductId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "baseSku" Decode.string)
        (Decode.field "shortDescription" Decode.string)
        (Decode.field "longDescription" Decode.string)
        (Decode.field "imageUrl" Decode.string)


type ProductVariantId
    = ProductVariantId Int


{-| TODO: isActive is unused, quantity only used for in-stock status
-}
type alias ProductVariant =
    { id : ProductVariantId
    , product : ProductId
    , skuSuffix : String
    , price : Cents
    , salePrice : Maybe Cents
    , quantity : Int
    , weight : Milligrams
    , isActive : Bool
    }


variantPrice : ProductVariant -> Cents
variantPrice { price, salePrice } =
    Maybe.withDefault price salePrice


variantDecoder : Decoder ProductVariant
variantDecoder =
    Decode.map8 ProductVariant
        (Decode.field "id" <| Decode.map ProductVariantId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "skuSuffix" Decode.string)
        (Decode.field "price" <| Decode.map Cents Decode.int)
        (Decode.field "salePrice" <| Decode.nullable <| Decode.map Cents Decode.int)
        (Decode.field "quantity" Decode.int)
        (Decode.field "weight" <| Decode.map Milligrams Decode.int)
        (Decode.field "isActive" Decode.bool)


isOutOfStock : List ProductVariant -> Bool
isOutOfStock =
    List.all (\v -> v.quantity <= 0)


isLimitedAvailablity : List ProductVariant -> Bool
isLimitedAvailablity =
    List.any (\v -> v.quantity <= 0)
