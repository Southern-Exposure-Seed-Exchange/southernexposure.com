module Product exposing
    ( Product
    , ProductId(..)
    , ProductVariant
    , ProductVariantId(..)
    , decoder
    , isLimitedAvailablity
    , isOutOfStock
    , nameWithLotSize
    , singleVariantName
    , variantDecoder
    , variantPrice
    )

import Dict exposing (Dict)
import Html exposing (Html, span)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown exposing (defaultOptions)
import Models.Fields exposing (Cents(..), ImageData, LotSize, imageDecoder, lotSizeDecoder, lotSizeToString)


type ProductId
    = ProductId Int


type alias Product =
    { id : ProductId
    , name : String
    , slug : String
    , baseSKU : String
    , longDescription : String
    , image : ImageData
    }


{-| Build a nicer name for a Product & Variant by including it's LotSize if
available. This renders the Product name as markdown to allow for HTML entities
& markup in titles.
-}
nameWithLotSize : { p | name : String } -> { v | lotSize : Maybe LotSize } -> Html msg
nameWithLotSize { name } { lotSize } =
    let
        lotSizeString =
            lotSize |> Maybe.map (\s -> ", " ++ lotSizeToString s) |> Maybe.withDefault ""
    in
    span [ class "product-name-lotsize" ]
        [ Markdown.toHtmlWith
            { defaultOptions | sanitize = False, smartypants = True }
            []
            (name ++ lotSizeString)
        ]


{-| Build the name for a Product, appending the LotSize if only one Variant is
present.
-}
singleVariantName : Product -> Dict Int ProductVariant -> Html msg
singleVariantName product variants =
    nameWithLotSize product <|
        case Dict.toList variants of
            [ ( _, variant ) ] ->
                { lotSize = variant.lotSize }

            _ ->
                { lotSize = Nothing }


decoder : Decoder Product
decoder =
    Decode.map6 Product
        (Decode.field "id" <| Decode.map ProductId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "baseSku" Decode.string)
        (Decode.field "longDescription" Decode.string)
        (Decode.field "image" imageDecoder)


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
    , lotSize : Maybe LotSize
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
        (Decode.field "lotSize" <| Decode.nullable lotSizeDecoder)
        (Decode.field "isActive" Decode.bool)


isOutOfStock : List ProductVariant -> Bool
isOutOfStock =
    List.all (\v -> v.quantity <= 0)


isLimitedAvailablity : List ProductVariant -> Bool
isLimitedAvailablity =
    List.any (\v -> v.quantity <= 0)
