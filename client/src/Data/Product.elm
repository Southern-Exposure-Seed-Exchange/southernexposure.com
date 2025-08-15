module Data.Product exposing
    ( Product
    , ProductId(..)
    , ProductVariant
    , ProductVariantId(..)
    , InventoryPolicy(..)
    , decoder
    , idDecoder
    , idEncoder
    , isLimitedAvailability
    , isPurchaseable
    , nameWithLotSize
    , productMainImage
    , singleVariantName
    , variantDecoder
    , variantPrice
    )

import Dict exposing (Dict)
import Html exposing (Html, span)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Markdown exposing (defaultOptions)
import Data.Fields exposing (Cents(..), ImageData, LotSize, blankImage, imageDecoder, lotSizeDecoder, lotSizeToString)
import Components.Microdata as Microdata


type ProductId
    = ProductId Int


idDecoder : Decoder ProductId
idDecoder =
    Decode.map ProductId Decode.int


idEncoder : ProductId -> Value
idEncoder (ProductId i) =
    Encode.int i


type alias Product =
    { id : ProductId
    , name : String
    , slug : String
    , baseSKU : String
    , longDescription : String
    , images : List ImageData
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
    span [ class "product-name-lotsize", Microdata.name ]
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


productMainImage : Product -> ImageData
productMainImage product = List.head product.images |> Maybe.withDefault blankImage

decoder : Decoder Product
decoder =
    Decode.map6 Product
        (Decode.field "id" idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "baseSku" Decode.string)
        (Decode.field "longDescription" Decode.string)
        (Decode.field "images" (Decode.list imageDecoder))


type ProductVariantId
    = ProductVariantId Int


type InventoryPolicy
    = RequireStock
    | AllowBackorder
    | Unlimited

inventoryPolicyDecoder : Decoder InventoryPolicy
inventoryPolicyDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "requireStock" ->
                        Decode.succeed RequireStock
                    "allowBackorder" ->
                        Decode.succeed AllowBackorder
                    "unlimited" ->
                        Decode.succeed Unlimited
                    _ ->
                        Decode.fail ("Unknown inventory policy: " ++ s)
            )

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
    , inventoryPolicy : InventoryPolicy
    , isActive : Bool
    }

isPurchaseable : ProductVariant -> Bool
isPurchaseable variant = not (variant.quantity <= 0 && variant.inventoryPolicy == RequireStock)

isLimitedAvailability : ProductVariant -> Bool
isLimitedAvailability variant = variant.inventoryPolicy == AllowBackorder && variant.quantity <= 0

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
        (Decode.field "inventoryPolicy" inventoryPolicyDecoder)
        |> Decode.andThen (\partialData ->
                Decode.map (\isActive -> partialData isActive )
                (Decode.field "isActive" Decode.bool)
        )
