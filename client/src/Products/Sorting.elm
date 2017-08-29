module Products.Sorting
    exposing
        ( Option
        , all
        , default
        , apply
        , toQueryString
        , fromQueryString
        , toQueryValue
        , fromQueryValue
        , toDescription
        )

import UrlParser as Url exposing ((<?>))
import Models.Fields exposing (Cents(..))
import Product exposing (Product, ProductVariant, SeedAttribute)


-- MODEL


type Option
    = ProductNameAsc
    | ProductNameDesc
    | PriceAsc
    | PriceDesc
    | ItemNumberAsc


all : List Option
all =
    [ ProductNameAsc, ProductNameDesc, PriceAsc, PriceDesc, ItemNumberAsc ]


default : Option
default =
    ProductNameAsc



-- APPLICATION


apply :
    Option
    -> List ( Product, List ProductVariant, Maybe SeedAttribute )
    -> List ( Product, List ProductVariant, Maybe SeedAttribute )
apply option =
    let
        getPriceFromVariants priceCollector default list =
            priceCollector (List.map (.price >> (\(Cents c) -> c)) list)
                |> Maybe.withDefault default

        compareVariantPrices vs1 vs2 =
            compare (getPriceFromVariants List.minimum 0 vs1)
                (getPriceFromVariants List.minimum 0 vs2)
    in
        List.sortWith
            (\( p1, vs1, _ ) ( p2, vs2, _ ) ->
                case option of
                    ProductNameAsc ->
                        compare (p1.name) (p2.name)

                    ProductNameDesc ->
                        compare (p2.name) (p1.name)

                    PriceAsc ->
                        compareVariantPrices vs1 vs2

                    PriceDesc ->
                        compareVariantPrices vs2 vs1

                    ItemNumberAsc ->
                        compare (p1.baseSKU) (p2.baseSKU)
            )



-- CONVERSIONS


toQueryString : Option -> String
toQueryString data =
    let
        value =
            toQueryValue data

        defaultValue =
            toQueryValue default
    in
        if value == defaultValue then
            ""
        else
            "sortBy=" ++ value


fromQueryString :
    Url.Parser ((a -> d) -> a -> d) (Option -> b)
    -> Url.Parser (b -> c) c
fromQueryString pathParser =
    Url.map (<|)
        (pathParser
            <?> Url.customParam "sortBy" (Maybe.withDefault "" >> fromQueryValue)
        )


toQueryValue : Option -> String
toQueryValue data =
    case data of
        ProductNameAsc ->
            "name-asc"

        ProductNameDesc ->
            "name-desc"

        PriceAsc ->
            "price-asc"

        PriceDesc ->
            "price-desc"

        ItemNumberAsc ->
            "number-asc"


fromQueryValue : String -> Option
fromQueryValue data =
    case data of
        "name-asc" ->
            ProductNameAsc

        "name-desc" ->
            ProductNameDesc

        "price-asc" ->
            PriceAsc

        "price-desc" ->
            PriceDesc

        "number-asc" ->
            ItemNumberAsc

        _ ->
            default


toDescription : Option -> String
toDescription data =
    case data of
        ProductNameAsc ->
            "Product Name - A to Z"

        ProductNameDesc ->
            "Product Name - Z to A"

        PriceAsc ->
            "Lowest Price"

        PriceDesc ->
            "Highest Price"

        ItemNumberAsc ->
            "Item Number"
