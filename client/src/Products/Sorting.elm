module Products.Sorting
    exposing
        ( Option
        , all
        , default
        , toQueryString
        , fromQueryString
        , toQueryValue
        , fromQueryValue
        , toDescription
        )

import UrlParser as Url exposing ((<?>))
import Models.Fields exposing (Cents(..))
import Product exposing (Product, ProductVariant)
import SeedAttribute exposing (SeedAttribute)


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
