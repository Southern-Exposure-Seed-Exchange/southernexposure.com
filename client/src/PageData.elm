module PageData
    exposing
        ( PageData
        , initial
        , CategoryDetails
        , categoryDetailsDecoder
        , ProductDetails
        , productDetailsDecoder
        , update
        )

import Json.Decode as Decode exposing (Decoder)
import Paginate exposing (PaginatedList)
import RemoteData exposing (WebData)
import Category exposing (Category)
import Product exposing (Product, ProductVariant)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(..))
import SeedAttribute exposing (SeedAttribute)


-- MODEL


type alias PageData =
    { categoryDetails : WebData CategoryDetails
    , productDetails : WebData ProductDetails
    }


initial : PageData
initial =
    { categoryDetails = RemoteData.NotAsked
    , productDetails = RemoteData.NotAsked
    }


type alias CategoryDetails =
    { category : Category
    , subCategories : List Category
    , products : PaginatedList ( Product, List ProductVariant, Maybe SeedAttribute )
    }


categoryDetailsDecoder : Decoder CategoryDetails
categoryDetailsDecoder =
    let
        productDataDecoder =
            Decode.map (Paginate.fromList (.perPage Pagination.default)) <|
                Decode.list <|
                    Decode.map3 (,,)
                        (Decode.field "product" Product.decoder)
                        (Decode.field "variants" <| Decode.list Product.variantDecoder)
                        (Decode.field "seedAttribute" <| Decode.nullable SeedAttribute.decoder)
    in
        Decode.map3 CategoryDetails
            (Decode.field "category" Category.decoder)
            (Decode.field "subCategories" <| Decode.list Category.decoder)
            (Decode.field "products" productDataDecoder)


type alias ProductDetails =
    { product : Product
    , variants : List ProductVariant
    , maybeSeedAttribute : Maybe SeedAttribute
    , categories : List Category
    }


productDetailsDecoder : Decoder ProductDetails
productDetailsDecoder =
    Decode.map4 ProductDetails
        (Decode.field "product" Product.decoder)
        (Decode.field "variants" <| Decode.list Product.variantDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable SeedAttribute.decoder)
        (Decode.field "categories" <| Decode.list Category.decoder)



-- UPDATE


update : Route -> PageData -> PageData
update route data =
    case route of
        ProductDetails _ ->
            data

        CategoryDetails _ pagination sortOption ->
            { data
                | categoryDetails =
                    Pagination.sortAndSetData pagination
                        (Sorting.apply sortOption)
                        (\d ps -> { d | products = ps })
                        .products
                        data.categoryDetails
            }
