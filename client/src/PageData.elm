module PageData
    exposing
        ( PageData
        , initial
        , CategoryDetails
        , categoryDetailsDecoder
        , categoryConfig
        , ProductDetails
        , productDetailsDecoder
        , SearchResults
        , searchConfig
        , ProductData
        , productDataDecoder
        , AdvancedSearch
        , advancedSearchDecoder
        )

import Http
import Json.Decode as Decode exposing (Decoder)
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Category exposing (Category, CategoryId(..))
import Product exposing (Product, ProductVariant)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing.Utils exposing (joinPath, withQueryStrings)
import Search
import SeedAttribute exposing (SeedAttribute)


-- MODEL


type alias PageData =
    { categoryDetails : Paginated ProductData { slug : String, sorting : Sorting.Option } CategoryDetails
    , productDetails : WebData ProductDetails
    , advancedSearch : WebData AdvancedSearch
    , searchResults : Paginated ProductData { data : Search.Data, sorting : Sorting.Option } String
    }


initial : PageData
initial =
    let
        categoryPaginate =
            Paginate.initial categoryConfig
                { slug = "", sorting = Sorting.default }
                (.page Pagination.default)
                (.perPage Pagination.default)
                |> Tuple.first

        searchPaginate =
            Paginate.initial searchConfig
                { data = Search.initial, sorting = Sorting.default }
                (.page Pagination.default)
                (.perPage Pagination.default)
                |> Tuple.first
    in
        { categoryDetails = categoryPaginate
        , productDetails = RemoteData.NotAsked
        , advancedSearch = RemoteData.NotAsked
        , searchResults = searchPaginate
        }


type alias CategoryDetails =
    { category : Category
    , subCategories : List Category
    }


categoryDetailsDecoder : Decoder CategoryDetails
categoryDetailsDecoder =
    Decode.map2 CategoryDetails
        (Decode.field "category" Category.decoder)
        (Decode.field "subCategories" <| Decode.list Category.decoder)


categoryConfig : Paginate.Config ProductData { slug : String, sorting : Sorting.Option } CategoryDetails
categoryConfig =
    let
        request { slug, sorting } page perPage =
            Http.get
                (joinPath [ "api/categories/details/", slug ]
                    ++ withQueryStrings
                        [ Pagination.toQueryString (Pagination.Data page perPage sorting) ]
                )
            <|
                Decode.map3 Paginate.FetchResponse
                    (Decode.field "products" <| Decode.list productDataDecoder)
                    (Decode.field "total" Decode.int)
                    (Decode.map Just categoryDetailsDecoder)
    in
        Paginate.makeConfig request


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


type alias SearchResults =
    Paginated ProductData { data : Search.Data, sorting : Sorting.Option } String


searchConfig : Paginate.Config ProductData { data : Search.Data, sorting : Sorting.Option } String
searchConfig =
    let
        request { data, sorting } page perPage =
            Http.post
                ("/api/products/search/"
                    ++ withQueryStrings [ Pagination.toQueryString (Pagination.Data page perPage sorting) ]
                )
                (Search.encode data |> Http.jsonBody)
                (Decode.map3 Paginate.FetchResponse
                    (Decode.field "products" <| Decode.list productDataDecoder)
                    (Decode.field "total" Decode.int)
                    (Decode.field "categoryName" <| Decode.nullable Decode.string)
                )
    in
        Paginate.makeConfig request


type alias ProductData =
    ( Product, List ProductVariant, Maybe SeedAttribute )


productDataDecoder : Decoder ProductData
productDataDecoder =
    Decode.map3 (,,)
        (Decode.field "product" Product.decoder)
        (Decode.field "variants" <| Decode.list Product.variantDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable SeedAttribute.decoder)


type alias AdvancedSearch =
    List AdvancedSearchCategory


type alias AdvancedSearchCategory =
    { id : CategoryId
    , name : String
    }


advancedSearchDecoder : Decoder AdvancedSearch
advancedSearchDecoder =
    Decode.field "categories" <|
        Decode.list <|
            Decode.map2 AdvancedSearchCategory
                (Decode.field "id" <| Decode.map CategoryId Decode.int)
                (Decode.field "name" Decode.string)
