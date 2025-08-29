module Pages.ProductList.Type exposing (..)

import Array
import Components.AddToCart.Type exposing (addToCartFromAmount, addToCartFromVariantId, initAddToCart)
import Components.ImageSlider.Type as ImageSlider
import Components.Pagination as Pagination
import Components.Product.Type as Product exposing (initProductModel)
import Components.Sorting as Sorting
import Data.Category exposing (CategoryId(..))
import Data.Fields exposing (Cents(..))
import Data.PageData as PageData exposing (CartDetails, CategoryDetails, ProductData, categoryConfig, getAmountBaseOnVariant, searchConfig)
import Data.Product as Product exposing (Product, ProductId(..), ProductVariantId(..), getFirstValidVariantIdFromDict)
import Data.Search as Search
import Dict exposing (Dict)
import Paginate exposing (Paginated)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { categoryDetails : Paginated ProductData { slug : String, sorting : Sorting.Option } CategoryDetails
    , searchResults : Paginated ProductData { data : Search.Data, sorting : Sorting.Option } String

    -- product dict state used in category detail and search result state
    , productDict : Dict Int Product.Model
    }


init : Model
init =
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
    , searchResults = searchPaginate
    , productDict = Dict.empty
    }



-------------------------------------------------------
-- Helper
-------------------------------------------------------


insertProductDetailToProductDict :
    WebData CartDetails
    -> PageData.ProductDetails
    -> Maybe ProductVariantId
    -> Dict Int Product.Model
    -> Dict Int Product.Model
insertProductDetailToProductDict cartDetailsRd productDetail selectedVariantId currentDict =
    let
        productId =
            productDetail.product.id |> (\(ProductId i) -> i)

        variantId =
            case selectedVariantId of
                Just vId ->
                    Just vId

                Nothing ->
                    getFirstValidVariantIdFromDict productDetail.variants
    in
    Dict.insert productId
        { variant = variantId
        , addToCart = addToCartFromVariantId cartDetailsRd variantId
        , imageSlider = ImageSlider.mkModel productDetail.product.name (Array.fromList productDetail.product.images)
        }
        currentDict


{-| Given a list of PageData.ProductData, populate productDict accordingly
-}
productDatasToProductDict : WebData CartDetails -> List ProductData -> Dict Int Product.Model
productDatasToProductDict cartDetailsRd productData =
    productData
        |> List.foldl
            (\p acc ->
                let
                    ( product, variantDict, _ ) =
                        p

                    pId =
                        (\(ProductId id) -> id) product.id

                    firstVariantId =
                        getFirstValidVariantIdFromDict variantDict

                    -- TODO:
                    -- - use the function on how to get the first variant of a product
                    -- - using that variant access the cart amount and assign to addToCart
                    -- - (when changing the variant, access the cart detail and assign the proper amount to the addToCart again, with this
                    -- we avoid having multiple addToCart state)
                    productModel =
                        { initProductModel
                            | variant = firstVariantId
                            , addToCart = addToCartFromVariantId cartDetailsRd firstVariantId
                        }
                in
                Dict.insert pId productModel acc
            )
            Dict.empty
