module Pages.ProductList.Type exposing (..)

import Components.Pagination as Pagination
import Components.Product.Type as Product
import Components.Sorting as Sorting
import Data.Category exposing (CategoryId(..))
import Data.Fields exposing (Cents(..))
import Data.PageData exposing (CategoryDetails, ProductData, categoryConfig, searchConfig)
import Data.Product as Product exposing (ProductVariantId(..))
import Data.Search as Search
import Dict exposing (Dict)
import Paginate exposing (Paginated)


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
