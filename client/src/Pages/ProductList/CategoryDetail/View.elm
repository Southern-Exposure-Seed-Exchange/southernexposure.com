module Pages.ProductList.CategoryDetail.View exposing (view)

import Components.Pagination as Pagination
import Components.Product.Type as Product
import Components.Product.Views as ProductViews
import Components.Sorting as Sorting
import Data.Category as Category
import Data.Fields exposing (imageToSrcSet, imgSrcFallback)
import Data.Msg exposing (Msg)
import Data.PageData as PageData exposing (ProductData)
import Data.Routing.Routing exposing (Route(..))
import Data.Shared exposing (Shared)
import Dict exposing (Dict)
import Html exposing (Html, a, div, h1, hr, img, text)
import Html.Attributes exposing (alt, attribute, class, src)
import Paginate exposing (Paginated)
import Utils.View exposing (rawHtml, routeLinkAttributes)


view :
    Shared Msg
    -> Pagination.Data
    -> Dict Int Product.Model
    -> Paginated ProductData { slug : String, sorting : Sorting.Option } PageData.CategoryDetails
    -> List (Html Msg)
view shared pagination productDict products =
    let
        { category, subCategories } =
            case Paginate.getResponseData products of
                Just r ->
                    r

                Nothing ->
                    { category = Category.initial, subCategories = [], predecessors = [] }

        subCategoryCards =
            if List.length subCategories > 0 then
                List.map subCategoryCard subCategories
                    |> div [ class "tw:grid tw:grid-cols-2 tw:lg:grid-cols-4 tw:gap-[24px] tw:pb-[32px]" ]

            else
                text ""

        subCategoryCard subCategory =
            div [ class "tw:bg-[rgba(30,12,3,0.02)] tw:rounded-[16px] tw:p-[16px]" ]
                [ a (routeLinkAttributes <| CategoryDetails subCategory.slug Pagination.default)
                    [ div [ class "tw:flex tw:flex-col tw:gap-[8px] tw:text-center tw:items-center" ]
                        [ div [ class "tw:rounded-[8px] tw:overflow-hidden" ]
                            [ img
                                [ class ""
                                , src subCategory.image.original
                                , alt <| "Category Image for " ++ subCategory.name
                                ]
                                []
                            ]
                        , div [ class "my-auto" ] [ text subCategory.name ]
                        ]
                    ]
                ]

        categoryImage =
            div [ class "tw:rounded-[8px] tw:overflow-hidden tw:border tw:border-[rgba(30,12,3,0.08)]" ]
                [ img
                    [ class "img-fluid"
                    , src <| imgSrcFallback category.image
                    , imageToSrcSet category.image
                    , attribute "sizes" "100px"
                    , alt <| "Category Image for " ++ category.name
                    ]
                    []
                ]
    in
    [ div [ class "tw:px-0 tw:lg:px-[16px]" ]
        [ div [ class "tw:flex tw:gap-[12px] tw:pb-[20px] tw:flex-col tw:lg:flex-row tw:lg:items-center tw:gap-[12px]" ]
            [ div [ class "tw:flex tw:items-start" ]
                [ categoryImage ]
            , h1 [ class "mb-0 se-h1" ] [ text category.name ]
            ]

        -- , hr [ class "mt-2" ] []
        , div [ class "tw:pb-[24px] static-page" ]
            [ rawHtml category.description
            ]
        , subCategoryCards
        ]
    ]
        ++ ProductViews.listView shared (CategoryDetails category.slug) pagination productDict products
