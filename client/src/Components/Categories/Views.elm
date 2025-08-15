module Components.Categories.Views exposing (details)

import Data.Category as Category
import Html exposing (Html, a, div, h1, hr, img, text)
import Html.Attributes exposing (alt, attribute, class, src)
import Data.Msg exposing (Msg)
import Data.Fields exposing (imageToSrcSet, imgSrcFallback)
import Data.PageData as PageData exposing (ProductData)
import Pages.Cart.Type exposing (CartForms)
import Paginate exposing (Paginated)
import Components.Products.Pagination as Pagination
import Components.Products.Sorting as Sorting
import Components.Products.Views as ProductViews
import Data.Routing.Routing as Routing exposing (Route(..))
import Utils.View exposing (rawHtml, routeLinkAttributes)


details :
    Pagination.Data
    -> CartForms
    -> Paginated ProductData { slug : String, sorting : Sorting.Option } PageData.CategoryDetails
    -> List (Html Msg)
details pagination addToCartForms products =
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
                    |> div [ class "tw:grid tw:grid-cols-4 tw:gap-[24px]" ]

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
    in
    [ div [ class "tw:px-[16px]" ]
        [ div [ class "tw:flex tw:gap-[12px] tw:pb-[20px]" ]
            [ div [ class "tw:rounded-[8px] tw:overflow-hidden tw:border tw:border-[rgba(30,12,3,0.08)]" ]
                [ img
                    [ class "img-fluid"
                    , src <| imgSrcFallback category.image
                    , imageToSrcSet category.image
                    , attribute "sizes" "100px"
                    , alt <| "Category Image for " ++ category.name
                    ]
                    []
                ]
            , h1 [ class "mb-0 pl-2" ] [ text category.name ]
            ]

        -- , hr [ class "mt-2" ] []
        , div [ class "tw:pb-[24px] static-page" ]
            [ rawHtml category.description
            ]
        , subCategoryCards
        , div [ class "tw:pb-[32px]" ] []
        ]
    ]
        ++ ProductViews.listView (CategoryDetails category.slug) pagination addToCartForms products
