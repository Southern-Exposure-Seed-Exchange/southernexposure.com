module Views.Category exposing (details)

import Category
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Messages exposing (Msg)
import Model exposing (CartForms)
import PageData exposing (ProductData)
import Paginate exposing (Paginated)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Products.Views as ProductViews
import Routing exposing (Route(..))
import Views.Images as Images
import Views.Utils exposing (rawHtml, routeLinkAttributes)


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
                    |> div [ class "row" ]

            else
                text ""

        subCategoryCard subCategory =
            div [ class "col-6 col-sm-4 col-md-3 mb-2" ]
                [ a (routeLinkAttributes <| CategoryDetails subCategory.slug Pagination.default)
                    [ div [ class "h-100 text-center" ]
                        [ img
                            [ class "img-fluid mx-auto"
                            , src <| Images.media <| "categories/" ++ subCategory.imageURL
                            ]
                            []
                        , div [ class "my-auto" ] [ text subCategory.name ]
                        ]
                    ]
                ]
    in
    [ div [ class "d-flex align-items-center" ]
        [ img
            [ class "img-fluid"
            , src <| Images.media <| "categories/" ++ category.imageURL
            ]
            []
        , h1 [ class "mb-0 pl-2" ] [ text category.name ]
        ]
    , hr [ class "mt-2" ] []
    , rawHtml category.description
    , subCategoryCards
    ]
        ++ ProductViews.list (CategoryDetails category.slug) pagination addToCartForms products
