module Views.Category exposing (adminList, details)

import Category
import Html exposing (..)
import Html.Attributes exposing (attribute, class, src, style)
import Messages exposing (Msg)
import Model exposing (CartForms)
import Models.Fields exposing (imageToSrcSet, imgSrcFallback)
import PageData exposing (ProductData)
import Paginate exposing (Paginated)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Products.Views as ProductViews
import Routing exposing (Route(..))
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
                            , src <| imgSrcFallback subCategory.image
                            , imageToSrcSet subCategory.image
                            , attribute "sizes" <|
                                String.join ", "
                                    [ "(max-width: 575px) 257px"
                                    , "(max-width: 767px) 160px"
                                    , "(max-width: 991px) 115px"
                                    , "(max-width: 1199px) 160px"
                                    , "225px"
                                    ]
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
            , src <| imgSrcFallback category.image
            , imageToSrcSet category.image
            , attribute "sizes" "100px"
            ]
            []
        , h1 [ class "mb-0 pl-2" ] [ text category.name ]
        ]
    , hr [ class "mt-2" ] []
    , rawHtml category.description
    , subCategoryCards
    ]
        ++ ProductViews.list (CategoryDetails category.slug) pagination addToCartForms products


{-| TODO: Add Button to Create New Category, Add Edit Buttons to Table
-}
adminList : PageData.AdminCategoryListData -> List (Html Msg)
adminList { roots } =
    let
        depthPadding depth =
            style "margin-left" <| String.fromInt (depth * 20 + 10) ++ "px"

        renderCategory depth (PageData.AdminListCategory c) =
            tr []
                [ td [] [ div [ depthPadding depth ] [ text c.name ] ]
                ]
                :: List.concatMap (renderCategory <| depth + 1) c.children
    in
    [ table [ class "table table-sm table-striped" ]
        [ tbody [] <|
            List.concatMap (renderCategory 0) roots
        ]
    ]
