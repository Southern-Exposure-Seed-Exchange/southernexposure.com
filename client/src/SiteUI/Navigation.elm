module SiteUI.Navigation exposing (view)

import Dict
import Html exposing (Html, text, div, ul, li, span, button, a, node)
import Html.Attributes exposing (attribute, id, class, href, type_)
import RemoteData exposing (WebData)
import Category exposing (CategoryId(..))
import Messages exposing (Msg)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(..), reverse)
import SiteUI exposing (NavigationData)
import Views.Utils exposing (routeLinkAttributes)


view : WebData NavigationData -> Html Msg
view navigationData =
    let
        categoryNavigation =
            RemoteData.toMaybe navigationData
                |> Maybe.map (\data -> List.map (rootCategory data.children) data.roots)
                |> Maybe.withDefault []

        rootCategory children category =
            let
                (CategoryId categoryId) =
                    category.id

                dropdownCategories children =
                    let
                        childItems =
                            List.map childCategory children

                        itemsPerColumn =
                            ceiling <| (toFloat <| List.length childItems) / 3.0

                        splitItems items =
                            [ List.take itemsPerColumn items
                            , List.drop itemsPerColumn items |> List.take itemsPerColumn
                            , List.drop (itemsPerColumn * 2) items
                            ]
                    in
                        if List.length childItems < 15 then
                            childItems
                        else
                            [ div [ class "row no-gutters multi-column-dropdown" ] <|
                                List.map (\i -> div [ class "col" ] i)
                                    (splitItems childItems)
                            ]
            in
                case Dict.get categoryId children of
                    Nothing ->
                        li [ class "nav-item" ]
                            [ a
                                ([ class "nav-link" ]
                                    ++ (routeLinkAttributes <|
                                            CategoryDetails category.slug
                                                Pagination.default
                                                Sorting.default
                                       )
                                )
                                [ text category.name ]
                            ]

                    Just children ->
                        li [ class "nav-item dropdown" ]
                            [ a
                                [ class "nav-link dropdown-toggle"
                                , href <|
                                    reverse <|
                                        CategoryDetails category.slug
                                            Pagination.default
                                            Sorting.default
                                , attribute "data-toggle" "dropdown"
                                , attribute "aria-haspopup" "true"
                                , attribute "aria-expanded" "false"
                                ]
                                [ text category.name ]
                            , div [ class "dropdown-menu mt-0" ] <| dropdownCategories children
                            ]

        childCategory category =
            a
                ([ class "dropdown-item" ]
                    ++ (routeLinkAttributes <|
                            CategoryDetails
                                category.slug
                                Pagination.default
                                Sorting.default
                       )
                )
                [ text category.name ]
    in
        div [ id "navigation", class "container" ]
            [ node "nav"
                [ class "navbar navbar-expand-md navbar-light bg-success" ]
                [ button
                    [ class "navbar-toggler"
                    , type_ "button"
                    , attribute "data-toggle" "collapse"
                    , attribute "data-target" "#category-navbar"
                    , attribute "aria-controls" "navbarSupportedContent"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "Toggle navigation"
                    ]
                    [ span [ class "navbar-toggler-icon" ] [] ]
                , div [ id "category-navbar", class "collapse navbar-collapse" ]
                    [ ul [ class "navbar-nav mx-auto d-flex text-left" ]
                        categoryNavigation
                    ]
                ]
            ]
