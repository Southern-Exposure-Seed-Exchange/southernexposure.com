module SiteUI.Navigation exposing (view)

import Dict
import Html exposing (Html, text, div, ul, li, span, button, a, node, form)
import Html.Attributes exposing (attribute, id, class, href, type_)
import RemoteData exposing (WebData)
import Category exposing (CategoryId(..))
import Messages exposing (Msg(SearchMsg))
import Products.Pagination as Pagination
import Routing exposing (Route(..), reverse)
import Search
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch
import Views.Utils exposing (routeLinkAttributes, icon)


view : WebData NavigationData -> List CategoryId -> Search.Data -> Html Msg
view navigationData activeCategories searchData =
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
                        li [ class <| "nav-item" ++ activeClass category ]
                            [ a
                                ([ class "nav-link" ]
                                    ++ (routeLinkAttributes <|
                                            CategoryDetails category.slug Pagination.default
                                       )
                                )
                                [ text category.name ]
                            ]

                    Just children ->
                        li [ class <| "nav-item dropdown" ++ activeClass category ]
                            [ a
                                [ class "nav-link dropdown-toggle"
                                , href <|
                                    reverse <|
                                        CategoryDetails category.slug Pagination.default
                                , attribute "data-toggle" "dropdown"
                                , attribute "aria-haspopup" "true"
                                , attribute "aria-expanded" "false"
                                ]
                                [ text category.name ]
                            , div [ class "dropdown-menu mt-0" ] <| dropdownCategories children
                            ]

        childCategory category =
            a
                ([ class <| "dropdown-item" ++ activeClass category ]
                    ++ (routeLinkAttributes <|
                            CategoryDetails category.slug Pagination.default
                       )
                )
                [ text category.name ]

        activeClass category =
            if List.member category.id activeCategories then
                " active "
            else
                ""
    in
        div [ id "navigation", class "container" ]
            [ node "nav"
                [ class "navbar navbar-expand-md navbar-light bg-success" ]
                [ button
                    [ class "navbar-toggler ml-auto"
                    , type_ "button"
                    , attribute "data-toggle" "collapse"
                    , attribute "data-target" "#search-navbar"
                    , attribute "aria-controls" "navbarSupportedContent"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "Toggle navigation"
                    ]
                    [ icon "search p-1" ]
                , button
                    [ class "navbar-toggler ml-2"
                    , type_ "button"
                    , attribute "data-toggle" "collapse"
                    , attribute "data-target" "#category-navbar"
                    , attribute "aria-controls" "navbarSupportedContent"
                    , attribute "aria-expanded" "false"
                    , attribute "aria-label" "Toggle navigation"
                    ]
                    [ span [ class "navbar-toggler-icon" ] [] ]
                , div [ id "search-navbar", class "collapse navbar-collapse" ]
                    [ div [ class "mt-2" ] [ SiteSearch.form SearchMsg "light" searchData ]
                    , ul [ class "nav navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ a (class "nav-link" :: routeLinkAttributes AdvancedSearch)
                                [ text "Advanced Search" ]
                            ]
                        ]
                    ]
                , div [ id "category-navbar", class "collapse navbar-collapse" ]
                    [ ul [ class "navbar-nav mx-auto d-flex text-left" ]
                        categoryNavigation
                    ]
                ]
            ]
