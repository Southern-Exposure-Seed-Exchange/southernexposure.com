module Components.Navbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData


view navigationData rootCategory mobileOnlyItems =
    let
        categoryNavigation =
            RemoteData.toMaybe navigationData
                |> Maybe.map (\data -> List.map (rootCategory data.children) data.roots)
                |> Maybe.withDefault []
    in
    div [ id "category-navbar", class "collapse navbar-collapse" ]
        [ ul [ class "navbar-nav d-flex text-left tw:text-[16px] tw:leading-[24px]" ] <|
            categoryNavigation
                ++ mobileOnlyItems
        ]
