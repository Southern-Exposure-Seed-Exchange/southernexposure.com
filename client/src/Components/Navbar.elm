module Components.Navbar exposing (..)

import Data.Category exposing (Category)
import Data.Msg exposing (Msg)
import Data.SiteUI exposing (NavigationData)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)


view : WebData NavigationData -> (Dict Int (List Category) -> Category -> Html Msg) -> List (Html Msg) -> Html Msg -> Html Msg
view navigationData rootCategory mobileOnlyItems allProductLink =
    let
        categoryNavigation =
            RemoteData.toMaybe navigationData
                |> Maybe.map (\data -> List.map (rootCategory data.children) data.roots)
                |> Maybe.withDefault []
    in
    div [ id "category-navbar", class "collapse navbar-collapse tw:pt-[32px] tw:lg:pt-0" ]
        [ ul [ class "navbar-nav d-flex text-left tw:text-[16px] tw:leading-[24px]" ] <|
            [ allProductLink ]
                ++ categoryNavigation
                ++ mobileOnlyItems
        ]
