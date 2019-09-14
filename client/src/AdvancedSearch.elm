module AdvancedSearch exposing (Msg(..), update, view)

import Category exposing (CategoryId(..))
import Html exposing (..)
import Html.Attributes exposing (checked, class, for, id, selected, src, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode
import PageData
import Products.Pagination as Pagination
import Routing exposing (Route(..))
import Search
import SeedAttribute


type Msg
    = KeywordInput String
    | SearchTitles
    | SearchTitlesAndDescriptions
    | IsOrganic Bool
    | IsHeirloom Bool
    | IsRegional Bool
    | IsSmallGrower Bool
    | CategorySelect (Maybe CategoryId)


update : Msg -> Search.Data -> Search.Data
update msg data =
    case msg of
        KeywordInput str ->
            { data | query = str }

        SearchTitles ->
            { data | searchIn = Search.Titles }

        SearchTitlesAndDescriptions ->
            { data | searchIn = Search.TitlesAndDescriptions }

        IsOrganic value ->
            { data | isOrganic = value }

        IsHeirloom value ->
            { data | isHeirloom = value }

        IsRegional value ->
            { data | isRegional = value }

        IsSmallGrower value ->
            { data | isSmallGrower = value }

        CategorySelect value ->
            { data | category = value }


view : (Route -> msg) -> (Msg -> msg) -> Search.Data -> PageData.AdvancedSearch -> List (Html msg)
view routingMsg formMsg data categories =
    let
        radioInput msg selector value content =
            div [ class "form-check form-check-inline d-block d-sm-inline-flex" ]
                [ label [ class "form-check-label" ]
                    [ input
                        [ class "form-check-input"
                        , type_ "radio"
                        , onClick <| formMsg msg
                        , checked (selector data == value)
                        ]
                        []
                    , text content
                    ]
                ]

        filterInput { msg, attribute, selector, content } =
            div [ class "form-check form-check-inline d-block d-sm-inline-flex" ]
                [ label [ class "form-check-label" ]
                    [ input
                        [ class "form-check-input"
                        , type_ "checkbox"
                        , onCheck <|
                            formMsg
                                << msg
                        , checked <| selector data
                        ]
                        []
                    , img [ src <| SeedAttribute.iconUrl attribute ] []
                    , text content
                    ]
                ]

        filterCheckboxes =
            List.map filterInput
                [ { msg = IsOrganic
                  , attribute = SeedAttribute.Organic
                  , selector = .isOrganic
                  , content = "Organic"
                  }
                , { msg = IsHeirloom
                  , attribute = SeedAttribute.Heirloom
                  , selector = .isHeirloom
                  , content = "Heirloom"
                  }
                , { msg = IsRegional
                  , attribute = SeedAttribute.Regional
                  , selector = .isRegional
                  , content = "South-East"
                  }
                , { msg = IsSmallGrower
                  , attribute = SeedAttribute.SmallGrower
                  , selector = .isSmallGrower
                  , content = "From Small Farms"
                  }
                ]

        categorySelect =
            select [ class "form-control", onCategorySelect <| formMsg << CategorySelect ] <|
                option [ value "", selected (data.category == Nothing) ] [ text "All Categories" ]
                    :: List.map categoryOption categories

        categoryOption { id, name } =
            let
                (CategoryId idAsInt) =
                    id
            in
            option [ value <| String.fromInt idAsInt, selected (Just id == data.category) ]
                [ text name ]

        onCategorySelect msg =
            targetValue
                |> Decode.map (String.toInt >> Maybe.map CategoryId >> msg)
                |> on "change"
    in
    [ h1 [] [ text "Advanced Search" ]
    , hr [] []
    , form [ onSubmit << routingMsg <| SearchResults data Pagination.default, class "advanced-search" ]
        [ div [ class "form-group" ]
            [ legend [ class "font-weight-bold", for "keywords" ] [ text "Keywords: " ]
            , input
                [ id "keywords"
                , class "form-control"
                , type_ "search"
                , value data.query
                , onInput <| formMsg << KeywordInput
                ]
                []
            ]
        , div []
            [ label [ class "mr-4 font-weight-bold" ] [ text "Search In: " ]
            , radioInput SearchTitles .searchIn Search.Titles "Titles"
            , radioInput SearchTitlesAndDescriptions
                .searchIn
                Search.TitlesAndDescriptions
                "Titles & Descriptions"
            ]
        , legend [ class "font-weight-bold" ] [ text "Filters:" ]
        , div [ class "row align-items-center" ]
            [ div [ class "col filters mb-3" ] filterCheckboxes
            , div [ class "col-auto mb-3" ] [ categorySelect ]
            ]
        , button [ class "mb-3 btn btn-primary", type_ "submit" ] [ text "Submit" ]
        ]
    ]
