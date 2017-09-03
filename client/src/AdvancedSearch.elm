module AdvancedSearch exposing (Msg(..), update, view)

import Html exposing (..)
import Html.Attributes exposing (id, class, for, type_, value, checked, selected, src)
import Html.Events exposing (onSubmit, onInput, onClick, onCheck, on, targetValue)
import Json.Decode as Decode
import Category exposing (CategoryId(..))
import Search
import SeedAttribute
import Routing exposing (Route(SearchResults))
import PageData
import Products.Pagination as Pagination


type Msg
    = KeywordInput String
    | SearchTitles
    | SearchTitlesAndDescriptions
    | IsOrganic Bool
    | IsHeirloom Bool
    | IsRegional Bool
    | IsEcological Bool
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

        IsEcological value ->
            { data | isEcological = value }

        CategorySelect value ->
            { data | category = value }


view : (Route -> msg) -> (Msg -> msg) -> Search.Data -> PageData.AdvancedSearch -> List (Html msg)
view routingMsg formMsg data categories =
    let
        radioInput msg selector value content =
            div [ class "form-check form-check-inline" ]
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

        filterInput ( msg, attribute, selector, content ) =
            div [ class "form-check form-check-inline" ]
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
                [ ( IsOrganic, SeedAttribute.Organic, .isOrganic, "Organic" )
                , ( IsHeirloom, SeedAttribute.Heirloom, .isHeirloom, "Heirloom" )
                , ( IsRegional, SeedAttribute.Regional, .isRegional, "South-East" )
                , ( IsEcological, SeedAttribute.Ecological, .isEcological, "Ecologically Grown" )
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
                option [ value <| toString idAsInt, selected (Just id == data.category) ]
                    [ text name ]

        onCategorySelect msg =
            targetValue
                |> Decode.map (String.toInt >> Result.toMaybe >> Maybe.map CategoryId >> msg)
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
                    , type_ "text"
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
                [ div [ class "col-auto filters" ] filterCheckboxes
                , div [ class "col" ] [ categorySelect ]
                ]
            , button [ class "mb-3 btn btn-primary", type_ "submit" ] [ text "Submit" ]
            ]
        ]
