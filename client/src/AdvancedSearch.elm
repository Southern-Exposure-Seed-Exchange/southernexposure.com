module AdvancedSearch exposing (Msg(..), update, view)

import Category exposing (CategoryId(..))
import Components.Svg exposing (heirLoomSvg, organicSvg, searchSvg, smallFarmSvg, sunSvg)
import Html exposing (..)
import Html.Attributes exposing (checked, class, id, name, placeholder, selected, src, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode
import Products.Pagination as Pagination
import RemoteData exposing (RemoteData(..), WebData)
import Routing exposing (Route(..))
import Search
import SeedAttribute
import SiteUI exposing (CategoryListData)
import Views.Aria as Aria


type Msg
    = KeywordInput String
    | SearchTitles
    | SearchTitlesAndDescriptions
    | IsOrganic Bool
    | IsHeirloom Bool
    | IsRegional Bool
    | IsSmallGrower Bool
    | CategorySelect (Maybe CategoryId)


update : Msg -> Search.Data -> ( Search.Data, Search.FormActionType )
update msg data =
    case msg of
        KeywordInput str ->
            ( { data | query = str }, Search.NoSubmitForm )

        SearchTitles ->
            ( { data | searchIn = Search.Titles }, Search.NoSubmitForm )

        SearchTitlesAndDescriptions ->
            ( { data | searchIn = Search.TitlesAndDescriptions }, Search.NoSubmitForm )

        IsOrganic value ->
            ( { data | isOrganic = value }, Search.SubmitForm )

        IsHeirloom value ->
            ( { data | isHeirloom = value }, Search.SubmitForm )

        IsRegional value ->
            ( { data | isRegional = value }, Search.SubmitForm )

        IsSmallGrower value ->
            ( { data | isSmallGrower = value }, Search.SubmitForm )

        CategorySelect value ->
            ( { data | category = value }, Search.SubmitForm )


mainView : Route -> (Route -> msg) -> (Msg -> msg) -> Search.Data -> CategoryListData -> List (Html msg)
mainView route routingMsg formMsg data categories =
    let
        showDetail =
            case route of
                AdvancedSearch ->
                    True

                _ ->
                    False

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

        filterInput { msg, attribute, selector, content, svgIcon } =
            div [ class "form-check d-block tw:py-[8px]" ]
                [ label [ class "form-check-label tw:flex! tw:items-center tw:gap-[8px] tw:cursor-pointer" ]
                    [ input
                        [ class "form-check-input tw:mt-[0px]!"
                        , type_ "checkbox"
                        , onCheck <|
                            formMsg
                                << msg
                        , checked <| selector data
                        ]
                        []
                    , span [ class "tw:line-clamp-1" ] [ text content ]
                    , div [ class "tw:w-[20px]" ]
                        [ svgIcon
                        ]
                    ]
                ]

        filterCheckboxes =
            List.map filterInput
                [ { msg = IsOrganic
                  , attribute = SeedAttribute.Organic
                  , selector = .isOrganic
                  , content = "Organic"
                  , svgIcon = organicSvg
                  }
                , { msg = IsHeirloom
                  , attribute = SeedAttribute.Heirloom
                  , selector = .isHeirloom
                  , content = "Heirloom"
                  , svgIcon = heirLoomSvg
                  }
                , { msg = IsRegional
                  , attribute = SeedAttribute.Regional
                  , selector = .isRegional
                  , content = "Especially well-suited to the South-East"
                  , svgIcon = sunSvg
                  }
                , { msg = IsSmallGrower
                  , attribute = SeedAttribute.SmallGrower
                  , selector = .isSmallGrower
                  , content = "From Small Farms"
                  , svgIcon = smallFarmSvg
                  }
                ]

        categorySelect =
            select
                [ onCategorySelect <| formMsg << CategorySelect
                , Aria.label "Filter by Category"
                , class "tw:cursor-pointer tw:opacity-60"
                ]
            <|
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

        submitButton =
            button [ class "shrink-0", type_ "submit" ] [ searchSvg ]

        searchBar : Html msg
        searchBar =
            div [ class "tw:w-full tw:py-[16px] tw:px-[18px] bg-white tw:rounded-[16px] tw:border tw:border-[rgba(232,231,230,1)] tw:flex tw:gap-[8px] tw:items-center " ]
                [ submitButton
                , input
                    [ id "keywords"
                    , class "tw:block tw:w-full tw:placeholder:text-[rgba(187,182,179,1)]"
                    , type_ "search"
                    , value data.query
                    , onInput <| formMsg << KeywordInput
                    , Aria.label "Search Terms"
                    , placeholder "Search"
                    ]
                    []
                , categorySelect
                ]

        searchIn =
            if showDetail then
                [ div []
                    [ label [ class "mr-4 font-weight-bold" ] [ text "Search In: " ]
                    , radioInput SearchTitles .searchIn Search.Titles "Titles"
                    , radioInput SearchTitlesAndDescriptions
                        .searchIn
                        Search.TitlesAndDescriptions
                        "Titles & Descriptions"
                    ]
                ]

            else
                []
    in
    (if showDetail then
        [ h1 [] [ text "Advanced Search" ]
        , hr [ class "tw:pb-[36px]" ] []
        ]

     else
        []
    )
        ++ form [ class "tw:pb-[28px] tw:flex tw:flex-col tw:gap-[16px]", onSubmit << routingMsg <| SearchResults data Pagination.default, class "advanced-search" ]
            [ searchBar
            , div [ class "tw:w-full tw:flex tw:justify-between tw:px-[16px] tw:gap-[24px]" ] filterCheckboxes
            ]
        :: searchIn


view : Route -> (Route -> msg) -> (Msg -> msg) -> Search.Data -> WebData CategoryListData -> List (Html msg)
view route routingMsg formMsg data categoriesRd =
    case categoriesRd of
        Success categories ->
            mainView route routingMsg formMsg data categories

        _ ->
            []
