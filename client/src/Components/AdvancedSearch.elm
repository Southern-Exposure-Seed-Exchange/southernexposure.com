module Components.AdvancedSearch exposing (Msg(..), update, view)

import Components.Aria as Aria
import Components.Button as Button exposing (..)
import Components.Pagination as Pagination
import Components.Svg exposing (heirLoomSvg, organicSvg, searchSvg, smallFarmSvg, sunSvg)
import Data.Category exposing (CategoryId(..))
import Data.Routing.Routing exposing (Route(..))
import Data.Search as Search
import Data.SeedAttribute as SeedAttribute
import Data.SiteUI exposing (CategoryListData)
import Html exposing (..)
import Html.Attributes exposing (checked, class, id, name, placeholder, selected, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)


type Msg
    = KeywordInput String
    | SearchTitles
    | SearchTitlesAndDescriptions
    | IsOrganic Bool
    | IsHeirloom Bool
    | IsRegional Bool
    | IsSmallGrower Bool
    | CategorySelect (Maybe CategoryId)
    | SetMobileFilterStatus Bool


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
            ( { data | isOrganic = value, mobileFilterStatus = False }, Search.SubmitForm )

        IsHeirloom value ->
            ( { data | isHeirloom = value, mobileFilterStatus = False }, Search.SubmitForm )

        IsRegional value ->
            ( { data | isRegional = value, mobileFilterStatus = False }, Search.SubmitForm )

        IsSmallGrower value ->
            ( { data | isSmallGrower = value, mobileFilterStatus = False }, Search.SubmitForm )

        CategorySelect value ->
            ( { data | category = value, mobileFilterStatus = False }, Search.SubmitForm )

        SetMobileFilterStatus value ->
            ( { data | mobileFilterStatus = value }, Search.NoSubmitForm )


mainView : (Route -> msg) -> (Msg -> msg) -> Search.Data -> CategoryListData -> List (Html msg)
mainView routingMsg formMsg data categories =
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

        filterInput { msg, selector, content, svgIcon } =
            div [ class "form-check d-block tw:py-[8px] tw:pr-[8px] tw:lg:pr-0" ]
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
                  , content = "Certified Organic"
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

        categorySelect isWidthFull =
            select
                [ onCategorySelect <| formMsg << CategorySelect
                , Aria.label "Filter by Category"
                , class <|
                    (if isWidthFull then
                        "tw:w-full"

                     else
                        "tw:w-[140px]"
                    )
                        ++ " tw:cursor-pointer tw:opacity-60"
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

        -- button [ class "shrink-0", type_ "submit" ] [ searchSvg "tw:fill-[#1E0C03]"]
        searchBar : Html msg
        searchBar =
            div [ class "tw:w-full tw:py-[8px] tw:pl-[18px] tw:pr-[8px] bg-white tw:rounded-[16px] tw:border tw:border-[rgba(232,231,230,1)] tw:flex tw:gap-[8px] tw:items-center " ]
                [ input
                    [ id "keywords"
                    , class "tw:block tw:w-full tw:placeholder:text-[rgba(187,182,179,1)]"
                    , type_ "search"
                    , value data.query
                    , onInput <| formMsg << KeywordInput
                    , Aria.label "Search Terms"
                    , placeholder "Type product name..."
                    ]
                    []
                , categorySelect False
                , div [ class "tw:w-[1px] tw:h-[12px] tw:bg-[rgba(30,12,3,0.2)]" ] []
                , div [ class "tw:pl-[10px]" ]
                    [ Button.view { defaultButton | label = "", icon = Just <| searchSvg "tw:fill-white", type_ = Button.FormSubmit, size = Button.Custom "tw:py-[10px] tw:px-[20px]" }
                    ]
                ]

        desktopView =
            form [ class "tw:hidden tw:lg:flex tw:pb-[28px]  tw:flex-col tw:gap-[16px] advanced-search", onSubmit << routingMsg <| SearchResults data Pagination.default ]
                [ searchBar
                , div [ class "tw:w-full tw:flex tw:justify-between tw:px-[16px] tw:gap-[24px]" ] filterCheckboxes
                ]

        -- Mobile View
        --------------------------------------------
        mobileFilterIcon : Html msg
        mobileFilterIcon =
            button
                [ type_ "button"
                , onClick <| formMsg <| SetMobileFilterStatus (not data.mobileFilterStatus)
                , class "tw:w-[44px] tw:h-[44px] tw:border tw:rounded-[16px]! tw:shrink-0 tw:border-[rgba(77,170,154,1)] tw:flex tw:items-center tw:justify-center tw:bg-[rgba(167,215,197,0.2)]"
                ]
                [ Components.Svg.filterSvg
                ]

        mobileCategorySelect : Html msg
        mobileCategorySelect =
            div [ class "tw:px-[18px] tw:w-full tw:h-[44px] tw:flex tw:items-center tw:justify-center tw:rounded-[16px] tw:border tw:border-[rgba(232,231,230,1)] " ]
                [ categorySelect True
                ]

        mobileSearchBar : Html msg
        mobileSearchBar =
            div [ class "tw:w-full tw:h-[44px] tw:pl-[18px] tw:pr-[8px] bg-white tw:rounded-[16px] tw:border tw:border-[rgba(232,231,230,1)] tw:flex tw:gap-[8px] tw:items-center " ]
                [ input
                    [ id "keywords"
                    , class "tw:block tw:w-full tw:placeholder:text-[rgba(187,182,179,1)]"
                    , type_ "search"
                    , value data.query
                    , onInput <| formMsg << KeywordInput
                    , Aria.label "Search Terms"
                    , placeholder "Type product name..."
                    ]
                    []
                , div [ class "tw:pl-[10px]" ]
                    [ Button.view { defaultButton | label = "", icon = Just <| searchSvg "tw:fill-white", type_ = Button.FormSubmit, size = Button.Custom "tw:py-[8px] tw:px-[12px]" }
                    ]
                ]

        mobileView =
            form [ class "tw:pb-[28px] tw:flex tw:lg:hidden tw:flex-col tw:gap-[16px]", onSubmit << routingMsg <| SearchResults data Pagination.default ] <|
                [ div [ class "tw:flex tw:gap-[16px]" ]
                    [ mobileSearchBar
                    , mobileFilterIcon
                    ]
                ]
                    ++ (if data.mobileFilterStatus then
                            [ mobileCategorySelect
                            , div [ class "tw:w-full tw:flex tw:px-[16px] tw:gap-[6px] tw:flex-wrap" ] filterCheckboxes
                            ]

                        else
                            []
                       )
    in
    [ desktopView
    , mobileView
    ]


view : Route -> (Route -> msg) -> (Msg -> msg) -> Search.Data -> WebData CategoryListData -> List (Html msg)
view route routingMsg formMsg data categoriesRd =
    case categoriesRd of
        Success categories ->
            mainView routingMsg formMsg data categories

        _ ->
            []
