module Products.AdminViews exposing
    ( ListForm
    , ListMsg
    , initialListForm
    , list
    , updateListForm
    )

import Html exposing (Html, div, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, for, id, name, type_, value)
import Html.Events exposing (onCheck, onInput)
import PageData
import Views.Utils exposing (icon)


type alias ListForm =
    { query : String
    , onlyActive : Bool
    }


initialListForm : ListForm
initialListForm =
    { query = ""
    , onlyActive = True
    }


type ListMsg
    = InputQuery String
    | InputOnlyActive Bool


updateListForm : ListMsg -> ListForm -> ListForm
updateListForm msg model =
    case msg of
        InputQuery val ->
            { model | query = val }

        InputOnlyActive val ->
            { model | onlyActive = val }


{-| TODO: Add edit links to table rows, add New Product button
-}
list : ListForm -> PageData.AdminProductListData -> List (Html ListMsg)
list listForm { products } =
    let
        activeIcon isActive =
            if isActive then
                icon "check-circle text-success"

            else
                icon "times-circle text-danger"

        renderProduct { id, name, baseSku, categories, isActive } =
            tr []
                [ td [] [ text <| String.fromInt id ]
                , td [] [ text baseSku ]
                , td [] [ text name ]
                , td [] [ text <| String.join ", " categories ]
                , td [ class "text-center" ] [ activeIcon isActive ]
                , td [] [ text "Edit" ]
                ]

        searchInput =
            div [ class "input-group mr-4" ]
                [ div [ class "input-group-prepend" ] [ span [ class "input-group-text" ] [ icon "search" ] ]
                , input
                    [ type_ "search"
                    , name "search"
                    , value listForm.query
                    , onInput InputQuery
                    , class "form-control"
                    ]
                    []
                ]

        onlyActiveInput =
            div [ class "flex-shrink-0 form-check form-check-inline" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , id "onlyActive"
                    , checked listForm.onlyActive
                    , onCheck InputOnlyActive
                    ]
                    []
                , label [ class "form-check-label", for "onlyActive" ]
                    [ text "Only Active Products" ]
                ]

        filterProduct p =
            List.foldr
                (\t b ->
                    b
                        && (iContains t p.name
                                || iContains t p.baseSku
                                || iContains t (String.fromInt p.id)
                                || List.any (iContains t) p.categories
                           )
                        && (p.isActive || not listForm.onlyActive)
                )
                True
                queryTerms

        iContains s1 s2 =
            String.contains s1 (String.toLower s2)

        queryTerms =
            String.words listForm.query
                |> List.map String.toLower
    in
    [ div [ class "d-flex align-items-center justify-content-between mb-2" ]
        [ searchInput, onlyActiveInput ]
    , table [ class "table table-striped table-sm" ]
        [ thead []
            [ tr [ class "text-center" ]
                [ th [] [ text "ID" ]
                , th [] [ text "SKU" ]
                , th [] [ text "Name" ]
                , th [] [ text "Category" ]
                , th [] [ text "Active" ]
                , th [] []
                ]
            ]
        , tbody [] <| List.map renderProduct <| List.filter filterProduct products
        ]
    ]
