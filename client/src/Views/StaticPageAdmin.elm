module Views.StaticPageAdmin exposing (list)

import Html exposing (Html, table, tbody, td, text, tr)
import Html.Attributes exposing (class)
import PageData


list : PageData.AdminPageListData -> List (Html msg)
list { pages } =
    let
        renderPage { name } =
            tr []
                [ td [] [ text name ] ]
    in
    [ table [ class "table table-sm table-striped" ]
        [ tbody [] <| List.map renderPage pages ]
    ]
