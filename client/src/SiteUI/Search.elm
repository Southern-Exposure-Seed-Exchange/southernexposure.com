module SiteUI.Search
    exposing
        ( Data
        , Msg(..)
        , update
        , form
        )

import Html exposing (Html, div, span, text, button, input)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Navigation
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import Routing exposing (Route(SearchResults), reverse)
import Search


type alias Data =
    Search.Data



-- UPDATE


type Msg
    = Update String
    | Submit


update : Msg -> Data -> ( Data, Cmd msg )
update msg data =
    case msg of
        Update query ->
            ( { data | query = query }, Cmd.none )

        Submit ->
            ( data, Navigation.newUrl << reverse <| SearchResults data Pagination.default Sorting.default )



-- VIEW


form : (Msg -> msg) -> Search.Data -> Html msg
form tagger { query } =
    Html.form [ onSubmit <| tagger Submit ]
        [ div [ class "input-group input-group-sm" ]
            [ input
                [ class "form-control"
                , value query
                , type_ "text"
                , onInput <| tagger << Update
                ]
                []
            , span [ class "input-group-btn" ]
                [ button [ class "btn btn-primary", type_ "button" ] [ text "Search" ]
                ]
            ]
        ]
