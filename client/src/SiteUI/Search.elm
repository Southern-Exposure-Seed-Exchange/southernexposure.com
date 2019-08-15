module SiteUI.Search exposing
    ( Data
    , Msg(..)
    , form
    , update
    )

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Products.Pagination as Pagination
import Routing exposing (Route(..), reverse)
import Search


type alias Data =
    Search.Data



-- UPDATE


type Msg
    = Update String
    | Submit


update : Routing.Key -> Msg -> Data -> ( Data, Cmd msg )
update key msg data =
    case msg of
        Update query ->
            ( { data | query = query }, Cmd.none )

        Submit ->
            ( data, Routing.newUrl key <| SearchResults data Pagination.default )



-- VIEW


form : (Msg -> msg) -> String -> Search.Data -> Html msg
form tagger buttonColor { query } =
    Html.form [ onSubmit <| tagger Submit ]
        [ div [ class "input-group input-group-sm" ]
            [ input
                [ class "form-control"
                , value query
                , type_ "search"
                , onInput <| tagger << Update
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ class <| "btn btn-" ++ buttonColor, type_ "submit" ] [ text "Search" ]
                ]
            ]
        ]
