module SiteUI.Search exposing
    ( Data
    , Msg(..)
    , form
    , update
    )

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, name, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Products.Pagination as Pagination
import Routing exposing (Route(..))
import Search
import Views.Microdata as Microdata


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
    div Microdata.website
        [ Microdata.urlMeta "https://www.southernexposure.com/"
        , Html.form (onSubmit (tagger Submit) :: Microdata.potentialAction :: Microdata.searchAction)
            [ Microdata.targetMeta "https://www.southernexposure.com/search/?q={header_search}"
            , div [ class "input-group input-group-sm" ]
                [ input
                    [ class "form-control"
                    , value query
                    , type_ "search"
                    , name "header_search"
                    , onInput <| tagger << Update
                    , required True
                    , Microdata.queryInput
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ button [ class <| "btn btn-" ++ buttonColor, type_ "submit" ] [ text "Search" ]
                    ]
                ]
            ]
        ]
