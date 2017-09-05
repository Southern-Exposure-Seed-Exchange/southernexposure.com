module Search
    exposing
        ( Data
        , initial
        , SearchScope(..)
        , resetQuery
        , encode
        , toQueryString
        , fromQueryString
        , UniqueSearch(..)
        , uniqueSearch
        )

import Json.Encode as Encode exposing (Value)
import UrlParser as Url exposing ((<?>))
import Category exposing (CategoryId(..))
import Routing.Utils as Routing exposing (queryFlag, queryParameter)
import SeedAttribute


type alias Data =
    { query : String
    , searchIn : SearchScope
    , isOrganic : Bool
    , isHeirloom : Bool
    , isRegional : Bool
    , isEcological : Bool
    , category : Maybe CategoryId
    }


type SearchScope
    = Titles
    | TitlesAndDescriptions


initial : Data
initial =
    { query = ""
    , searchIn = TitlesAndDescriptions
    , isOrganic = False
    , isHeirloom = False
    , isRegional = False
    , isEcological = False
    , category = Nothing
    }


resetQuery : Data -> Data
resetQuery data =
    { data | query = "" }


encode : Data -> Value
encode data =
    Encode.object
        [ ( "query", Encode.string data.query )
        , ( "searchDescription", Encode.bool <| data.searchIn == TitlesAndDescriptions )
        , ( "filterOrganic", Encode.bool data.isOrganic )
        , ( "filterHeirloom", Encode.bool data.isHeirloom )
        , ( "filterRegional", Encode.bool data.isRegional )
        , ( "filterEcological", Encode.bool data.isEcological )
        , ( "category"
          , Maybe.map (\(CategoryId i) -> Encode.int i) data.category
                |> Maybe.withDefault Encode.null
          )
        ]


toQueryString : Data -> String
toQueryString data =
    [ queryParameter ( "q", data.query )
    , queryFlag "organic" data.isOrganic
    , queryFlag "heirloom" data.isHeirloom
    , queryFlag "southeast" data.isRegional
    , queryFlag "ecological" data.isEcological
    , queryFlag "titlesOnly" (data.searchIn == Titles)
    , Maybe.map (\(CategoryId i) -> queryParameter ( "category", toString i ))
        data.category
        |> Maybe.withDefault ""
    ]
        |> Routing.joinQueryStrings


fromQueryString :
    Url.Parser
        ((Data -> a)
         -> String
         -> SearchScope
         -> Bool
         -> Bool
         -> Bool
         -> Bool
         -> Maybe CategoryId
         -> a
        )
        (String -> SearchScope -> Bool -> Bool -> Bool -> Bool -> Maybe CategoryId -> b)
    -> Url.Parser (b -> c) c
fromQueryString pathParser =
    pathParser
        <?> Url.customParam "q" (Maybe.withDefault "")
        <?> Url.customParam "titlesOnly"
                (Maybe.map (always Titles)
                    >> Maybe.withDefault TitlesAndDescriptions
                )
        <?> Routing.parseFlag "organic"
        <?> Routing.parseFlag "heirloom"
        <?> Routing.parseFlag "regional"
        <?> Routing.parseFlag "ecological"
        <?> Routing.fromIntParam "category" CategoryId
        |> Url.map
            (\constructor q descr org heir reg eco cat ->
                constructor <| Data q descr org heir reg eco cat
            )



-- Unique Searches


type UniqueSearch
    = AttributeSearch SeedAttribute.Attribute
    | AllProducts


uniqueSearch : Data -> Maybe UniqueSearch
uniqueSearch { query, searchIn, category, isOrganic, isHeirloom, isRegional, isEcological } =
    case ( query, searchIn, category ) of
        ( "", TitlesAndDescriptions, Nothing ) ->
            case ( isOrganic, isHeirloom, isRegional, isEcological ) of
                ( True, False, False, False ) ->
                    Just <| AttributeSearch SeedAttribute.Organic

                ( False, True, False, False ) ->
                    Just <| AttributeSearch SeedAttribute.Heirloom

                ( False, False, True, False ) ->
                    Just <| AttributeSearch SeedAttribute.Regional

                ( False, False, False, True ) ->
                    Just <| AttributeSearch SeedAttribute.Ecological

                ( False, False, False, False ) ->
                    Just AllProducts

                _ ->
                    Nothing

        _ ->
            Nothing
