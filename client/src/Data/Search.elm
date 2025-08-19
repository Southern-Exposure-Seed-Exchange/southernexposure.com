module Data.Search exposing
    ( Data
    , FormActionType(..)
    , SearchScope(..)
    , UniqueSearch(..)
    , encode
    , fromQueryString
    , initial
    , resetQuery
    , toQueryString
    , uniqueSearch
    )

import Data.Category as Category exposing (CategoryId(..))
import Data.Routing.Utils as Routing exposing (fromStringParam, fromStringWithDefaultParam, queryFlag, queryParameter)
import Data.SeedAttribute as SeedAttribute
import Json.Encode as Encode exposing (Value)
import Url.Parser as Url exposing ((<?>))


type alias Data =
    { query : String
    , searchIn : SearchScope
    , isOrganic : Bool
    , isHeirloom : Bool
    , isRegional : Bool
    , isSmallGrower : Bool
    , category : Maybe CategoryId
    , mobileFilterStatus : Bool
    }


type SearchScope
    = Titles
    | TitlesAndDescriptions


type FormActionType
    = SubmitForm
    | NoSubmitForm


initial : Data
initial =
    { query = ""
    , searchIn = TitlesAndDescriptions
    , isOrganic = False
    , isHeirloom = False
    , isRegional = False
    , isSmallGrower = False
    , category = Nothing
    , mobileFilterStatus = False
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
        , ( "filterSmallGrower", Encode.bool data.isSmallGrower )
        , ( "category"
          , Maybe.map (\(CategoryId i) -> Encode.int i) data.category
                |> Maybe.withDefault Encode.null
          )
        ]



-- Query String Generation / Parsing


searchQueryFlagName : String
searchQueryFlagName =
    "q"


organicFlagName : String
organicFlagName =
    "organic"


heirloomFlagName : String
heirloomFlagName =
    "heirloom"


regionalFlagName : String
regionalFlagName =
    "regional"


smallGrowerFlagName : String
smallGrowerFlagName =
    "small-grower"


categoryFlagName : String
categoryFlagName =
    "category"


titlesOnlyFlagName : String
titlesOnlyFlagName =
    "titlesOnly"


toQueryString : Data -> String
toQueryString data =
    [ queryParameter ( searchQueryFlagName, data.query )
    , queryFlag organicFlagName data.isOrganic
    , queryFlag heirloomFlagName data.isHeirloom
    , queryFlag regionalFlagName data.isRegional
    , queryFlag smallGrowerFlagName data.isSmallGrower
    , queryFlag titlesOnlyFlagName (data.searchIn == Titles)
    , Maybe.map (\(CategoryId i) -> queryParameter ( categoryFlagName, String.fromInt i ))
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
        <?> fromStringParam searchQueryFlagName identity
        <?> fromStringWithDefaultParam titlesOnlyFlagName (always Titles) TitlesAndDescriptions
        <?> Routing.parseFlag organicFlagName
        <?> Routing.parseFlag heirloomFlagName
        <?> Routing.parseFlag regionalFlagName
        <?> Routing.parseFlag smallGrowerFlagName
        <?> Routing.fromIntParam categoryFlagName CategoryId
        |> Url.map
            (\constructor q descr org heir reg eco cat ->
                constructor <| Data q descr org heir reg eco cat initial.mobileFilterStatus
            )



-- Unique Searches


type UniqueSearch
    = AttributeSearch SeedAttribute.Attribute
    | AllProducts


uniqueSearch : Data -> Maybe UniqueSearch
uniqueSearch { query, searchIn, category, isOrganic, isHeirloom, isRegional, isSmallGrower } =
    case ( query, searchIn, category ) of
        ( "", TitlesAndDescriptions, Nothing ) ->
            let
                none =
                    List.all ((==) False)
            in
            if isOrganic && none [ isHeirloom, isRegional, isSmallGrower ] then
                Just <| AttributeSearch SeedAttribute.Organic

            else if isHeirloom && none [ isOrganic, isRegional, isSmallGrower ] then
                Just <| AttributeSearch SeedAttribute.Heirloom

            else if isRegional && none [ isOrganic, isHeirloom, isSmallGrower ] then
                Just <| AttributeSearch SeedAttribute.Regional

            else if isSmallGrower && none [ isOrganic, isHeirloom, isRegional ] then
                Just <| AttributeSearch SeedAttribute.SmallGrower

            else if none [ isOrganic, isHeirloom, isRegional, isSmallGrower ] then
                Just AllProducts

            else
                Nothing

        _ ->
            Nothing
