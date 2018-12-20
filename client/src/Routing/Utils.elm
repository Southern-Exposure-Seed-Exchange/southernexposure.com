module Routing.Utils exposing
    ( fromIntParam
    , fromStringParam
    , fromStringWithDefaultParam
    , joinPath
    , joinQueryStrings
    , optionalIntParam
    , parseFlag
    , queryFlag
    , queryParameter
    , withQueryStrings
    )

import Url.Parser.Query as Query



-- Parsing


fromStringParam : String -> (String -> a) -> Query.Parser a
fromStringParam param fromString =
    Query.map (Maybe.withDefault "" >> fromString) <| Query.string param


fromStringWithDefaultParam : String -> (String -> a) -> a -> Query.Parser a
fromStringWithDefaultParam param mapper default =
    Query.map (Maybe.map mapper >> Maybe.withDefault default) <| Query.string param


fromIntParam : String -> (Int -> a) -> Query.Parser (Maybe a)
fromIntParam param fromInt =
    Query.map (Maybe.map fromInt) <| Query.int param


optionalIntParam : String -> Int -> Query.Parser Int
optionalIntParam param default =
    Query.map (Maybe.withDefault default) <| Query.int param


parseFlag : String -> Query.Parser Bool
parseFlag param =
    fromStringWithDefaultParam param ((==) "1") False



-- Building


joinPath : List String -> String
joinPath paths =
    String.join "/" <| "" :: paths ++ [ "" ]


queryParameter : ( String, String ) -> String
queryParameter ( name, value ) =
    name ++ "=" ++ value


queryFlag : String -> Bool -> String
queryFlag flag showFlag =
    if showFlag then
        queryParameter ( flag, "1" )

    else
        ""


withQueryStrings : List String -> String
withQueryStrings =
    joinQueryStrings >> prependQueryStart


joinQueryStrings : List String -> String
joinQueryStrings =
    List.filter (not << String.isEmpty)
        >> String.join "&"


prependQueryStart : String -> String
prependQueryStart queryString =
    if String.isEmpty queryString then
        ""

    else
        "?" ++ queryString
