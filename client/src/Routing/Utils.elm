module Routing.Utils
    exposing
        ( fromIntParam
        , optionalIntParam
        , parseFlag
        , joinPath
        , queryParameter
        , queryFlag
        , withQueryStrings
        , joinQueryStrings
        )

import UrlParser exposing (QueryParser, customParam)


-- Parsing


fromIntParam : String -> (Int -> a) -> QueryParser (Maybe a -> b) b
fromIntParam param fromInt =
    customParam param
        (Maybe.andThen (String.toInt >> Result.toMaybe)
            >> Maybe.map fromInt
        )


optionalIntParam : String -> Int -> QueryParser (Int -> b) b
optionalIntParam param default =
    customParam param
        (Maybe.andThen (String.toInt >> Result.toMaybe)
            >> Maybe.withDefault default
        )


parseFlag : String -> QueryParser (Bool -> b) b
parseFlag param =
    customParam param (Maybe.map ((==) "1") >> Maybe.withDefault False)



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
