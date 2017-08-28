module Routing.Utils exposing (optionalIntParam)

import UrlParser exposing (QueryParser, customParam)


optionalIntParam : String -> Int -> QueryParser (Int -> b) b
optionalIntParam param default =
    customParam param
        (Maybe.andThen (String.toInt >> Result.toMaybe)
            >> Maybe.withDefault default
        )
