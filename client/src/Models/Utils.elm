module Models.Utils exposing (posixToDateString, slugify)

import Iso8601
import Time exposing (Posix)


{-| Strip out undesired characters from a string, returning a value suitable for URLs.

Analogous to the Server's Models.Utils.slugify function.

-}
slugify : String -> String
slugify val =
    let
        cleaned =
            String.map replaceChar val

        replaceChar character =
            if Char.isAlphaNum character || character == '_' || character == '-' then
                character

            else
                ' '
    in
    cleaned
        |> String.toLower
        |> String.words
        |> String.join "-"


{-| Returns just the `YYYY-MM-DD` portion of the time's ISO8601 string.
-}
posixToDateString : Posix -> String
posixToDateString =
    Iso8601.fromTime >> String.left 10
