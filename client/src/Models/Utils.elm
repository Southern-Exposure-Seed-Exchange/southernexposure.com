module Models.Utils exposing (slugify)

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
