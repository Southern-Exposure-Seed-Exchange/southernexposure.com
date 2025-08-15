module Utils.Decode exposing (..)

import Json.Decode as Decode exposing (Decoder)


unit : Decoder ()
unit =
  Decode.list (Decode.fail "excepted an empty list")
    |> Decode.andThen (\_ -> Decode.succeed ())
