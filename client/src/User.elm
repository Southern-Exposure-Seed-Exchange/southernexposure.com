module User
    exposing
        ( UserId(..)
        , AuthStatus(..)
        , User
        , unauthorized
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)


type UserId
    = UserId Int


type AuthStatus
    = Anonymous
    | Authorized User


type alias User =
    { id : UserId
    , email : String
    , firstName : String
    , lastname : String
    , authToken : String
    }


unauthorized : AuthStatus
unauthorized =
    Anonymous


decoder : Decoder AuthStatus
decoder =
    let
        authorizedUserDecoder =
            Decode.map5 User
                (Decode.field "id" <| Decode.map UserId Decode.int)
                (Decode.field "email" Decode.string)
                (Decode.field "firstName" Decode.string)
                (Decode.field "lastName" Decode.string)
                (Decode.field "token" Decode.string)
                |> Decode.map Authorized
    in
        Decode.oneOf
            [ authorizedUserDecoder
            , Decode.succeed unauthorized
            ]
