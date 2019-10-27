module User exposing
    ( AuthStatus(..)
    , User
    , UserId(..)
    , decoder
    , storeDetails
    , unauthorized
    )

import Json.Decode as Decode exposing (Decoder)
import Ports


type UserId
    = UserId Int


type AuthStatus
    = Anonymous
    | Authorized User


type alias User =
    { id : UserId
    , email : String
    }


unauthorized : AuthStatus
unauthorized =
    Anonymous


decoder : Decoder AuthStatus
decoder =
    let
        authorizedUserDecoder =
            Decode.map2 User
                (Decode.field "id" <| Decode.map UserId Decode.int)
                (Decode.field "email" Decode.string)
                |> Decode.map Authorized
    in
    Decode.oneOf
        [ authorizedUserDecoder
        , Decode.succeed unauthorized
        ]


storeDetails : AuthStatus -> Cmd msg
storeDetails authStatus =
    case authStatus of
        Anonymous ->
            Cmd.none

        Authorized user ->
            let
                (UserId id) =
                    user.id
            in
            Ports.storeAuthDetails id
