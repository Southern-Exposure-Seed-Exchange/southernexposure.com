module Data.User exposing
    ( AuthStatus(..)
    , User
    , UserId(..)
    , decoder
    , isAdmin
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
    , isAdmin : Bool
    }


unauthorized : AuthStatus
unauthorized =
    Anonymous


isAdmin : AuthStatus -> Bool
isAdmin auth =
    case auth of
        Anonymous ->
            False

        Authorized user ->
            user.isAdmin


decoder : Decoder AuthStatus
decoder =
    let
        authorizedUserDecoder =
            Decode.map3 User
                (Decode.field "id" <| Decode.map UserId Decode.int)
                (Decode.field "email" Decode.string)
                (Decode.field "isAdmin" Decode.bool)
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
