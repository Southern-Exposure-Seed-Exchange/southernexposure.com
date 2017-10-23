module User
    exposing
        ( UserId(..)
        , AuthStatus(..)
        , User
        , unauthorized
        , decoder
        , storeDetails
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
    , authToken : String
    }


unauthorized : AuthStatus
unauthorized =
    Anonymous


decoder : Decoder AuthStatus
decoder =
    let
        authorizedUserDecoder =
            Decode.map3 User
                (Decode.field "id" <| Decode.map UserId Decode.int)
                (Decode.field "email" Decode.string)
                (Decode.field "token" Decode.string)
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
                Ports.storeAuthDetails ( user.authToken, id )
