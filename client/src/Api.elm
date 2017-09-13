module Api
    exposing
        ( get
        , post
        , put
        , withToken
        , withJsonBody
        , withJsonResponse
        , withErrorHandler
        , sendRequest
        , FormErrors
        , initialErrors
        , formErrorsDecoder
        , addError
        )

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import RemoteData
import Time


type alias Request a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time.Time
    , withCredentials : Bool
    }


initialRequest : Request String
initialRequest =
    { method = "GET"
    , headers = []
    , url = ""
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }


get : String -> Request String
get url =
    { initialRequest | method = "GET", url = url }


post : String -> Request String
post url =
    { initialRequest | method = "POST", url = url }


put : String -> Request String
put url =
    { initialRequest | method = "PUT", url = url }


withToken : String -> Request a -> Request a
withToken token request =
    { request | headers = Http.header "Auth-Token" token :: request.headers }


withJsonBody : Value -> Request a -> Request a
withJsonBody body request =
    { request | body = Http.jsonBody body }


withJsonResponse : Decoder a -> Request b -> Request a
withJsonResponse decoder request =
    { request | expect = Http.expectJson decoder }


withErrorHandler : (RemoteData.WebData (Result FormErrors a) -> msg) -> Request a -> Cmd msg
withErrorHandler msg request =
    let
        errorHandler response =
            case response of
                RemoteData.Failure (Http.BadStatus rawResponse) ->
                    if rawResponse.status.code == validationErrorCode then
                        case Decode.decodeString formErrorsDecoder rawResponse.body of
                            Ok errors ->
                                RemoteData.Success <| Err errors

                            Err _ ->
                                RemoteData.Failure <| Http.BadStatus rawResponse
                    else
                        RemoteData.Failure <| Http.BadStatus rawResponse

                RemoteData.Failure error ->
                    RemoteData.Failure error

                RemoteData.Success data ->
                    RemoteData.Success <| Ok data

                RemoteData.Loading ->
                    RemoteData.Loading

                RemoteData.NotAsked ->
                    RemoteData.NotAsked
    in
        request
            |> Http.request
            |> RemoteData.sendRequest
            |> Cmd.map (errorHandler >> msg)


sendRequest : (RemoteData.WebData a -> msg) -> Request a -> Cmd msg
sendRequest msg =
    Http.request >> RemoteData.sendRequest >> Cmd.map msg



-- Request Body Validation Errors


validationErrorCode : Int
validationErrorCode =
    422


type alias Field =
    String


type alias ErrorMessage =
    String


type alias FormErrors =
    Dict Field (List ErrorMessage)


initialErrors : FormErrors
initialErrors =
    Dict.empty


formErrorsDecoder : Decoder FormErrors
formErrorsDecoder =
    Decode.dict <| Decode.list Decode.string


addError : Field -> ErrorMessage -> FormErrors -> FormErrors
addError field message errors =
    flip (Dict.update field) errors <|
        \val ->
            case val of
                Nothing ->
                    Just [ message ]

                Just es ->
                    Just <| message :: es
