module Api
    exposing
        ( Endpoint(..)
        , get
        , post
        , put
        , withToken
        , withJsonBody
        , withJsonResponse
        , withErrorHandler
        , sendRequest
        , toRequest
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
import Products.Pagination as Pagination
import Routing.Utils exposing (joinPath, withQueryStrings)


-- API ENDPOINTS


type Endpoint
    = NavigationData
    | CategoryDetails String Pagination.Data
    | ProductDetails String
    | ProductSearch Pagination.Data
    | PageDetails String
    | AdvancedSearchData
    | CustomerLogin
    | CustomerRegister
    | CustomerAuthorize
    | CustomerResetRequest
    | CustomerPasswordReset
    | CustomerLocations
    | CustomerContactDetails
    | CustomerEditLogin
    | CustomerEditContact
    | CartAddCustomer
    | CartDetailsCustomer
    | CartUpdateCustomer
    | CartAddAnonymous
    | CartDetailsAnonymous
    | CartUpdateAnonymous


toUrl : Endpoint -> String
toUrl endpoint =
    let
        endpointUrl =
            case endpoint of
                NavigationData ->
                    joinPath [ "categories", "nav" ]

                CategoryDetails slug data ->
                    joinPath [ "categories", "details", slug ]
                        ++ withQueryStrings [ Pagination.toQueryString data ]

                ProductDetails slug ->
                    joinPath [ "products", "details", slug ]

                ProductSearch data ->
                    joinPath [ "products", "search" ]
                        ++ withQueryStrings [ Pagination.toQueryString data ]

                PageDetails slug ->
                    joinPath [ "pages", "details", slug ]

                AdvancedSearchData ->
                    joinPath [ "categories", "search" ]

                CustomerLogin ->
                    joinPath [ "customers", "login" ]

                CustomerRegister ->
                    joinPath [ "customers", "register" ]

                CustomerAuthorize ->
                    joinPath [ "customers", "authorize" ]

                CustomerResetRequest ->
                    joinPath [ "customers", "reset-request" ]

                CustomerPasswordReset ->
                    joinPath [ "customers", "reset-password" ]

                CustomerLocations ->
                    joinPath [ "customers", "locations" ]

                CustomerContactDetails ->
                    joinPath [ "customers", "contact" ]

                CustomerEditLogin ->
                    joinPath [ "customers", "edit" ]

                CustomerEditContact ->
                    joinPath [ "customers", "contact-edit" ]

                CartAddCustomer ->
                    joinPath [ "carts", "customer", "add" ]

                CartDetailsCustomer ->
                    joinPath [ "carts", "customer", "details" ]

                CartUpdateCustomer ->
                    joinPath [ "carts", "customer", "update" ]

                CartAddAnonymous ->
                    joinPath [ "carts", "anonymous", "add" ]

                CartDetailsAnonymous ->
                    joinPath [ "carts", "anonymous", "details" ]

                CartUpdateAnonymous ->
                    joinPath [ "carts", "anonymous", "update" ]
    in
        "/api" ++ endpointUrl



-- REQUEST BUILDING


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


get : Endpoint -> Request String
get endpoint =
    { initialRequest | method = "GET", url = toUrl endpoint }


post : Endpoint -> Request String
post endpoint =
    { initialRequest | method = "POST", url = toUrl endpoint }


put : Endpoint -> Request String
put endpoint =
    { initialRequest | method = "PUT", url = toUrl endpoint }


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


toRequest : Request a -> Http.Request a
toRequest =
    Http.request



-- API VALIDATION ERROR RESPONSES


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
