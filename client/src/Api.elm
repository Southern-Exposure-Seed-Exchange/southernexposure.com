module Api exposing
    ( Endpoint(..)
    , FormErrors
    , addError
    , apiFailureToError
    , delete
    , errorHtml
    , formErrorsDecoder
    , generalFormErrors
    , get
    , getErrorHtml
    , initialErrors
    , patch
    , post
    , put
    , sendRequest
    , withErrorHandler
    , withJsonBody
    , withJsonResponse
    , withStringErrorHandler
    , withStringResponse
    )

import Category exposing (CategoryId(..))
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Products.Pagination as Pagination
import RemoteData exposing (WebData)
import Routing.Utils exposing (joinPath, queryParameter, withQueryStrings)
import StaticPage exposing (StaticPageId)



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
    | CustomerLogout
    | CustomerResetRequest
    | CustomerPasswordReset
    | CustomerLocations
    | CustomerMyAccount (Maybe Int)
    | CustomerEditLogin
    | CustomerAddressDetails
    | CustomerEditAddress Int
    | CustomerDeleteAddress Int
    | CartAddCustomer
    | CartDetailsCustomer
    | CartUpdateCustomer
    | CartCountCustomer
    | CartQuickOrderCustomer
    | CartAddAnonymous
    | CartDetailsAnonymous
    | CartUpdateAnonymous
    | CartQuickOrderAnonymous
    | CheckoutDetailsCustomer
    | CheckoutPlaceOrderCustomer
    | CheckoutDetailsAnonymous
    | CheckoutPlaceOrderAnonymous
    | CheckoutSuccess
    | AdminCategoryList
    | AdminNewCategory
    | AdminEditCategoryData CategoryId
    | AdminEditCategory
    | AdminPageList
    | AdminNewPage
    | AdminEditPageData StaticPageId
    | AdminEditPage
    | AdminOrderList Int Int String


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

                CustomerLogout ->
                    joinPath [ "customers", "logout" ]

                CustomerResetRequest ->
                    joinPath [ "customers", "reset-request" ]

                CustomerPasswordReset ->
                    joinPath [ "customers", "reset-password" ]

                CustomerLocations ->
                    joinPath [ "customers", "locations" ]

                CustomerMyAccount Nothing ->
                    joinPath [ "customers", "my-account" ]

                CustomerMyAccount (Just limit) ->
                    joinPath [ "customers", "my-account" ]
                        ++ withQueryStrings
                            [ queryParameter ( "limit", String.fromInt limit ) ]

                CustomerEditLogin ->
                    joinPath [ "customers", "edit" ]

                CustomerAddressDetails ->
                    joinPath [ "customers", "addresses" ]

                CustomerEditAddress id ->
                    joinPath [ "customers", "address-edit", String.fromInt id ]

                CustomerDeleteAddress id ->
                    joinPath [ "customers", "address-delete", String.fromInt id ]

                CartAddCustomer ->
                    joinPath [ "carts", "customer", "add" ]

                CartDetailsCustomer ->
                    joinPath [ "carts", "customer", "details" ]

                CartUpdateCustomer ->
                    joinPath [ "carts", "customer", "update" ]

                CartCountCustomer ->
                    joinPath [ "carts", "customer", "count" ]

                CartQuickOrderCustomer ->
                    joinPath [ "carts", "customer", "quick-order" ]

                CartAddAnonymous ->
                    joinPath [ "carts", "anonymous", "add" ]

                CartDetailsAnonymous ->
                    joinPath [ "carts", "anonymous", "details" ]

                CartUpdateAnonymous ->
                    joinPath [ "carts", "anonymous", "update" ]

                CartQuickOrderAnonymous ->
                    joinPath [ "carts", "anonymous", "quick-order" ]

                CheckoutDetailsCustomer ->
                    joinPath [ "checkout", "customer-details" ]

                CheckoutPlaceOrderCustomer ->
                    joinPath [ "checkout", "customer-place-order" ]

                CheckoutDetailsAnonymous ->
                    joinPath [ "checkout", "anonymous-details" ]

                CheckoutPlaceOrderAnonymous ->
                    joinPath [ "checkout", "anonymous-place-order" ]

                CheckoutSuccess ->
                    joinPath [ "checkout", "success" ]

                AdminCategoryList ->
                    joinPath [ "admin", "categories", "list" ]

                AdminNewCategory ->
                    joinPath [ "admin", "categories", "new" ]

                AdminEditCategoryData (CategoryId cId) ->
                    joinPath [ "admin", "categories", "edit", String.fromInt cId ]

                AdminEditCategory ->
                    joinPath [ "admin", "categories", "edit" ]

                AdminPageList ->
                    joinPath [ "admin", "pages", "list" ]

                AdminNewPage ->
                    joinPath [ "admin", "pages", "new" ]

                AdminEditPageData pageId ->
                    joinPath [ "admin", "pages", "edit", StaticPage.idToString pageId ]

                AdminEditPage ->
                    joinPath [ "admin", "pages", "edit" ]

                AdminOrderList page perPage query ->
                    joinPath [ "admin", "orders", "list" ]
                        ++ withQueryStrings
                            [ queryParameter ( "page", String.fromInt page )
                            , queryParameter ( "perPage", String.fromInt perPage )
                            , queryParameter ( "query", query )
                            ]
    in
    "/api" ++ endpointUrl



-- REQUEST BUILDING


type alias Request a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    }


initialRequest : Request (Maybe String)
initialRequest =
    { method = "GET"
    , headers = []
    , url = ""
    , body = Http.emptyBody
    , expect = Http.expectString Result.toMaybe
    , timeout = Nothing
    , tracker = Nothing
    }


initialMethod : String -> Endpoint -> Request (Maybe String)
initialMethod method endpoint =
    { initialRequest | method = method, url = toUrl endpoint }


get : Endpoint -> Request (Maybe String)
get =
    initialMethod "GET"


post : Endpoint -> Request (Maybe String)
post =
    initialMethod "POST"


put : Endpoint -> Request (Maybe String)
put =
    initialMethod "PUT"


patch : Endpoint -> Request (Maybe String)
patch =
    initialMethod "PATCH"


delete : Endpoint -> Request (Maybe String)
delete =
    initialMethod "DELETE"


withJsonBody : Value -> Request a -> Request a
withJsonBody body request =
    { request | body = Http.jsonBody body }


withJsonResponse : Decoder a -> Request b -> Request (WebData a)
withJsonResponse decoder request =
    { method = request.method
    , headers = request.headers
    , url = request.url
    , body = request.body
    , expect = Http.expectJson RemoteData.fromResult decoder
    , timeout = request.timeout
    , tracker = request.tracker
    }


withStringResponse : Request b -> Request (WebData String)
withStringResponse request =
    { method = request.method
    , headers = request.headers
    , url = request.url
    , body = request.body
    , expect = Http.expectString RemoteData.fromResult
    , timeout = request.timeout
    , tracker = request.tracker
    }


withErrorHandler : Decoder a -> Request b -> Request (WebData (Result FormErrors a))
withErrorHandler decoder request =
    let
        expectDecoded =
            errorHandler <|
                \body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| Http.BadBody (Decode.errorToString err)
    in
    { method = request.method
    , headers = request.headers
    , url = request.url
    , body = request.body
    , expect = expectDecoded
    , timeout = request.timeout
    , tracker = request.tracker
    }


withStringErrorHandler : Request a -> Request (WebData (Result FormErrors String))
withStringErrorHandler request =
    { method = request.method
    , headers = request.headers
    , url = request.url
    , body = request.body
    , expect = errorHandler Ok
    , timeout = request.timeout
    , tracker = request.tracker
    }


errorHandler :
    (String -> Result Http.Error a)
    -> Http.Expect (WebData (Result FormErrors a))
errorHandler bodyFunction =
    Http.expectStringResponse RemoteData.fromResult <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| Http.BadUrl url

                Http.Timeout_ ->
                    Err <| Http.Timeout

                Http.NetworkError_ ->
                    Err <| Http.NetworkError

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == validationErrorCode then
                        case Decode.decodeString formErrorsDecoder body of
                            Ok errors ->
                                Ok <| Err errors

                            Err _ ->
                                Err <| Http.BadStatus metadata.statusCode

                    else
                        Err <| Http.BadStatus metadata.statusCode

                Http.GoodStatus_ _ body ->
                    Result.map Ok <| bodyFunction body


sendRequest : (a -> msg) -> Request a -> Cmd msg
sendRequest msg =
    Http.request >> Cmd.map msg



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
    (\a -> Dict.update field a errors) <|
        \val ->
            case val of
                Nothing ->
                    Just [ message ]

                Just es ->
                    Just <| message :: es


{-| Render a field's error messages in red text.
-}
getErrorHtml : Field -> FormErrors -> Html.Html msg
getErrorHtml field errors =
    Dict.get field errors
        |> Maybe.map errorHtml
        |> Maybe.withDefault (Html.text "")


{-| Render a list of error messages in red text.
-}
errorHtml : List ErrorMessage -> Html.Html msg
errorHtml =
    List.map Html.text
        >> List.intersperse (Html.br [] [])
        >> Html.div [ class "text-danger" ]


generalFormErrors : { model | errors : FormErrors } -> Html.Html msg
generalFormErrors =
    .errors >> getErrorHtml ""


{-| Transform an HTTP Error into a FormError.
-}
apiFailureToError : Http.Error -> FormErrors
apiFailureToError error =
    let
        errorMessage =
            case error of
                Http.BadStatus code ->
                    if code == 404 then
                        "The form submission URL is incorrect. Please contact us if this error continues."

                    else if code == 403 then
                        "You do not have permission to submit this form. Please contact us if you think this is an error."

                    else if code >= 500 && code < 600 then
                        "The server encountered an error while processing your request. Please try again or contact us if it continues."

                    else
                        "We encountered an unexpected response code(" ++ String.fromInt code ++ ") while processing the server response. Please try again or contact us if it continues."

                Http.BadBody message ->
                    "We encountered an error while processing the request from the server: " ++ message

                Http.BadUrl url ->
                    "We tried submitting the form to an invalid URL: " ++ url

                Http.Timeout ->
                    "The server took too long to send us a reply. Please try again or contact us for help."

                Http.NetworkError ->
                    "We were unable to establish a connection to our server. Please verify your internet connection and try again."
    in
    initialErrors |> addError "" errorMessage
