module Api exposing
    ( Endpoint(..), get, post, put, patch, delete
    , withJsonBody, withStringResponse, withJsonResponse, withStringErrorHandler, withErrorHandler
    , sendRequest
    , FormErrors, initialErrors, addError, prefixedArrayErrors
    , apiFailureToError, formErrorsDecoder
    , errorHtml, generalFormErrors, getErrorHtml
    )

{-|


# Requests

@docs Endpoint, get, post, put, patch, delete
@docs withJsonBody, withStringResponse, withJsonResponse, withStringErrorHandler, withErrorHandler
@docs sendRequest


# Errors

@docs FormErrors, initialErrors, addError, prefixedArrayErrors
@docs apiFailureToError, formErrorsDecoder
@docs errorHtml, generalFormErrors, getErrorHtml

-}

import Category exposing (CategoryId(..))
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Product exposing (ProductId(..))
import Products.Pagination as Pagination
import RemoteData exposing (WebData)
import Routing.Utils exposing (joinPath, queryFlag, queryParameter, withQueryStrings)
import StaticPage exposing (StaticPageId)



-- API ENDPOINTS


type Endpoint
    = NavigationData
    | CategoryDetails String Pagination.Data
    | ProductDetails String
    | ProductSearch Pagination.Data
    | PageDetails String
    | CategorySearchData
    | CustomerLogin Bool
    | CustomerRegister
    | CustomerVerifyEmail String
    | CustomerRequestVerification Int
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
    | GuestCheckoutSuccess
    | CheckoutHelcimToken
    | CheckoutHelcimTokenAnonymous
    | AdminCategoryList
    | AdminNewCategory
    | AdminEditCategoryData CategoryId
    | AdminEditCategory
    | AdminPageList
    | AdminNewPage
    | AdminEditPageData StaticPageId
    | AdminEditPage
    | AdminOrderList Int Int String
    | AdminOrderDetails Int
    | AdminOrderComment
    | AdminOrderRefund
    | AdminCustomerList Int Int String
    | AdminEditCustomerData Int
    | AdminEditCustomer
    | AdminDeleteCustomer Int
    | AdminProductList
    | AdminProductSharedData
    | AdminNewProduct
    | AdminEditProductData ProductId
    | AdminEditProduct
    | AdminCouponList
    | AdminNewCoupon
    | AdminEditCouponData Int
    | AdminEditCoupon
    | AdminSurchargesData
    | AdminSurcharges
    | AdminShippingData
    | AdminShipping
    | AdminSettingsData
    | AdminSettings
    | AdminProductSaleList
    | AdminProductSaleNew
    | AdminEditProductSaleData Int
    | AdminEditProductSale
    | AdminCategorySaleList
    | AdminNewCategorySale
    | AdminEditCategorySaleData Int
    | AdminEditCategorySale
    | AdminDashboardReports


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

                CategorySearchData ->
                    joinPath [ "categories", "search" ]

                CustomerLogin clearCart ->
                    joinPath [ "customers", "login" ]
                        ++ withQueryStrings [ queryFlag "clearCart" clearCart ]

                CustomerRegister ->
                    joinPath [ "customers", "register" ]

                CustomerVerifyEmail uuid  ->
                    joinPath [ "customers", "verify", uuid ]

                CustomerRequestVerification customerId ->
                    joinPath [ "customers", "request-verification"]
                        ++ withQueryStrings [queryParameter ( "customer", String.fromInt customerId)]

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

                GuestCheckoutSuccess ->
                    joinPath [ "checkout", "anonymous-success" ]

                CheckoutHelcimToken ->
                    joinPath [ "checkout", "helcim-checkout-token" ]

                CheckoutHelcimTokenAnonymous ->
                    joinPath [ "checkout", "anonymous-helcim-checkout-token" ]

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

                AdminOrderDetails orderId ->
                    joinPath [ "admin", "orders", "details", String.fromInt orderId ]

                AdminOrderComment ->
                    joinPath [ "admin", "orders", "comment" ]

                AdminOrderRefund ->
                    joinPath [ "admin", "orders", "refund" ]

                AdminCustomerList page perPage query ->
                    joinPath [ "admin", "customers", "list" ]
                        ++ withQueryStrings
                            [ queryParameter ( "page", String.fromInt page )
                            , queryParameter ( "perPage", String.fromInt perPage )
                            , queryParameter ( "query", query )
                            ]

                AdminEditCustomerData customerId ->
                    joinPath [ "admin", "customers", "edit", String.fromInt customerId ]

                AdminEditCustomer ->
                    joinPath [ "admin", "customers", "edit" ]

                AdminDeleteCustomer customerId ->
                    joinPath [ "admin", "customers", "delete", String.fromInt customerId ]

                AdminProductList ->
                    joinPath [ "admin", "products", "list" ]

                AdminProductSharedData ->
                    joinPath [ "admin", "products", "data" ]

                AdminNewProduct ->
                    joinPath [ "admin", "products", "new" ]

                AdminEditProductData (ProductId pId) ->
                    joinPath [ "admin", "products", "edit", String.fromInt pId ]

                AdminEditProduct ->
                    joinPath [ "admin", "products", "edit" ]

                AdminCouponList ->
                    joinPath [ "admin", "coupons", "list" ]

                AdminNewCoupon ->
                    joinPath [ "admin", "coupons", "new" ]

                AdminEditCouponData cId ->
                    joinPath [ "admin", "coupons", "edit", String.fromInt cId ]

                AdminEditCoupon ->
                    joinPath [ "admin", "coupons", "edit" ]

                AdminSurchargesData ->
                    joinPath [ "admin", "surcharges", "data" ]

                AdminSurcharges ->
                    joinPath [ "admin", "surcharges", "update" ]

                AdminShippingData ->
                    joinPath [ "admin", "shipping", "data" ]

                AdminShipping ->
                    joinPath [ "admin", "shipping", "update" ]

                AdminSettingsData ->
                    joinPath [ "admin", "settings", "data" ]

                AdminSettings ->
                    joinPath [ "admin", "settings", "update" ]

                AdminProductSaleList ->
                    joinPath [ "admin", "product-sales", "list" ]

                AdminProductSaleNew ->
                    joinPath [ "admin", "product-sales", "new" ]

                AdminEditProductSaleData psId ->
                    joinPath [ "admin", "product-sales", "edit", String.fromInt psId ]

                AdminEditProductSale ->
                    joinPath [ "admin", "product-sales", "edit" ]

                AdminCategorySaleList ->
                    joinPath [ "admin", "category-sales", "list" ]

                AdminNewCategorySale ->
                    joinPath [ "admin", "category-sales", "new" ]

                AdminEditCategorySaleData csId ->
                    joinPath [ "admin", "category-sales", "edit", String.fromInt csId ]

                AdminEditCategorySale ->
                    joinPath [ "admin", "category-sales", "edit" ]

                AdminDashboardReports ->
                    joinPath [ "admin", "dashboard", "reports" ]
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


{-| Filter the FormErrors, keeping only errors for the given prefix & array
index. The prefix is stripped from the remaining field names.
-}
prefixedArrayErrors : String -> Int -> FormErrors -> FormErrors
prefixedArrayErrors prefix index errs =
    let
        fullPrefix =
            prefix ++ "-" ++ String.fromInt index

        prefixLength =
            String.length fullPrefix

        isArrayError field =
            String.startsWith fullPrefix field

        stripPrefix field =
            if String.startsWith (fullPrefix ++ "-") field then
                String.dropLeft (prefixLength + 1) field

            else
                String.dropLeft prefixLength field
    in
    Dict.foldl
        (\k v acc ->
            if isArrayError k then
                Dict.insert (stripPrefix k) v acc

            else
                acc
        )
        Dict.empty
        errs


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
