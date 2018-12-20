module Main exposing (main)

import Address
import AdvancedSearch
import Api
import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.MyAccount as MyAccount
import Auth.ResetPassword as ResetPassword
import Browser
import Browser.Navigation
import Cart
import Checkout
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Locations
import Messages exposing (Msg(..))
import Model exposing (Model)
import PageData exposing (CartItemId(..), PageData, ProductData)
import Paginate exposing (Paginated)
import Ports
import Product exposing (ProductId(..), ProductVariantId(..))
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route(..), parseRoute, reverse)
import Search exposing (UniqueSearch(..))
import SeedAttribute exposing (SeedAttribute)
import SiteUI
import SiteUI.Search as SiteSearch
import StaticPage exposing (StaticPage)
import Task
import Time
import Update.Utils exposing (batchCommand, discardCommand, extraCommand, maybeCommand, noCommand, updateAndCommand, withCommand)
import Url exposing (Url)
import User exposing (AuthStatus, User)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions =
            Sub.batch
                [ Ports.loggedOut (always LogOut)
                , Ports.loggedIn OtherTabLoggedIn
                , Ports.newCartSessionToken OtherTabNewCartToken
                , Ports.cartItemCountChanged OtherTabCartItemCountChanged
                , Sub.map CheckoutMsg Checkout.subscriptions
                ]
                |> always
        , view = view
        , onUrlChange = parseRoute >> UrlUpdate
        , onUrlRequest = LinkClick
        }


type alias Flags =
    { authToken : Maybe String
    , authUserId : Maybe Int
    , cartSessionToken : Maybe String
    , cartItemCount : Maybe Int
    }



-- MODEL


init : Flags -> Url -> Routing.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            parseRoute url

        ( model, cmd ) =
            Model.initial key route
                |> (\m ->
                        { m
                            | maybeSessionToken = flags.cartSessionToken
                            , cartItemCount = Maybe.withDefault 0 flags.cartItemCount
                        }
                   )
                |> fetchDataForRoute
                |> Tuple.mapSecond
                    (\cmd_ ->
                        if route == Checkout && flags.authToken /= Nothing then
                            Cmd.none

                        else
                            cmd_
                    )

        authorizationCmd =
            Maybe.map2 reAuthorize flags.authUserId flags.authToken
                |> Maybe.withDefault (redirectIfAuthRequired key route)
    in
    ( model
    , Cmd.batch
        [ cmd
        , getNavigationData
        , authorizationCmd
        , Task.perform NewZone Time.here
        ]
    )



-- COMMANDS


{-| TODO: Move to PageData module?
-}
fetchDataForRoute : Model -> ( Model, Cmd Msg )
fetchDataForRoute ({ route, pageData, key } as model) =
    let
        updateCategoryDetails slug pagination products =
            products
                |> Paginate.updateData PageData.categoryConfig
                    { slug = slug, sorting = pagination.sorting }
                |> discardCommand (Paginate.updatePerPage PageData.categoryConfig pagination.perPage)
                |> discardCommand (Paginate.jumpTo PageData.categoryConfig pagination.page)

        ( data, cmd ) =
            case route of
                ProductDetails slug ->
                    ( { pageData | productDetails = RemoteData.Loading }
                    , getProductDetailsData slug
                    )

                CategoryDetails slug pagination ->
                    updateCategoryDetails slug pagination pageData.categoryDetails
                        |> Tuple.mapFirst (\cd -> { pageData | categoryDetails = cd })
                        |> Tuple.mapSecond (Cmd.map CategoryPaginationMsg)

                AdvancedSearch ->
                    ( { pageData | advancedSearch = RemoteData.Loading }
                    , getAdvancedSearchData
                    )

                SearchResults searchData pagination ->
                    pageData.searchResults
                        |> Paginate.updateData PageData.searchConfig
                            { data = searchData, sorting = pagination.sorting }
                        |> discardCommand (Paginate.updatePerPage PageData.searchConfig pagination.perPage)
                        |> discardCommand (Paginate.jumpTo PageData.searchConfig pagination.page)
                        |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                        |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)

                PageDetails slug ->
                    ( { pageData | pageDetails = RemoteData.Loading }
                    , getPageDetails slug
                    )

                CreateAccount ->
                    fetchLocationsOnce pageData

                CreateAccountSuccess ->
                    doNothing

                Login ->
                    doNothing

                ResetPassword _ ->
                    doNothing

                MyAccount ->
                    case model.currentUser of
                        User.Authorized user ->
                            { pageData | myAccount = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (MyAccount.getDetails user.authToken Nothing)

                        User.Anonymous ->
                            doNothing

                EditLogin ->
                    doNothing

                EditAddress ->
                    getAddressDetails model.currentUser pageData

                OrderDetails orderId ->
                    case model.currentUser of
                        User.Authorized user ->
                            { pageData | orderDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (getCheckoutSuccessDetails user.authToken orderId)

                        User.Anonymous ->
                            doNothing

                Cart ->
                    pageData
                        |> fetchCartDetails model.currentUser model.maybeSessionToken

                QuickOrder ->
                    doNothing

                Checkout ->
                    case model.currentUser of
                        User.Authorized user ->
                            { pageData | checkoutDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand
                                    (Checkout.getCustomerDetails GetCheckoutDetails
                                        user.authToken
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                    )

                        User.Anonymous ->
                            let
                                getDetails =
                                    case model.maybeSessionToken of
                                        Nothing ->
                                            Tuple.mapSecond (always <| Routing.newUrl key Cart)

                                        Just token ->
                                            batchCommand
                                                (Checkout.getAnonymousDetails GetCheckoutDetails
                                                    token
                                                    (Just <| .country Address.initial)
                                                    (Just <| .state Address.initial)
                                                    ""
                                                )
                            in
                            { pageData | checkoutDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> getDetails

                CheckoutSuccess orderId _ ->
                    case model.currentUser of
                        User.Authorized user ->
                            { pageData | orderDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (getCheckoutSuccessDetails user.authToken orderId)

                        User.Anonymous ->
                            doNothing

                NotFound ->
                    doNothing

        doNothing =
            ( pageData, Cmd.none )
    in
    ( { model | pageData = data }, cmd )


fetchLocationsOnce : PageData -> ( PageData, Cmd Msg )
fetchLocationsOnce pageData =
    case pageData.locations of
        RemoteData.Success _ ->
            ( pageData, Cmd.none )

        _ ->
            ( { pageData | locations = RemoteData.Loading }
            , getAddressLocations
            )


fetchCartDetails : AuthStatus -> Maybe String -> PageData -> ( PageData, Cmd Msg )
fetchCartDetails authStatus maybeSessionToken pageData =
    case authStatus of
        User.Anonymous ->
            ( { pageData | cartDetails = RemoteData.Loading }
            , getAnonymousCartDetails maybeSessionToken
            )

        User.Authorized user ->
            ( { pageData | cartDetails = RemoteData.Loading }
            , getCartDetails user.authToken
            )


getProductDetailsData : String -> Cmd Msg
getProductDetailsData slug =
    Api.get (Api.ProductDetails slug)
        |> Api.withJsonResponse PageData.productDetailsDecoder
        |> Api.sendRequest GetProductDetailsData


getNavigationData : Cmd Msg
getNavigationData =
    Api.get Api.NavigationData
        |> Api.withJsonResponse SiteUI.navigationDecoder
        |> Api.sendRequest GetNavigationData


getAdvancedSearchData : Cmd Msg
getAdvancedSearchData =
    Api.get Api.AdvancedSearchData
        |> Api.withJsonResponse PageData.advancedSearchDecoder
        |> Api.sendRequest GetAdvancedSearchData


getPageDetails : String -> Cmd Msg
getPageDetails slug =
    Api.get (Api.PageDetails slug)
        |> Api.withJsonResponse (Decode.field "page" StaticPage.decoder)
        |> Api.sendRequest GetPageDetailsData


getAddressLocations : Cmd Msg
getAddressLocations =
    Api.get Api.CustomerLocations
        |> Api.withJsonResponse Locations.addressLocationsDecoder
        |> Api.sendRequest GetAddressLocations


getAddressDetails : AuthStatus -> PageData -> ( PageData, Cmd Msg )
getAddressDetails authStatus pageData =
    case authStatus of
        User.Anonymous ->
            ( pageData, Cmd.none )

        User.Authorized user ->
            let
                getDetails pd =
                    ( { pd | addressDetails = RemoteData.Loading }
                    , detailsCmd
                    )

                detailsCmd =
                    Api.get Api.CustomerAddressDetails
                        |> Api.withJsonResponse PageData.addressDetailsDecoder
                        |> Api.withToken user.authToken
                        |> Api.sendRequest GetAddressDetails
            in
            fetchLocationsOnce pageData
                |> updateAndCommand getDetails


getCartDetails : String -> Cmd Msg
getCartDetails token =
    Api.get Api.CartDetailsCustomer
        |> Api.withToken token
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest GetCartDetails


getAnonymousCartDetails : Maybe String -> Cmd Msg
getAnonymousCartDetails maybeCartToken =
    let
        parameters =
            Encode.object
                [ ( "sessionToken", Encode.string <| Maybe.withDefault "" maybeCartToken ) ]
    in
    Api.post Api.CartDetailsAnonymous
        |> Api.withJsonBody parameters
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest GetCartDetails


reAuthorize : Int -> String -> Cmd Msg
reAuthorize userId token =
    let
        authParameters =
            Encode.object
                [ ( "userId", Encode.int userId )
                , ( "token", Encode.string token )
                ]
    in
    Api.post Api.CustomerAuthorize
        |> Api.withJsonBody authParameters
        |> Api.withJsonResponse User.decoder
        |> Api.sendRequest ReAuthorize


addToCustomerCart : String -> Int -> ProductVariantId -> Cmd Msg
addToCustomerCart token quantity (ProductVariantId variantId) =
    let
        body =
            Encode.object
                [ ( "variant", Encode.int variantId )
                , ( "quantity", Encode.int quantity )
                ]
    in
    Api.post Api.CartAddCustomer
        |> Api.withJsonBody body
        |> Api.withJsonResponse (Decode.succeed token)
        |> Api.withToken token
        |> Api.sendRequest (SubmitAddToCartResponse quantity)


addToAnonymousCart : Maybe String -> Int -> ProductVariantId -> Cmd Msg
addToAnonymousCart maybeSessionToken quantity (ProductVariantId variantId) =
    let
        body =
            Encode.object
                [ ( "variant", Encode.int variantId )
                , ( "quantity", Encode.int quantity )
                , ( "sessionToken", encodeMaybe Encode.string maybeSessionToken )
                ]

        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    Api.post Api.CartAddAnonymous
        |> Api.withJsonBody body
        |> Api.withStringResponse
        |> Api.sendRequest (SubmitAddToCartResponse quantity)


getCustomerCartItemsCount : String -> Cmd Msg
getCustomerCartItemsCount token =
    Api.get Api.CartCountCustomer
        |> Api.withJsonResponse (Decode.field "itemCount" Decode.int)
        |> Api.withToken token
        |> Api.sendRequest GetCartItemCount


getCheckoutSuccessDetails : String -> Int -> Cmd Msg
getCheckoutSuccessDetails token orderId =
    Api.post Api.CheckoutSuccess
        |> Api.withJsonBody (Encode.object [ ( "orderId", Encode.int orderId ) ])
        |> Api.withJsonResponse PageData.orderDetailsDecoder
        |> Api.withToken token
        |> Api.sendRequest GetCheckoutSuccessDetails



-- UPDATE


{-| TODO: Refactor pagedata messages into separate msg & update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ pageData, key } as model) =
    case msg of
        UrlUpdate route ->
            { model | route = route }
                |> fetchDataForRoute
                |> clearSearchForm
                |> extraCommand (always (Ports.collapseMobileMenus ()))

        NavigateTo route ->
            ( model, Routing.newUrl key route )

        LinkClick (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        LinkClick (Browser.Internal url) ->
            ( model, Routing.newUrl key <| parseRoute url )

        NewZone zone ->
            ( { model | zone = zone }, Cmd.none )

        LogOut ->
            let
                ( updatedModel, fetchCmd ) =
                    { model | currentUser = User.unauthorized, cartItemCount = 0 }
                        |> fetchDataForRoute
            in
            ( updatedModel
            , Cmd.batch
                [ redirectIfAuthRequired key model.route
                , Ports.removeAuthDetails ()
                , Ports.setCartItemCount 0
                , fetchCmd
                ]
            )

        OtherTabLoggedIn authData ->
            ( model, reAuthorize authData.userId authData.token )

        OtherTabNewCartToken cartSessionToken ->
            { model | maybeSessionToken = Just cartSessionToken }
                |> noCommand

        OtherTabCartItemCountChanged quantity ->
            let
                fetchCommand m =
                    case m.route of
                        Cart ->
                            fetchDataForRoute m

                        Checkout ->
                            fetchDataForRoute m

                        _ ->
                            ( m, Cmd.none )
            in
            { model | cartItemCount = quantity }
                |> fetchCommand

        ChangeCartFormVariantId productId variantId ->
            model
                |> updateCartVariant productId variantId
                |> noCommand

        ChangeCartFormQuantity productId quantity ->
            model
                |> updateCartQuantity productId quantity
                |> noCommand

        SubmitAddToCart (ProductId productId) defaultVariant ->
            let
                performRequest f =
                    ( model, f quantity variantId )

                ( variantId, quantity ) =
                    Dict.get productId model.addToCartForms
                        |> Maybe.withDefault { variant = Nothing, quantity = 1 }
                        |> (\v -> ( v.variant |> Maybe.withDefault defaultVariant, v.quantity ))
            in
            case model.currentUser of
                User.Authorized user ->
                    performRequest (addToCustomerCart user.authToken)

                User.Anonymous ->
                    performRequest (addToAnonymousCart model.maybeSessionToken)

        -- TODO: error/success alert
        SubmitAddToCartResponse quantity response ->
            case response of
                RemoteData.Success sessionToken ->
                    updateSessionTokenAndCartItemCount model quantity sessionToken

                _ ->
                    model |> noCommand

        ShowAllOrders ->
            case model.currentUser of
                User.Authorized user ->
                    ( model, MyAccount.getDetails user.authToken (Just 0) )

                User.Anonymous ->
                    ( model, Cmd.none )

        SearchMsg subMsg ->
            let
                ( searchData, cmd ) =
                    SiteSearch.update key subMsg model.searchData
            in
            ( { model | searchData = searchData }, cmd )

        AdvancedSearchMsg subMsg ->
            ( { model | advancedSearchData = AdvancedSearch.update subMsg model.advancedSearchData }
            , Cmd.none
            )

        CreateAccountMsg subMsg ->
            let
                ( updatedForm, maybeAuthStatus, cmd ) =
                    CreateAccount.update key subMsg model.createAccountForm model.maybeSessionToken
            in
            ( { model
                | createAccountForm = updatedForm
                , currentUser = maybeAuthStatus |> Maybe.withDefault model.currentUser
              }
            , Cmd.map CreateAccountMsg cmd
            )

        LoginMsg subMsg ->
            let
                ( updatedForm, maybeAuthStatus, cmd ) =
                    Login.update key subMsg model.loginForm model.maybeSessionToken

                cartItemsCommand =
                    case maybeAuthStatus of
                        Just (User.Authorized user) ->
                            getCustomerCartItemsCount user.authToken

                        _ ->
                            Cmd.none
            in
            ( { model
                | loginForm = updatedForm
                , currentUser = maybeAuthStatus |> Maybe.withDefault model.currentUser
              }
            , Cmd.batch [ Cmd.map LoginMsg cmd, cartItemsCommand ]
            )

        ResetPasswordMsg subMsg ->
            let
                cartItemsCommand maybeAuthStatus =
                    case maybeAuthStatus of
                        Just (User.Authorized user) ->
                            getCustomerCartItemsCount user.authToken

                        _ ->
                            Cmd.none
            in
            ResetPassword.update key subMsg model.resetPasswordForm model.maybeSessionToken
                |> (\( form, maybeAuthStatus, cmd ) ->
                        ( { model
                            | resetPasswordForm = form
                            , currentUser = maybeAuthStatus |> Maybe.withDefault model.currentUser
                          }
                        , Cmd.batch
                            [ Cmd.map ResetPasswordMsg cmd
                            , cartItemsCommand maybeAuthStatus
                            ]
                        )
                   )

        EditLoginMsg subMsg ->
            EditLogin.update key subMsg model.editLoginForm model.currentUser
                |> Tuple.mapFirst (\form -> { model | editLoginForm = form })
                |> Tuple.mapSecond (Cmd.map EditLoginMsg)

        EditAddressMsg subMsg ->
            EditAddress.update key subMsg model.editAddressForm model.currentUser pageData.addressDetails
                |> Tuple.mapFirst (\form -> { model | editAddressForm = form })
                |> Tuple.mapSecond (Cmd.map EditAddressMsg)

        EditCartMsg subMsg ->
            let
                updatedPageData =
                    Maybe.map RemoteData.Success
                        >> Maybe.withDefault pageData.cartDetails
                        >> (\cd -> { pageData | cartDetails = cd })

                updatedForm form =
                    Maybe.map Cart.fromCartDetails
                        >> Maybe.withDefault form
            in
            model.pageData.cartDetails
                |> RemoteData.withDefault PageData.blankCartDetails
                |> Cart.update subMsg model.currentUser model.maybeSessionToken model.editCartForm
                |> (\( form, maybeDetails, cmd ) ->
                        ( { model
                            | pageData = updatedPageData maybeDetails
                            , editCartForm = updatedForm form maybeDetails
                          }
                        , cmd
                        )
                            |> updateAndCommand (updateCartItemCountFromDetails maybeDetails)
                   )

        QuickOrderMsg subMsg ->
            QuickOrder.update subMsg model.quickOrderForms model.currentUser model.maybeSessionToken
                |> (\( forms, maybeQuantityAndToken, cmd ) ->
                        let
                            newQuantityAndToken m =
                                maybeQuantityAndToken
                                    |> Maybe.map (\( q, t ) -> updateSessionTokenAndCartItemCount m q t)
                                    |> Maybe.withDefault ( m, Cmd.none )
                        in
                        ( { model | quickOrderForms = forms }
                        , Cmd.batch
                            [ Cmd.map QuickOrderMsg cmd
                            , Maybe.map (always <| Routing.newUrl key Cart) maybeQuantityAndToken
                                |> Maybe.withDefault Cmd.none
                            ]
                        )
                            |> updateAndCommand newQuantityAndToken
                   )

        CheckoutMsg subMsg ->
            let
                handleOutMsg maybeMsg ( model_, cmd ) =
                    case maybeMsg of
                        Just (Checkout.CustomerOrderCompleted orderId) ->
                            ( { model_ | cartItemCount = 0 }
                            , Cmd.batch
                                [ cmd
                                , Routing.newUrl key <| CheckoutSuccess orderId False
                                , Ports.setCartItemCount 0
                                ]
                            )

                        Just (Checkout.AnonymousOrderCompleted orderId newAuthStatus) ->
                            ( { model_ | cartItemCount = 0, currentUser = newAuthStatus }
                            , Cmd.batch
                                [ cmd
                                , Routing.newUrl key <| CheckoutSuccess orderId True
                                , Ports.setCartItemCount 0
                                , Ports.removeCartSessionToken ()
                                , User.storeDetails newAuthStatus
                                ]
                            )

                        Just (Checkout.DetailsRefreshed checkoutDetails) ->
                            let
                                updatedPageData =
                                    { pageData
                                        | checkoutDetails =
                                            RemoteData.Success checkoutDetails
                                    }
                            in
                            ( { model_ | pageData = updatedPageData }
                            , cmd
                            )

                        Nothing ->
                            ( model_, cmd )
            in
            Checkout.update subMsg
                model.checkoutForm
                model.currentUser
                model.maybeSessionToken
                pageData.checkoutDetails
                |> (\( form, maybeOutMsg, cmd ) ->
                        ( { model | checkoutForm = form }
                        , Cmd.map CheckoutMsg cmd
                        )
                            |> handleOutMsg maybeOutMsg
                   )

        ReAuthorize response ->
            case response of
                RemoteData.Success authStatus ->
                    { model | currentUser = authStatus, maybeSessionToken = Nothing }
                        |> fetchDataForRoute

                RemoteData.Failure _ ->
                    ( { model | currentUser = User.Anonymous }
                    , Cmd.batch
                        [ Ports.removeAuthDetails ()
                        , redirectIfAuthRequired key model.route
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GetProductDetailsData response ->
            let
                updatedPageData =
                    { pageData | productDetails = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )
                |> extraCommand (always Ports.scrollToTop)

        -- TODO: Handle navigation loading errors - maybe w/ a global error list
        GetNavigationData response ->
            ( { model | navigationData = response }, Cmd.none )

        GetAdvancedSearchData response ->
            let
                updatedPageData =
                    { pageData | advancedSearch = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetPageDetailsData response ->
            let
                updatedPageData =
                    { pageData | pageDetails = response }
            in
            ( { model | pageData = updatedPageData }
            , Cmd.none
            )
                |> extraCommand (always Ports.scrollToTop)

        GetAddressLocations response ->
            let
                updatedPageData =
                    { pageData | locations = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetMyAccountDetails response ->
            let
                updatedPageData =
                    { pageData | myAccount = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAddressDetails response ->
            let
                updatedPageData =
                    { pageData | addressDetails = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetCartDetails response ->
            let
                updatedPageData =
                    { pageData | cartDetails = response }
            in
            { model | pageData = updatedPageData }
                |> resetEditCartForm response
                |> updateCartItemCountFromDetails (RemoteData.toMaybe response)

        GetCartItemCount response ->
            { model | cartItemCount = response |> RemoteData.toMaybe |> Maybe.withDefault 0 }
                |> withCommand (\m -> Ports.setCartItemCount m.cartItemCount)

        GetCheckoutDetails response ->
            let
                updatedPageData =
                    { pageData | checkoutDetails = response }

                cmd =
                    case response of
                        RemoteData.Success { items } ->
                            if List.isEmpty items then
                                Routing.newUrl key Cart

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            (\a -> Tuple.pair a cmd) <|
                case ( pageData.checkoutDetails, response ) of
                    ( RemoteData.Success _, RemoteData.Success _ ) ->
                        { model | pageData = updatedPageData }

                    ( _, RemoteData.Success details ) ->
                        { model
                            | pageData = updatedPageData
                            , checkoutForm =
                                Checkout.initialWithDefaults
                                    details.shippingAddresses
                                    details.billingAddresses
                        }

                    _ ->
                        { model | pageData = updatedPageData }

        GetCheckoutSuccessDetails response ->
            let
                updatedPageData =
                    { pageData | orderDetails = response }
            in
            { model | pageData = updatedPageData } |> noCommand

        CategoryPaginationMsg subMsg ->
            pageData.categoryDetails
                |> Paginate.update PageData.categoryConfig subMsg
                |> Tuple.mapSecond (Cmd.map CategoryPaginationMsg)
                |> (\( ps, cmd ) ->
                        ( ps, Cmd.batch [ cmd, updatePageFromPagination key model.route ps ] )
                   )
                |> Tuple.mapFirst (\cd -> { pageData | categoryDetails = cd })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> extraCommand (always Ports.scrollToTop)

        SearchPaginationMsg subMsg ->
            Paginate.update PageData.searchConfig subMsg pageData.searchResults
                |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)
                |> (\( sr, cmd ) ->
                        ( sr, Cmd.batch [ cmd, updatePageFromPagination key model.route sr ] )
                   )
                |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> extraCommand (always Ports.scrollToTop)


updatePageFromPagination : Routing.Key -> Route -> Paginated a b c -> Cmd msg
updatePageFromPagination key route paginated =
    let
        ( maybePage, newRouteConstructor ) =
            case route of
                CategoryDetails slug pagination ->
                    ( Just pagination.page, \p -> CategoryDetails slug { pagination | page = p } )

                SearchResults data pagination ->
                    ( Just pagination.page, \p -> SearchResults data { pagination | page = p } )

                _ ->
                    ( Nothing, always route )

        newPage =
            Paginate.getPage paginated
    in
    case maybePage of
        Nothing ->
            Cmd.none

        Just page ->
            if page == newPage then
                Cmd.none

            else
                Routing.newUrl key <| newRouteConstructor newPage


clearSearchForm : ( Model, Cmd msg ) -> ( Model, Cmd msg )
clearSearchForm ( model, cmd ) =
    (\a -> Tuple.pair a cmd) <|
        case model.route of
            AdvancedSearch ->
                { model | searchData = Search.initial }

            SearchResults _ _ ->
                model

            _ ->
                { model
                    | searchData = Search.initial
                    , advancedSearchData = Search.initial
                }


updateCartQuantity : ProductId -> Int -> Model -> Model
updateCartQuantity (ProductId productId) quantity model =
    let
        addToCartForms =
            Dict.update productId updateForm model.addToCartForms

        updateForm maybeForm =
            case maybeForm of
                Nothing ->
                    Just { variant = Nothing, quantity = quantity }

                Just v ->
                    Just { v | quantity = quantity }
    in
    { model | addToCartForms = addToCartForms }


updateCartVariant : ProductId -> ProductVariantId -> Model -> Model
updateCartVariant (ProductId productId) variantId model =
    let
        addToCartForms =
            Dict.update productId updateForm model.addToCartForms

        updateForm maybeForm =
            case maybeForm of
                Nothing ->
                    Just { variant = Just variantId, quantity = 1 }

                Just v ->
                    Just { v | variant = Just variantId }
    in
    { model | addToCartForms = addToCartForms }


resetEditCartForm : WebData PageData.CartDetails -> Model -> Model
resetEditCartForm response model =
    case response of
        RemoteData.Success details ->
            { model | editCartForm = Cart.fromCartDetails details }

        _ ->
            model


updateCartItemCountFromDetails : Maybe PageData.CartDetails -> Model -> ( Model, Cmd Msg )
updateCartItemCountFromDetails maybeCartDetails model =
    case maybeCartDetails of
        Nothing ->
            ( model, Cmd.none )

        Just cartDetails ->
            let
                itemCount =
                    List.foldl (.quantity >> (+)) 0 cartDetails.items
            in
            ( { model | cartItemCount = itemCount }
            , Ports.setCartItemCount itemCount
            )


updateSessionTokenAndCartItemCount : Model -> Int -> String -> ( Model, Cmd msg )
updateSessionTokenAndCartItemCount model quantity sessionToken =
    if String.isEmpty sessionToken then
        { model | cartItemCount = model.cartItemCount + quantity }
            |> withCommand (\m -> Ports.setCartItemCount m.cartItemCount)

    else if Just sessionToken /= model.maybeSessionToken then
        ( { model
            | maybeSessionToken = Just sessionToken
            , cartItemCount = quantity
          }
        , Cmd.batch
            [ Ports.storeCartSessionToken sessionToken
            , Ports.setCartItemCount quantity
            ]
        )

    else
        ( { model | cartItemCount = model.cartItemCount + quantity }
        , Ports.setCartItemCount (model.cartItemCount + quantity)
        )


redirectIfAuthRequired : Routing.Key -> Route -> Cmd msg
redirectIfAuthRequired key route =
    if Routing.authRequired route then
        Routing.newUrl key <| PageDetails "home"

    else
        Cmd.none
