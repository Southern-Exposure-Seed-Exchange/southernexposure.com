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
import Categories.AdminViews as CategoryAdmin
import Category exposing (CategoryId)
import Checkout
import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Locations
import Messages exposing (Msg(..))
import Model exposing (Model)
import PageData exposing (CartItemId(..), PageData)
import Paginate exposing (Paginated)
import Ports
import Product exposing (ProductId(..), ProductVariantId(..))
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..), parseRoute)
import Search exposing (UniqueSearch(..))
import SiteUI
import SiteUI.Search as SiteSearch
import StaticPage exposing (StaticPageId)
import Task
import Time
import Update.Utils exposing (batchCommand, discardCommand, extraCommand, noCommand, updateAndCommand, withCommand)
import Url exposing (Url)
import User exposing (AuthStatus)
import View exposing (view)
import Views.StaticPageAdmin as StaticPageAdmin


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
                , Time.every (60 * 60 * 1000) (always <| UpdateZone)
                ]
                |> always
        , view = view
        , onUrlChange = parseRoute >> UrlUpdate
        , onUrlRequest = LinkClick
        }


type alias Flags =
    { authUserId : Maybe Int
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
                        if route == Checkout && flags.authUserId /= Nothing then
                            Cmd.none

                        else
                            cmd_
                    )

        authorizationCmd =
            Maybe.map reAuthorize flags.authUserId
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
                    doNothing

                CreateAccountSuccess ->
                    doNothing

                Login _ ->
                    doNothing

                ResetPassword _ ->
                    doNothing

                MyAccount ->
                    case model.currentUser of
                        User.Authorized _ ->
                            { pageData | myAccount = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (MyAccount.getDetails Nothing)

                        User.Anonymous ->
                            doNothing

                EditLogin ->
                    doNothing

                EditAddress ->
                    getAddressDetails model.currentUser pageData

                OrderDetails orderId ->
                    case model.currentUser of
                        User.Authorized _ ->
                            { pageData | orderDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (getCheckoutSuccessDetails orderId)

                        User.Anonymous ->
                            doNothing

                Cart ->
                    pageData
                        |> fetchCartDetails model.currentUser model.maybeSessionToken

                QuickOrder ->
                    doNothing

                Checkout ->
                    case model.currentUser of
                        User.Authorized _ ->
                            { pageData | checkoutDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand
                                    (Checkout.getCustomerDetails GetCheckoutDetails
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        ""
                                        False
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
                                                    ""
                                                    False
                                                )
                            in
                            { pageData | checkoutDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> getDetails

                CheckoutSuccess orderId _ ->
                    case model.currentUser of
                        User.Authorized _ ->
                            { pageData | orderDetails = RemoteData.Loading }
                                |> fetchLocationsOnce
                                |> batchCommand (getCheckoutSuccessDetails orderId)

                        User.Anonymous ->
                            ( pageData, redirectIfAuthRequired key model.route )

                Admin Dashboard ->
                    doNothing

                Admin CategoryList ->
                    ( { pageData | adminCategoryList = RemoteData.Loading }
                    , getAdminCategoryList
                    )

                Admin CategoryNew ->
                    ( { pageData | adminNewCategory = RemoteData.Loading }
                    , getAdminNewCategoryData
                    )

                Admin (CategoryEdit cId) ->
                    ( { pageData
                        | adminEditCategory = RemoteData.Loading
                        , adminNewCategory = RemoteData.Loading
                      }
                    , Cmd.batch [ getAdminEditCategoryData cId, getAdminNewCategoryData ]
                    )

                Admin PageList ->
                    ( { pageData | adminPageList = RemoteData.Loading }
                    , getAdminPageList
                    )

                Admin PageNew ->
                    doNothing

                Admin (PageEdit pageId) ->
                    ( { pageData | adminEditPage = RemoteData.Loading }
                    , getAdminEditPageData pageId
                    )

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

        User.Authorized _ ->
            ( { pageData | cartDetails = RemoteData.Loading }
            , getCartDetails
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

        User.Authorized _ ->
            let
                getDetails pd =
                    ( { pd | addressDetails = RemoteData.Loading }
                    , detailsCmd
                    )

                detailsCmd =
                    Api.get Api.CustomerAddressDetails
                        |> Api.withJsonResponse PageData.addressDetailsDecoder
                        |> Api.sendRequest GetAddressDetails
            in
            fetchLocationsOnce pageData
                |> updateAndCommand getDetails


getCartDetails : Cmd Msg
getCartDetails =
    Api.get Api.CartDetailsCustomer
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


reAuthorize : Int -> Cmd Msg
reAuthorize userId =
    let
        authParameters =
            Encode.object [ ( "userId", Encode.int userId ) ]
    in
    Api.post Api.CustomerAuthorize
        |> Api.withJsonBody authParameters
        |> Api.withJsonResponse User.decoder
        |> Api.sendRequest ReAuthorize


addToCustomerCart : Int -> ProductVariantId -> Cmd Msg
addToCustomerCart quantity (ProductVariantId variantId) =
    let
        body =
            Encode.object
                [ ( "variant", Encode.int variantId )
                , ( "quantity", Encode.int quantity )
                ]
    in
    Api.post Api.CartAddCustomer
        |> Api.withJsonBody body
        |> Api.withJsonResponse (Decode.succeed "")
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


getCustomerCartItemsCount : Cmd Msg
getCustomerCartItemsCount =
    Api.get Api.CartCountCustomer
        |> Api.withJsonResponse (Decode.field "itemCount" Decode.int)
        |> Api.sendRequest GetCartItemCount


getCheckoutSuccessDetails : Int -> Cmd Msg
getCheckoutSuccessDetails orderId =
    Api.post Api.CheckoutSuccess
        |> Api.withJsonBody (Encode.object [ ( "orderId", Encode.int orderId ) ])
        |> Api.withJsonResponse PageData.orderDetailsDecoder
        |> Api.sendRequest GetCheckoutSuccessDetails


logOut : Cmd Msg
logOut =
    Api.post Api.CustomerLogout
        |> Api.withJsonResponse (Decode.succeed ())
        |> Api.sendRequest LogOutResponse


getAdminCategoryList : Cmd Msg
getAdminCategoryList =
    Api.get Api.AdminCategoryList
        |> Api.withJsonResponse PageData.adminCategoryListDataDecoder
        |> Api.sendRequest GetAdminCategoryList


getAdminNewCategoryData : Cmd Msg
getAdminNewCategoryData =
    Api.get Api.AdminNewCategory
        |> Api.withJsonResponse PageData.adminNewCategoryDataDecoder
        |> Api.sendRequest GetAdminNewCategoryData


getAdminEditCategoryData : CategoryId -> Cmd Msg
getAdminEditCategoryData cId =
    Api.get (Api.AdminEditCategoryData cId)
        |> Api.withJsonResponse PageData.adminEditCategoryDataDecoder
        |> Api.sendRequest GetAdminEditCategoryData


getAdminPageList : Cmd Msg
getAdminPageList =
    Api.get Api.AdminPageList
        |> Api.withJsonResponse PageData.adminPageListDataDecoder
        |> Api.sendRequest GetAdminPageList


getAdminEditPageData : StaticPageId -> Cmd Msg
getAdminEditPageData pageId =
    Api.get (Api.AdminEditPageData pageId)
        |> Api.withJsonResponse PageData.adminEditPageDataDecoder
        |> Api.sendRequest GetAdminEditPageData



-- UPDATE


{-| TODO: Refactor pagedata messages into separate msg & update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ pageData, key } as model) =
    case msg of
        UrlUpdate route ->
            { model | route = route }
                |> resetForm model.route
                |> fetchDataForRoute
                |> clearSearchForm
                |> extraCommand (always (Ports.collapseMobileMenus ()))

        NavigateTo route ->
            ( model, Routing.newUrl key route )

        LinkClick (Browser.External url) ->
            ( model, Browser.Navigation.load url )

        LinkClick (Browser.Internal url) ->
            if url.path /= "/account/logout/" then
                ( model, Routing.newUrl key <| parseRoute url )

            else
                ( model, Cmd.none )

        NewZone zone ->
            ( { model | zone = zone }, Cmd.none )

        UpdateZone ->
            ( model, Task.perform NewZone Time.here )

        LogOut ->
            ( model, logOut )

        OtherTabLoggedIn userId ->
            ( model, reAuthorize userId )

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
                User.Authorized _ ->
                    performRequest addToCustomerCart

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
                User.Authorized _ ->
                    ( model, MyAccount.getDetails (Just 0) )

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
                        Just (User.Authorized _) ->
                            getCustomerCartItemsCount

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
                        Just (User.Authorized _) ->
                            getCustomerCartItemsCount

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
                        , Cmd.map EditCartMsg cmd
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

        NewCategoryMsg subMsg ->
            CategoryAdmin.updateNewForm model.key subMsg model.newCategoryForm
                |> Tuple.mapFirst (\form -> { model | newCategoryForm = form })
                |> Tuple.mapSecond (Cmd.map NewCategoryMsg)

        EditCategoryMsg subMsg ->
            CategoryAdmin.updateEditForm model.key pageData.adminEditCategory subMsg model.editCategoryForm
                |> Tuple.mapFirst (\form -> { model | editCategoryForm = form })
                |> Tuple.mapSecond (Cmd.map EditCategoryMsg)

        NewPageMsg subMsg ->
            StaticPageAdmin.updateNewForm model.key subMsg model.newPageForm
                |> Tuple.mapFirst (\form -> { model | newPageForm = form })
                |> Tuple.mapSecond (Cmd.map NewPageMsg)

        EditPageMsg subMsg ->
            StaticPageAdmin.updateEditForm model.key pageData.adminEditPage subMsg model.editPageForm
                |> Tuple.mapFirst (\form -> { model | editPageForm = form })
                |> Tuple.mapSecond (Cmd.map EditPageMsg)

        ReAuthorize response ->
            case response of
                RemoteData.Success authStatus ->
                    let
                        baseUpdate =
                            { model | currentUser = authStatus, maybeSessionToken = Nothing }
                                |> fetchDataForRoute
                    in
                    case model.route of
                        Login (Just "") ->
                            baseUpdate
                                |> batchCommand (Routing.newUrl key <| PageDetails "home")

                        Login (Just newUrl) ->
                            baseUpdate
                                |> batchCommand (Browser.Navigation.pushUrl key newUrl)

                        Login Nothing ->
                            baseUpdate
                                |> batchCommand (Routing.newUrl key MyAccount)

                        _ ->
                            baseUpdate

                RemoteData.Failure _ ->
                    ( { model | currentUser = User.Anonymous }
                    , Cmd.batch
                        [ Ports.removeAuthDetails ()
                        , redirectIfAuthRequired key model.route
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        LogOutResponse response ->
            case response of
                RemoteData.Success _ ->
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

                _ ->
                    -- TODO: Couldn't log user out - show error toast/message
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
            { model | pageData = updatedPageData }
                |> withCommand (redirect403IfAnonymous key response)

        GetAddressDetails response ->
            let
                updatedPageData =
                    { pageData | addressDetails = response }
            in
            { model | pageData = updatedPageData }
                |> withCommand (redirect403IfAnonymous key response)

        GetCartDetails response ->
            let
                updatedPageData =
                    { pageData | cartDetails = response }
            in
            { model | pageData = updatedPageData }
                |> resetEditCartForm response
                |> updateCartItemCountFromDetails (RemoteData.toMaybe response)
                |> extraCommand (redirect403IfAnonymous key response)

        GetCartItemCount response ->
            { model | cartItemCount = response |> RemoteData.toMaybe |> Maybe.withDefault 0 }
                |> withCommand (\m -> Ports.setCartItemCount m.cartItemCount)

        GetCheckoutDetails response ->
            let
                updatedPageData =
                    case response of
                        RemoteData.Success (Ok det) ->
                            { pageData | checkoutDetails = RemoteData.Success det }

                        _ ->
                            pageData

                cmd =
                    case response of
                        RemoteData.Success (Ok { items }) ->
                            if List.isEmpty items then
                                Routing.newUrl key Cart

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            extraCommand (redirect403IfAnonymous key response) <|
                (\a -> Tuple.pair a cmd) <|
                    case ( pageData.checkoutDetails, response ) of
                        ( RemoteData.Success _, RemoteData.Success (Ok _) ) ->
                            { model | pageData = updatedPageData }

                        ( _, RemoteData.Success (Ok details) ) ->
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
            { model | pageData = updatedPageData }
                |> withCommand (redirect403IfAnonymous key response)

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

        GetAdminCategoryList response ->
            let
                updatedPageData =
                    { pageData | adminCategoryList = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminNewCategoryData response ->
            let
                updatedPageData =
                    { pageData | adminNewCategory = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminEditCategoryData response ->
            let
                updatedPageData =
                    { pageData | adminEditCategory = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminPageList response ->
            let
                updatedPageData =
                    { pageData | adminPageList = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminEditPageData response ->
            let
                updatedPageData =
                    { pageData | adminEditPage = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )


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


{-| Reset any forms from the previous route
-}
resetForm : Route -> Model -> Model
resetForm oldRoute model =
    let
        resetAdminForm adminRoute =
            case adminRoute of
                Dashboard ->
                    model

                CategoryList ->
                    model

                CategoryNew ->
                    { model | newCategoryForm = CategoryAdmin.initialNewForm }

                CategoryEdit _ ->
                    { model | editCategoryForm = CategoryAdmin.initialEditForm }

                PageList ->
                    model

                PageNew ->
                    { model | newPageForm = StaticPageAdmin.initialNewForm }

                PageEdit _ ->
                    { model | editPageForm = StaticPageAdmin.initialEditForm }
    in
    case oldRoute of
        ProductDetails _ ->
            model

        CategoryDetails _ _ ->
            model

        AdvancedSearch ->
            { model | advancedSearchData = Search.initial }

        SearchResults _ _ ->
            model

        PageDetails _ ->
            model

        CreateAccount ->
            { model | createAccountForm = CreateAccount.initial }

        CreateAccountSuccess ->
            model

        Login _ ->
            { model | loginForm = Login.initial }

        ResetPassword _ ->
            { model | resetPasswordForm = ResetPassword.initial }

        MyAccount ->
            model

        EditLogin ->
            { model | editLoginForm = EditLogin.initial }

        EditAddress ->
            { model | editAddressForm = EditAddress.initial }

        OrderDetails _ ->
            model

        Cart ->
            { model | editCartForm = Cart.initial }

        QuickOrder ->
            { model | quickOrderForms = QuickOrder.initial }

        Checkout ->
            { model | checkoutForm = Checkout.initial }

        CheckoutSuccess _ _ ->
            model

        Admin adminRoute ->
            resetAdminForm adminRoute

        NotFound ->
            model


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
        Routing.newUrl key <| Login <| Just <| Routing.reverse route

    else
        Cmd.none


redirect403IfAnonymous : Routing.Key -> RemoteData.WebData a -> { m | currentUser : AuthStatus, route : Route } -> Cmd Msg
redirect403IfAnonymous key response { currentUser, route } =
    case ( response, currentUser ) of
        ( RemoteData.Failure (Http.BadStatus 403), User.Anonymous ) ->
            Routing.newUrl key <| Login <| Just <| Routing.reverse route

        _ ->
            Cmd.none
