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
import Model exposing (CartForms, Model)
import PageData exposing (CartItemId(..), PageData)
import Paginate exposing (Paginated)
import Ports
import Process
import Product exposing (ProductId(..), ProductVariantId(..))
import Products.AdminViews as ProductAdmin
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
import Views.CouponAdmin as CouponAdmin
import Views.CustomerAdmin as CustomerAdmin
import Views.OrderAdmin as OrderAdmin
import Views.StaticPageAdmin as StaticPageAdmin


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = updateWrapper
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

        waitForAuthorization =
            Routing.authRequired route || (route == Checkout && flags.authUserId /= Nothing)

        ( model, cmd ) =
            Model.initial key route
                |> (\m ->
                        { m
                            | maybeSessionToken = flags.cartSessionToken
                            , cartItemCount = Maybe.withDefault 0 flags.cartItemCount
                        }
                   )
                |> (\m ->
                        if waitForAuthorization then
                            ( m, Cmd.none )

                        else
                            fetchDataForRoute m
                   )

        authorizationCmd =
            Maybe.map reAuthorize flags.authUserId
                |> Maybe.withDefault (redirectIfAuthRequired key route)

        metadataCmd =
            if pageLoadCompleted model then
                Ports.updatePageMetadata
                    { url = Routing.reverse route
                    , title = View.pageTitle model
                    , description = View.pageDescription model
                    , image = View.pageImage model
                    }

            else
                Cmd.none
    in
    ( model
    , Cmd.batch
        [ cmd
        , getNavigationData
        , authorizationCmd
        , Task.perform NewZone Time.here
        , metadataCmd
        ]
    )



-- COMMANDS


{-| TODO: Move to PageData module?
-}
fetchDataForRoute : Model -> ( Model, Cmd Msg )
fetchDataForRoute ({ route, pageData, key } as model) =
    let
        updateCategoryDetails slug pagination products =
            let
                updater =
                    if slug == (.slug <| Paginate.getRequestData products) then
                        Paginate.updateData

                    else
                        Paginate.updateAndResetData
            in
            products
                |> updater PageData.categoryConfig
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

                PageDetails slug _ ->
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

                Admin (OrderList { page, perPage, query }) ->
                    pageData.adminOrderList
                        |> Paginate.updateData PageData.ordersConfig query
                        |> discardCommand (Paginate.updatePerPage PageData.ordersConfig perPage)
                        |> discardCommand (Paginate.jumpTo PageData.ordersConfig page)
                        |> Tuple.mapFirst (\ol -> { pageData | adminOrderList = ol })
                        |> Tuple.mapSecond (Cmd.map AdminOrderPaginationMsg)
                        |> updateAndCommand fetchLocationsOnce

                Admin (AdminOrderDetails orderId) ->
                    { pageData | adminOrderDetails = RemoteData.Loading }
                        |> fetchLocationsOnce
                        |> batchCommand (getAdminOrderDetails orderId)

                Admin (CustomerList { page, perPage, query }) ->
                    pageData.adminCustomerList
                        |> Paginate.updateData PageData.customersConfig query
                        |> discardCommand (Paginate.updatePerPage PageData.customersConfig perPage)
                        |> discardCommand (Paginate.jumpTo PageData.customersConfig page)
                        |> Tuple.mapFirst (\cl -> { pageData | adminCustomerList = cl })
                        |> Tuple.mapSecond (Cmd.map AdminCustomerPaginationMsg)

                Admin (CustomerEdit customerId) ->
                    ( { pageData | adminEditCustomer = RemoteData.Loading }
                    , getAdminEditCustomerData customerId
                    )

                Admin ProductList ->
                    ( { pageData | adminProductList = RemoteData.Loading }
                    , getAdminProductList
                    )

                Admin ProductNew ->
                    ( { pageData | adminSharedProduct = RemoteData.Loading }
                    , getAdminSharedProductData
                    )

                Admin (ProductEdit productId) ->
                    ( { pageData | adminSharedProduct = RemoteData.Loading }
                    , Cmd.batch
                        [ getAdminSharedProductData
                        , getAdminEditProductData productId
                        ]
                    )

                Admin CouponList ->
                    ( { pageData | adminCouponList = RemoteData.Loading }
                    , getAdminCouponList
                    )

                Admin CouponNew ->
                    doNothing

                Admin (CouponEdit couponId) ->
                    ( { pageData | adminEditCoupon = RemoteData.Loading }
                    , getAdminEditCouponData couponId
                    )

                Redirect path ->
                    ( pageData
                    , Browser.Navigation.load path
                    )

                NotFound ->
                    doNothing

        doNothing =
            ( pageData, Cmd.none )

        -- For routes whose data fetching escapes the scope of `pageData`.
        updatedModel =
            case route of
                Admin (ProductEdit _) ->
                    let
                        newEditForm =
                            { productData = RemoteData.Loading, id = Nothing }
                    in
                    { model | editProductForm = newEditForm }

                _ ->
                    model
    in
    ( { updatedModel | pageData = data }, cmd )


fetchLocationsOnce : PageData -> ( PageData, Cmd Msg )
fetchLocationsOnce pageData =
    case pageData.locations of
        RemoteData.Success _ ->
            ( pageData, Cmd.none )

        RemoteData.Loading ->
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


addToCustomerCart : CartForms -> ProductId -> Int -> ProductVariantId -> ( CartForms, Cmd Msg )
addToCustomerCart forms pId quantity ((ProductVariantId variantId) as vId) =
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
        |> Api.sendRequest (SubmitAddToCartResponse pId quantity)
        |> setCartFormToLoading forms quantity pId vId


addToAnonymousCart : Maybe String -> CartForms -> ProductId -> Int -> ProductVariantId -> ( CartForms, Cmd Msg )
addToAnonymousCart maybeSessionToken forms pId quantity ((ProductVariantId variantId) as vId) =
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
        |> Api.sendRequest (SubmitAddToCartResponse pId quantity)
        |> setCartFormToLoading forms quantity pId vId


setCartFormToLoading : CartForms -> Int -> ProductId -> ProductVariantId -> Cmd Msg -> ( CartForms, Cmd Msg )
setCartFormToLoading forms quantity (ProductId pId) (ProductVariantId vId) cmd =
    let
        newForms =
            Dict.update pId updateForm forms

        updateForm maybeForm =
            case maybeForm of
                Nothing ->
                    Just
                        { variant = Just <| ProductVariantId vId
                        , quantity = quantity
                        , requestStatus = RemoteData.Loading
                        }

                Just v ->
                    Just { v | requestStatus = RemoteData.Loading }
    in
    ( newForms, cmd )


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


getAdminOrderDetails : Int -> Cmd Msg
getAdminOrderDetails orderId =
    Api.get (Api.AdminOrderDetails orderId)
        |> Api.withJsonResponse PageData.adminOrderDetailsDecoder
        |> Api.sendRequest GetAdminOrderDetails


getAdminEditCustomerData : Int -> Cmd Msg
getAdminEditCustomerData customerId =
    Api.get (Api.AdminEditCustomerData customerId)
        |> Api.withJsonResponse PageData.adminEditCustomerDataDecoder
        |> Api.sendRequest GetAdminEditCustomerData


getAdminProductList : Cmd Msg
getAdminProductList =
    Api.get Api.AdminProductList
        |> Api.withJsonResponse PageData.adminProductListDataDecoder
        |> Api.sendRequest GetAdminProductList


getAdminSharedProductData : Cmd Msg
getAdminSharedProductData =
    Api.get Api.AdminProductSharedData
        |> Api.withJsonResponse PageData.adminNewProductDataDecoder
        |> Api.sendRequest GetAdminSharedProductData


getAdminEditProductData : ProductId -> Cmd Msg
getAdminEditProductData productId =
    let
        decoder =
            Decode.map2 Tuple.pair
                ProductAdmin.formDecoder
                (Decode.field "id" Product.idDecoder)
    in
    Api.get (Api.AdminEditProductData productId)
        |> Api.withJsonResponse decoder
        |> Api.sendRequest GetAdminEditProductData


getAdminCouponList : Cmd Msg
getAdminCouponList =
    Api.get Api.AdminCouponList
        |> Api.withJsonResponse PageData.adminCouponListDataDecoder
        |> Api.sendRequest GetAdminCouponList


getAdminEditCouponData : Int -> Cmd Msg
getAdminEditCouponData couponId =
    Api.get (Api.AdminEditCouponData couponId)
        |> Api.withJsonResponse PageData.adminEditCouponDataDecoder
        |> Api.sendRequest GetAdminEditCouponData



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

        SubmitAddToCart ((ProductId productId) as pId) defaultVariant ->
            let
                performRequest f =
                    let
                        ( newForms, cmd ) =
                            f model.addToCartForms pId quantity variantId
                    in
                    ( { model | addToCartForms = newForms }, cmd )

                ( variantId, quantity ) =
                    Dict.get productId model.addToCartForms
                        |> Maybe.withDefault { variant = Nothing, quantity = 1, requestStatus = RemoteData.NotAsked }
                        |> (\v -> ( v.variant |> Maybe.withDefault defaultVariant, v.quantity ))
            in
            case model.currentUser of
                User.Authorized _ ->
                    performRequest addToCustomerCart

                User.Anonymous ->
                    performRequest (addToAnonymousCart model.maybeSessionToken)

        SubmitAddToCartResponse productId quantity response ->
            case response of
                RemoteData.Success sessionToken ->
                    updateSessionTokenAndCartItemCount model quantity sessionToken
                        |> updateCartFormRequestStatus productId response

                _ ->
                    model
                        |> noCommand
                        |> updateCartFormRequestStatus productId response

        ResetCartFormStatus (ProductId productId) ->
            let
                updatedForms =
                    Dict.update productId updateForm model.addToCartForms

                updateForm maybeForm =
                    case maybeForm of
                        Nothing ->
                            Just
                                { variant = Nothing
                                , quantity = 1
                                , requestStatus = RemoteData.NotAsked
                                }

                        Just v ->
                            Just { v | requestStatus = RemoteData.NotAsked }
            in
            { model | addToCartForms = updatedForms }
                |> noCommand

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

        OrderSearchMsg subMsg ->
            OrderAdmin.updateSearchForm key subMsg model.orderSearchForm
                |> Tuple.mapFirst (\form -> { model | orderSearchForm = form })
                |> Tuple.mapSecond (Cmd.map OrderSearchMsg)

        OrderDetailsMsg subMsg ->
            OrderAdmin.updateDetailsForm key pageData.adminOrderDetails subMsg model.orderDetailsForm
                |> Tuple.mapFirst (\form -> { model | orderDetailsForm = form })
                |> Tuple.mapSecond (Cmd.map OrderDetailsMsg)

        CustomerSearchMsg subMsg ->
            CustomerAdmin.updateSearchForm key subMsg model.customerSearchForm
                |> Tuple.mapFirst (\form -> { model | customerSearchForm = form })
                |> Tuple.mapSecond (Cmd.map CustomerSearchMsg)

        EditCustomerMsg subMsg ->
            CustomerAdmin.updateEditForm key pageData.adminEditCustomer subMsg model.editCustomerForm
                |> Tuple.mapFirst (\form -> { model | editCustomerForm = form })
                |> Tuple.mapSecond (Cmd.map EditCustomerMsg)

        ProductListMsg subMsg ->
            ProductAdmin.updateListForm subMsg model.productListForm
                |> (\form -> { model | productListForm = form })
                |> noCommand

        NewProductMsg subMsg ->
            ProductAdmin.updateNewForm key subMsg model.newProductForm
                |> Tuple.mapFirst (\form -> { model | newProductForm = form })
                |> Tuple.mapSecond (Cmd.map NewProductMsg)

        EditProductMsg subMsg ->
            ProductAdmin.updateEditForm key subMsg model.editProductForm
                |> Tuple.mapFirst (\form -> { model | editProductForm = form })
                |> Tuple.mapSecond (Cmd.map EditProductMsg)

        NewCouponMsg subMsg ->
            CouponAdmin.updateNewForm key subMsg model.newCouponForm
                |> Tuple.mapFirst (\form -> { model | newCouponForm = form })
                |> Tuple.mapSecond (Cmd.map NewCouponMsg)

        EditCouponMsg subMsg ->
            CouponAdmin.updateEditForm key pageData.adminEditCoupon subMsg model.editCouponForm
                |> Tuple.mapFirst (\form -> { model | editCouponForm = form })
                |> Tuple.mapSecond (Cmd.map EditCouponMsg)

        ReAuthorize response ->
            case response of
                RemoteData.Success authStatus ->
                    let
                        updatedModel =
                            { model | currentUser = authStatus, maybeSessionToken = Nothing }

                        baseUpdate =
                            if Routing.authRequired model.route || List.member model.route [ Cart, Checkout ] then
                                fetchDataForRoute updatedModel

                            else
                                ( updatedModel, Cmd.none )
                    in
                    case model.route of
                        Login (Just "") ->
                            baseUpdate
                                |> batchCommand (Routing.newUrl key Routing.homePage)

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

                scrollCmd =
                    case model.route of
                        PageDetails _ Nothing ->
                            Ports.scrollToTop

                        _ ->
                            Cmd.none
            in
            ( { model | pageData = updatedPageData }
            , scrollCmd
            )

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

        AdminOrderPaginationMsg subMsg ->
            pageData.adminOrderList
                |> Paginate.update PageData.ordersConfig subMsg
                |> Tuple.mapSecond (Cmd.map AdminOrderPaginationMsg)
                |> (\( ol, cmd ) ->
                        ( ol, Cmd.batch [ cmd, updatePageFromPagination key model.route ol ] )
                   )
                |> Tuple.mapFirst (\ol -> { pageData | adminOrderList = ol })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> extraCommand (always Ports.scrollToTop)

        GetAdminOrderDetails response ->
            let
                updatedPageData =
                    { pageData | adminOrderDetails = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        AdminCustomerPaginationMsg subMsg ->
            pageData.adminCustomerList
                |> Paginate.update PageData.customersConfig subMsg
                |> Tuple.mapSecond (Cmd.map AdminCustomerPaginationMsg)
                |> (\( cl, cmd ) ->
                        ( cl, Cmd.batch [ cmd, updatePageFromPagination key model.route cl ] )
                   )
                |> Tuple.mapFirst (\cl -> { pageData | adminCustomerList = cl })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> extraCommand (always Ports.scrollToTop)

        GetAdminEditCustomerData response ->
            let
                updatedPageData =
                    { pageData | adminEditCustomer = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminProductList response ->
            let
                updatedPageData =
                    { pageData | adminProductList = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminSharedProductData response ->
            let
                updatedPageData =
                    { pageData | adminSharedProduct = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminEditProductData response ->
            let
                newEditForm =
                    case response of
                        RemoteData.Success ( productData, productId ) ->
                            { productData = RemoteData.Success productData
                            , id = Just productId
                            }

                        _ ->
                            { productData = RemoteData.map Tuple.first response
                            , id = Nothing
                            }
            in
            ( { model | editProductForm = newEditForm }
            , Cmd.none
            )

        GetAdminCouponList response ->
            let
                updatedPageData =
                    { pageData | adminCouponList = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )

        GetAdminEditCouponData response ->
            let
                updatedPageData =
                    { pageData | adminEditCoupon = response }
            in
            ( { model | pageData = updatedPageData }, Cmd.none )


{-| Wrap the normal update function, checking to see if the page has finished
loading all it's dependent data. If so, run ports that should fire once a page
has loaded. Otherwise do nothing.
-}
updateWrapper : Msg -> Model -> ( Model, Cmd Msg )
updateWrapper msg model =
    let
        ( newModel, cmd ) =
            update msg model

        noLoadNeededOrLoadJustFinished =
            (not (pageLoadCompleted model) && pageLoadCompleted newModel)
                || (model.route /= newModel.route && pageLoadCompleted newModel)

        pageDetailsScroll =
            case newModel.route of
                PageDetails _ (Just fragment) ->
                    Ports.scrollToName fragment

                _ ->
                    Cmd.none

        statusCodeCmd =
            getStatusCode newModel
                |> Maybe.map Ports.logStatusCode
                |> Maybe.withDefault Cmd.none
    in
    if noLoadNeededOrLoadJustFinished then
        ( newModel
        , Cmd.batch
            [ Ports.updatePageMetadata
                { url = Routing.reverse newModel.route
                , title = View.pageTitle newModel
                , description = View.pageDescription newModel
                , image = View.pageImage newModel
                }
            , pageDetailsScroll
            , statusCodeCmd
            , cmd
            ]
        )

    else
        ( newModel, cmd )


{-| Check if all of the current route's data has been fetched successfully.
-}
pageLoadCompleted : Model -> Bool
pageLoadCompleted =
    runOnCurrentWebData
        (Maybe.map (\wd -> RemoteData.isSuccess wd || RemoteData.isFailure wd)
            >> Maybe.withDefault True
        )


{-| Get the status code of the current route's responses.
-}
getStatusCode : Model -> Maybe Int
getStatusCode =
    let
        checkNotFound : (Model -> Maybe Int) -> Model -> Maybe Int
        checkNotFound f model =
            if model.route == NotFound then
                Just 404

            else
                f model

        webDataToCode : WebData () -> Maybe Int
        webDataToCode webData =
            case webData of
                RemoteData.NotAsked ->
                    Nothing

                RemoteData.Loading ->
                    Nothing

                RemoteData.Success _ ->
                    Just 200

                RemoteData.Failure (Http.BadStatus code) ->
                    Just code

                RemoteData.Failure Http.Timeout ->
                    Just 408

                RemoteData.Failure (Http.BadUrl _) ->
                    Just 400

                RemoteData.Failure (Http.BadBody _) ->
                    Just 422

                RemoteData.Failure Http.NetworkError ->
                    Just 503
    in
    checkNotFound <|
        runOnCurrentWebData
            (Maybe.withDefault (RemoteData.Success ()) >> webDataToCode)


{-| Run some generic transforming function on the current route's page data.
-}
runOnCurrentWebData : (Maybe (WebData ()) -> b) -> Model -> b
runOnCurrentWebData runner { pageData, route } =
    let
        paginateRunner =
            Paginate.getRemoteData >> RemoteData.map (always ()) >> Just >> runner

        justRunner =
            RemoteData.map (always ()) >> Just >> runner

        nothingRunner =
            runner Nothing
    in
    case route of
        ProductDetails _ ->
            justRunner pageData.productDetails

        CategoryDetails _ _ ->
            paginateRunner pageData.categoryDetails

        AdvancedSearch ->
            justRunner pageData.advancedSearch

        SearchResults _ _ ->
            paginateRunner pageData.searchResults

        PageDetails _ _ ->
            justRunner pageData.pageDetails

        CreateAccount ->
            nothingRunner

        CreateAccountSuccess ->
            nothingRunner

        Login _ ->
            nothingRunner

        ResetPassword _ ->
            nothingRunner

        MyAccount ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.myAccount
                |> justRunner

        EditLogin ->
            nothingRunner

        EditAddress ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.addressDetails
                |> justRunner

        OrderDetails _ ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                |> justRunner

        Cart ->
            justRunner pageData.cartDetails

        QuickOrder ->
            nothingRunner

        Checkout ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.checkoutDetails
                |> justRunner

        CheckoutSuccess _ _ ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.orderDetails
                |> justRunner

        Admin Dashboard ->
            nothingRunner

        Admin CategoryList ->
            justRunner pageData.adminCategoryList

        Admin CategoryNew ->
            justRunner pageData.adminNewCategory

        Admin (CategoryEdit _) ->
            RemoteData.map2 Tuple.pair pageData.adminNewCategory pageData.adminEditCategory
                |> justRunner

        Admin PageList ->
            justRunner pageData.adminPageList

        Admin PageNew ->
            nothingRunner

        Admin (PageEdit _) ->
            justRunner pageData.adminEditPage

        Admin (OrderList _) ->
            justRunner pageData.locations

        Admin (AdminOrderDetails _) ->
            RemoteData.map2 Tuple.pair pageData.locations pageData.adminOrderDetails
                |> justRunner

        Redirect _ ->
            nothingRunner

        NotFound ->
            nothingRunner

        Admin (CustomerList _) ->
            paginateRunner pageData.adminCustomerList

        Admin (CustomerEdit _) ->
            justRunner pageData.adminEditCustomer

        Admin ProductList ->
            justRunner pageData.adminProductList

        Admin ProductNew ->
            justRunner pageData.adminSharedProduct

        Admin (ProductEdit _) ->
            justRunner pageData.adminSharedProduct

        Admin CouponList ->
            justRunner pageData.adminCouponList

        Admin CouponNew ->
            nothingRunner

        Admin (CouponEdit _) ->
            justRunner pageData.adminEditCoupon


{-| Update the current page number using the Paginated data.

This redirects users to the last page of the paginated data if they have
reached a page that is beyond the pagination(e.g., by manually setting the page
query parameter to a number above the page count).

-}
updatePageFromPagination : Routing.Key -> Route -> Paginated a b c -> Cmd msg
updatePageFromPagination key route paginated =
    let
        ( maybePage, newRouteConstructor ) =
            case route of
                CategoryDetails slug pagination ->
                    ( Just pagination.page, \p -> CategoryDetails slug { pagination | page = p } )

                SearchResults data pagination ->
                    ( Just pagination.page, \p -> SearchResults data { pagination | page = p } )

                Admin (OrderList params) ->
                    ( Just params.page, \p -> Admin <| OrderList { params | page = p } )

                Admin (CustomerList params) ->
                    ( Just params.page, \p -> Admin <| CustomerList { params | page = p } )

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

                OrderList _ ->
                    { model | orderSearchForm = OrderAdmin.initialSearchForm }

                AdminOrderDetails _ ->
                    { model | orderDetailsForm = OrderAdmin.initialDetailsForm }

                CustomerList _ ->
                    { model | customerSearchForm = CustomerAdmin.initialSearchForm }

                CustomerEdit _ ->
                    { model | editCustomerForm = CustomerAdmin.initialEditForm }

                ProductList ->
                    { model | productListForm = ProductAdmin.initialListForm }

                ProductNew ->
                    { model | newProductForm = ProductAdmin.initialNewForm }

                ProductEdit _ ->
                    { model | editProductForm = ProductAdmin.initialEditForm }

                CouponList ->
                    model

                CouponNew ->
                    { model | newCouponForm = CouponAdmin.initialNewForm }

                CouponEdit _ ->
                    { model | editCouponForm = CouponAdmin.initialEditForm }
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

        PageDetails _ _ ->
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

        Redirect _ ->
            model

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
                    Just
                        { variant = Nothing
                        , quantity = quantity
                        , requestStatus = RemoteData.NotAsked
                        }

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
                    Just
                        { variant = Just variantId
                        , quantity = 1
                        , requestStatus = RemoteData.NotAsked
                        }

                Just v ->
                    Just { v | variant = Just variantId }
    in
    { model | addToCartForms = addToCartForms }


updateCartFormRequestStatus : ProductId -> WebData a -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateCartFormRequestStatus (ProductId productId) response ( { addToCartForms } as model, cmd ) =
    let
        unitResponse =
            RemoteData.map (always ()) response

        updatedForms =
            Dict.update productId updateForm addToCartForms

        updateForm maybeForm =
            case maybeForm of
                Nothing ->
                    Just
                        { variant = Nothing
                        , quantity = 1
                        , requestStatus = unitResponse
                        }

                Just v ->
                    Just { v | requestStatus = unitResponse }

        resetStatusCmd =
            case response of
                RemoteData.Loading ->
                    Cmd.none

                RemoteData.NotAsked ->
                    Cmd.none

                _ ->
                    Process.sleep (10 * 1000)
                        |> Task.andThen (always <| Task.succeed <| ProductId productId)
                        |> Task.perform ResetCartFormStatus
    in
    ( { model | addToCartForms = updatedForms }, Cmd.batch [ cmd, resetStatusCmd ] )


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
