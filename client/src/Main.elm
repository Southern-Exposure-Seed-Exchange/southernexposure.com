module Main exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import AdvancedSearch
import Api
import Auth.CreateAccount as CreateAccount
import Auth.EditContact as EditContact
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Messages exposing (Msg(..))
import Model exposing (Model)
import PageData exposing (PageData, ProductData)
import Ports
import Routing exposing (Route(..), reverse, parseRoute)
import Search exposing (UniqueSearch(..))
import SeedAttribute exposing (SeedAttribute)
import SiteUI
import SiteUI.Search as SiteSearch
import StaticPage exposing (StaticPage)
import Update.Utils exposing (withCommand, discardCommand)
import User exposing (User, AuthStatus)
import View exposing (view)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (parseRoute >> UrlUpdate)
        { init = init
        , update = update
        , subscriptions =
            Sub.batch
                [ Ports.loggedOut (always LogOut)
                , Ports.loggedIn OtherTabLoggedIn
                ]
                |> always
        , view = view
        }


type alias Flags =
    { authToken : Maybe String
    , authUserId : Maybe Int
    }



-- MODEL


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            parseRoute location

        ( model, cmd ) =
            Model.initial route
                |> fetchDataForRoute

        authorizationCmd =
            Maybe.map2 reAuthorize flags.authUserId flags.authToken
                |> Maybe.withDefault Cmd.none
    in
        ( model
        , Cmd.batch
            [ cmd
            , getNavigationData
            , setPageTitle model
            , authorizationCmd
            ]
        )



-- COMMANDS


setPageTitle : Model -> Cmd Msg
setPageTitle { route, pageData } =
    let
        mapper selector f =
            selector pageData
                |> RemoteData.toMaybe
                |> Maybe.map (f >> Ports.setPageTitle)
                |> Maybe.withDefault Cmd.none
    in
        case route of
            ProductDetails _ ->
                mapper .productDetails (.product >> .name)

            CategoryDetails _ _ ->
                pageData.categoryDetails
                    |> Paginate.getResponseData
                    |> Maybe.map (.category >> .name)
                    |> Maybe.withDefault ""
                    |> Ports.setPageTitle

            AdvancedSearch ->
                Ports.setPageTitle "Advanced Search"

            SearchResults data _ ->
                Ports.setPageTitle <|
                    case Search.uniqueSearch data of
                        Nothing ->
                            "Search Results"

                        Just searchType ->
                            case searchType of
                                AllProducts ->
                                    "All Products"

                                AttributeSearch (SeedAttribute.Organic) ->
                                    "Organic Products"

                                AttributeSearch (SeedAttribute.Heirloom) ->
                                    "Heirloom Products"

                                AttributeSearch (SeedAttribute.Regional) ->
                                    "South-Eastern Products"

                                AttributeSearch (SeedAttribute.Ecological) ->
                                    "Ecologically Grown Products"

            PageDetails _ ->
                mapper .pageDetails .name

            CreateAccount ->
                Ports.setPageTitle "Create an Account"

            CreateAccountSuccess ->
                Ports.setPageTitle "Account Creation Successful"

            Login ->
                Ports.setPageTitle "Customer Login"

            MyAccount ->
                Ports.setPageTitle "My Account"

            EditLogin ->
                Ports.setPageTitle "Edit Login Details"

            EditContact ->
                Ports.setPageTitle "Edit Contact Details"

            NotFound ->
                Ports.setPageTitle "Page Not Found"


{-| TODO: Move to PageData module?
-}
fetchDataForRoute : Model -> ( Model, Cmd Msg )
fetchDataForRoute ({ route, pageData } as model) =
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

                SearchResults data pagination ->
                    pageData.searchResults
                        |> Paginate.updateData PageData.searchConfig
                            { data = data, sorting = pagination.sorting }
                        |> discardCommand (Paginate.updatePerPage PageData.searchConfig pagination.perPage)
                        |> discardCommand (Paginate.jumpTo PageData.searchConfig pagination.page)
                        |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                        |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)

                PageDetails slug ->
                    ( { pageData | pageDetails = RemoteData.Loading }
                    , getPageDetails slug
                    )

                CreateAccount ->
                    case pageData.locations of
                        RemoteData.Success _ ->
                            ( pageData, Cmd.none )

                        _ ->
                            ( { pageData | locations = RemoteData.Loading }
                            , getLocationsData
                            )

                CreateAccountSuccess ->
                    doNothing

                Login ->
                    doNothing

                MyAccount ->
                    doNothing

                EditLogin ->
                    doNothing

                EditContact ->
                    if RemoteData.isSuccess pageData.locations then
                        ( pageData, getContactDetails model.currentUser )
                    else
                        ( pageData
                        , Cmd.batch
                            [ getContactDetails model.currentUser
                            , getLocationsData
                            ]
                        )

                NotFound ->
                    doNothing

        doNothing =
            ( pageData, Cmd.none )
    in
        ( { model | pageData = data }, cmd )


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


getLocationsData : Cmd Msg
getLocationsData =
    Api.get Api.CustomerLocations
        |> Api.withJsonResponse PageData.locationDataDecoder
        |> Api.sendRequest GetLocationsData


getContactDetails : AuthStatus -> Cmd Msg
getContactDetails authStatus =
    case authStatus of
        User.Anonymous ->
            Cmd.none

        User.Authorized user ->
            Api.get Api.CustomerContactDetails
                |> Api.withJsonResponse PageData.contactDetailsDecoder
                |> Api.withToken user.authToken
                |> Api.sendRequest GetContactDetails


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



-- UPDATE


{-| TODO: Refactor pagedata messages into separate msg & update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ pageData } as model) =
    case msg of
        UrlUpdate route ->
            { model | route = route }
                |> fetchDataForRoute
                |> clearSearchForm
                |> withCommand setPageTitle
                |> withCommand (always (Ports.collapseMobileMenus ()))

        NavigateTo route ->
            ( model, Routing.newUrl route )

        LogOut ->
            ( { model | currentUser = User.unauthorized }
            , Cmd.batch [ logoutRedirect model.route, Ports.removeAuthDetails () ]
            )

        OtherTabLoggedIn authData ->
            ( model, reAuthorize authData.userId authData.token )

        SearchMsg subMsg ->
            let
                ( searchData, cmd ) =
                    SiteSearch.update subMsg model.searchData
            in
                ( { model | searchData = searchData }, cmd )

        AdvancedSearchMsg subMsg ->
            ( { model | advancedSearchData = AdvancedSearch.update subMsg model.advancedSearchData }
            , Cmd.none
            )

        CreateAccountMsg subMsg ->
            let
                ( updatedForm, maybeAuthStatus, cmd ) =
                    CreateAccount.update subMsg model.createAccountForm
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
                    Login.update subMsg model.loginForm
            in
                ( { model
                    | loginForm = updatedForm
                    , currentUser = maybeAuthStatus |> Maybe.withDefault model.currentUser
                  }
                , Cmd.map LoginMsg cmd
                )

        EditLoginMsg subMsg ->
            EditLogin.update subMsg model.editLoginForm model.currentUser
                |> Tuple.mapFirst (\form -> { model | editLoginForm = form })
                |> Tuple.mapSecond (Cmd.map EditLoginMsg)

        EditContactMsg subMsg ->
            EditContact.update subMsg model.editContactForm model.currentUser
                |> Tuple.mapFirst (\form -> { model | editContactForm = form })
                |> Tuple.mapSecond (Cmd.map EditContactMsg)

        ReAuthorize response ->
            case response of
                RemoteData.Success authStatus ->
                    { model | currentUser = authStatus }
                        |> fetchDataForRoute

                _ ->
                    ( model, Cmd.none )

        GetProductDetailsData response ->
            let
                updatedPageData =
                    { pageData | productDetails = response }
            in
                ( { model | pageData = updatedPageData }, Cmd.none )
                    |> withCommand setPageTitle
                    |> withCommand (always Ports.scrollToTop)

        GetNavigationData response ->
            ( { model | navigationData = logUnsuccessfulRequest response }, Cmd.none )

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
                    |> withCommand setPageTitle
                    |> withCommand (always Ports.scrollToTop)

        GetLocationsData response ->
            let
                updatedPageData =
                    { pageData | locations = response }
            in
                ( { model | pageData = updatedPageData }, Cmd.none )

        GetContactDetails response ->
            let
                updatedPageData =
                    { pageData
                        | contactDetails = response
                    }

                updatedForm =
                    response
                        |> RemoteData.toMaybe
                        |> Maybe.map EditContact.fromContactDetails
                        |> Maybe.withDefault model.editContactForm
            in
                ( { model
                    | pageData = updatedPageData
                    , editContactForm = updatedForm
                  }
                , Cmd.none
                )

        CategoryPaginationMsg subMsg ->
            pageData.categoryDetails
                |> Paginate.update PageData.categoryConfig subMsg
                |> Tuple.mapSecond (Cmd.map CategoryPaginationMsg)
                |> (\( ps, cmd ) ->
                        ( ps, Cmd.batch [ cmd, updatePageFromPagination model.route ps ] )
                   )
                |> Tuple.mapFirst (\cd -> { pageData | categoryDetails = cd })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> withCommand setPageTitle
                |> withCommand (always Ports.scrollToTop)

        SearchPaginationMsg subMsg ->
            Paginate.update PageData.searchConfig subMsg pageData.searchResults
                |> Tuple.mapSecond (Cmd.map SearchPaginationMsg)
                |> (\( sr, cmd ) ->
                        ( sr, Cmd.batch [ cmd, updatePageFromPagination model.route sr ] )
                   )
                |> Tuple.mapFirst (\sr -> { pageData | searchResults = sr })
                |> Tuple.mapFirst (\pd -> { model | pageData = pd })
                |> withCommand (always Ports.scrollToTop)


updatePageFromPagination : Route -> Paginated a b c -> Cmd msg
updatePageFromPagination route paginated =
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
                    Routing.newUrl <| newRouteConstructor newPage


clearSearchForm : ( Model, Cmd msg ) -> ( Model, Cmd msg )
clearSearchForm ( model, cmd ) =
    flip (,) cmd <|
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


logUnsuccessfulRequest : WebData a -> WebData a
logUnsuccessfulRequest response =
    case response of
        RemoteData.Success _ ->
            response

        _ ->
            Debug.log "Unsuccessful Request Returned" response


logoutRedirect : Route -> Cmd msg
logoutRedirect route =
    if Routing.authRequired route then
        Routing.newUrl <| PageDetails "home"
    else
        Cmd.none
