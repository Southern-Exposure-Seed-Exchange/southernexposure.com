module Messages
    exposing
        ( Msg(..)
        , EditCartMessage(..)
        )

import Paginate
import RemoteData exposing (WebData)
import AdvancedSearch
import Auth.CreateAccount as CreateAccount
import Auth.Login as Login
import Auth.EditLogin as EditLogin
import Auth.EditContact as EditContact
import Auth.ResetPassword as ResetPassword
import StaticPage exposing (StaticPage)
import PageData exposing (ProductData)
import Product exposing (ProductId, ProductVariantId)
import Routing exposing (Route)
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch
import User


type EditCartMessage
    = Quantity PageData.CartItemId Int
    | Remove PageData.CartItemId
    | Submit
    | UpdateResponse (WebData PageData.CartDetails)


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | LogOut
    | OtherTabLoggedIn { userId : Int, token : String }
    | OtherTabNewCartToken String
    | ChangeCartFormVariantId ProductId ProductVariantId
    | ChangeCartFormQuantity ProductId Int
    | SubmitAddToCart ProductId ProductVariantId
    | SubmitAddToCartResponse (WebData String)
    | SearchMsg SiteSearch.Msg
    | AdvancedSearchMsg AdvancedSearch.Msg
    | CreateAccountMsg CreateAccount.Msg
    | LoginMsg Login.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | EditLoginMsg EditLogin.Msg
    | EditContactMsg EditContact.Msg
    | EditCartMsg EditCartMessage
    | ReAuthorize (WebData User.AuthStatus)
    | GetProductDetailsData (WebData PageData.ProductDetails)
    | GetNavigationData (WebData NavigationData)
    | GetAdvancedSearchData (WebData PageData.AdvancedSearch)
    | GetPageDetailsData (WebData StaticPage)
    | GetLocationsData (WebData PageData.LocationData)
    | GetContactDetails (WebData PageData.ContactDetails)
    | GetCartDetails (WebData PageData.CartDetails)
    | CategoryPaginationMsg (Paginate.Msg ProductData PageData.CategoryDetails)
    | SearchPaginationMsg (Paginate.Msg ProductData String)
