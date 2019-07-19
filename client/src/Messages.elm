module Messages exposing (Msg(..))

import AdvancedSearch
import Api
import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.ResetPassword as ResetPassword
import Browser exposing (UrlRequest)
import Cart
import Checkout
import Locations exposing (AddressLocations)
import PageData exposing (ProductData)
import Paginate
import Product exposing (ProductId, ProductVariantId)
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route)
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch
import StaticPage exposing (StaticPage)
import Time
import User


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | LinkClick UrlRequest
    | NewZone Time.Zone
    | LogOut
      -- Other Tabs (via localStorage ports)
    | OtherTabLoggedIn { userId : Int, token : String }
    | OtherTabNewCartToken String
    | OtherTabCartItemCountChanged Int
      -- Product List/Details Cart Forms
    | ChangeCartFormVariantId ProductId ProductVariantId
    | ChangeCartFormQuantity ProductId Int
    | SubmitAddToCart ProductId ProductVariantId
    | SubmitAddToCartResponse Int (WebData String)
      -- My Account Page
    | ShowAllOrders
      -- Sub-Messages
    | SearchMsg SiteSearch.Msg
    | AdvancedSearchMsg AdvancedSearch.Msg
    | CreateAccountMsg CreateAccount.Msg
    | LoginMsg Login.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | EditLoginMsg EditLogin.Msg
    | EditAddressMsg EditAddress.Msg
    | EditCartMsg Cart.Msg
    | QuickOrderMsg QuickOrder.Msg
    | CheckoutMsg Checkout.Msg
      -- API Requests
    | ReAuthorize (WebData User.AuthStatus)
    | GetProductDetailsData (WebData PageData.ProductDetails)
    | GetNavigationData (WebData NavigationData)
    | GetAdvancedSearchData (WebData PageData.AdvancedSearch)
    | GetPageDetailsData (WebData StaticPage)
    | GetAddressLocations (WebData AddressLocations)
    | GetMyAccountDetails (WebData PageData.MyAccount)
    | GetAddressDetails (WebData PageData.AddressDetails)
    | GetCartDetails (WebData PageData.CartDetails)
    | GetCartItemCount (WebData Int)
    | GetCheckoutDetails (WebData (Result Api.FormErrors PageData.CheckoutDetails))
    | GetCheckoutSuccessDetails (WebData PageData.OrderDetails)
    | CategoryPaginationMsg (Paginate.Msg ProductData PageData.CategoryDetails)
    | SearchPaginationMsg (Paginate.Msg ProductData String)
