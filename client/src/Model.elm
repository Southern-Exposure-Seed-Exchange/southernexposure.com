module Model exposing
    ( CartForms
    , Model
    , initial
    )

import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.ResetPassword as ResetPassword
import Cart
import Categories.AdminViews as CategoryAdmin
import Checkout
import Dict exposing (Dict)
import PageData exposing (PageData)
import Product exposing (ProductVariantId)
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route)
import Search
import SiteUI exposing (NavigationData)
import Time
import User exposing (AuthStatus)


type alias Model =
    { navigationData : WebData NavigationData
    , route : Route
    , pageData : PageData
    , searchData : Search.Data
    , advancedSearchData : Search.Data
    , createAccountForm : CreateAccount.Form
    , loginForm : Login.Form
    , editLoginForm : EditLogin.Form
    , editAddressForm : EditAddress.Form
    , resetPasswordForm : ResetPassword.Form
    , addToCartForms : CartForms
    , editCartForm : Cart.Form
    , quickOrderForms : QuickOrder.Forms
    , checkoutForm : Checkout.Form
    , newCategoryForm : CategoryAdmin.NewForm
    , cartItemCount : Int
    , maybeSessionToken : Maybe String
    , currentUser : AuthStatus
    , zone : Time.Zone
    , key : Routing.Key
    }


type alias CartForms =
    Dict Int
        { variant : Maybe ProductVariantId
        , quantity : Int
        }


initial : Routing.Key -> Route -> Model
initial key route =
    { navigationData = RemoteData.Loading
    , route = route
    , pageData = PageData.initial
    , searchData = Search.initial
    , advancedSearchData = Search.initial
    , createAccountForm = CreateAccount.initial
    , loginForm = Login.initial
    , editLoginForm = EditLogin.initial
    , editAddressForm = EditAddress.initial
    , resetPasswordForm = ResetPassword.initial
    , addToCartForms = Dict.empty
    , editCartForm = Cart.initial
    , quickOrderForms = QuickOrder.initial
    , checkoutForm = Checkout.initial
    , newCategoryForm = CategoryAdmin.initialNewForm
    , cartItemCount = 0
    , maybeSessionToken = Nothing
    , currentUser = User.unauthorized
    , zone = Time.utc
    , key = key
    }
