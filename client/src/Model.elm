module Model
    exposing
        ( Model
        , CartForms
        , initial
        )

import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Auth.CreateAccount as CreateAccount
import Auth.EditContact as EditContact
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.ResetPassword as ResetPassword
import Cart
import PageData exposing (PageData)
import Product exposing (ProductVariantId)
import QuickOrder
import Routing exposing (Route)
import Search
import SiteUI exposing (NavigationData)
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
    , editContactForm : EditContact.Form
    , resetPasswordForm : ResetPassword.Form
    , addToCartForms : CartForms
    , editCartForm : Cart.Form
    , quickOrderForms : QuickOrder.Forms
    , cartItemCount : Int
    , maybeSessionToken : Maybe String
    , currentUser : AuthStatus
    }


type alias CartForms =
    Dict Int { variant : Maybe ProductVariantId, quantity : Int }


initial : Route -> Model
initial route =
    { navigationData = RemoteData.Loading
    , route = route
    , pageData = PageData.initial
    , searchData = Search.initial
    , advancedSearchData = Search.initial
    , createAccountForm = CreateAccount.initial
    , loginForm = Login.initial
    , editLoginForm = EditLogin.initial
    , editContactForm = EditContact.initial
    , resetPasswordForm = ResetPassword.initial
    , addToCartForms = Dict.empty
    , editCartForm = Cart.initial
    , quickOrderForms = QuickOrder.initial
    , cartItemCount = 0
    , maybeSessionToken = Nothing
    , currentUser = User.unauthorized
    }
