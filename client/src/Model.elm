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
import BootstrapGallery as Gallery
import Cart
import Categories.AdminViews as CategoryAdmin
import Checkout
import Dict exposing (Dict)
import Models.Fields exposing (ImageData)
import PageData exposing (PageData)
import Product exposing (ProductVariantId)
import Products.AdminViews as ProductAdmin
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route)
import Search
import SiteUI exposing (NavigationData)
import Time
import User exposing (AuthStatus)
import Views.CouponAdmin as CouponAdmin
import Views.CustomerAdmin as CustomerAdmin
import Views.OrderAdmin as OrderAdmin
import Views.SettingsAdmin as SettingsAdmin
import Views.ShippingAdmin as ShippingAdmin
import Views.StaticPageAdmin as StaticPageAdmin
import Views.SurchargesAdmin as SurchargesAdmin


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
    , productDetailsLightbox : Gallery.Model ImageData
    , editCartForm : Cart.Form
    , quickOrderForms : QuickOrder.Forms
    , checkoutForm : Checkout.Form
    , newCategoryForm : CategoryAdmin.NewForm
    , editCategoryForm : CategoryAdmin.EditForm
    , pageListForm : StaticPageAdmin.ListForm
    , newPageForm : StaticPageAdmin.NewForm
    , editPageForm : StaticPageAdmin.EditForm
    , orderSearchForm : OrderAdmin.SearchForm
    , orderDetailsForm : OrderAdmin.DetailsForm
    , customerSearchForm : CustomerAdmin.SearchForm
    , editCustomerForm : CustomerAdmin.EditForm
    , productListForm : ProductAdmin.ListForm
    , newProductForm : ProductAdmin.NewForm
    , editProductForm : ProductAdmin.EditForm
    , newCouponForm : CouponAdmin.NewForm
    , editCouponForm : CouponAdmin.EditForm
    , surchargeForm : SurchargesAdmin.Form
    , shippingMethodForm : ShippingAdmin.Form
    , settingsForm : SettingsAdmin.Form
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
        , requestStatus : WebData ()
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
    , productDetailsLightbox = Gallery.initial
    , editCartForm = Cart.initial
    , quickOrderForms = QuickOrder.initial
    , checkoutForm = Checkout.initial
    , newCategoryForm = CategoryAdmin.initialNewForm
    , editCategoryForm = CategoryAdmin.initialEditForm
    , pageListForm = StaticPageAdmin.initialListForm
    , newPageForm = StaticPageAdmin.initialNewForm
    , editPageForm = StaticPageAdmin.initialEditForm
    , orderSearchForm = OrderAdmin.initialSearchForm
    , orderDetailsForm = OrderAdmin.initialDetailsForm
    , customerSearchForm = CustomerAdmin.initialSearchForm
    , editCustomerForm = CustomerAdmin.initialEditForm
    , productListForm = ProductAdmin.initialListForm
    , newProductForm = ProductAdmin.initialNewForm
    , editProductForm = ProductAdmin.initialEditForm
    , newCouponForm = CouponAdmin.initialNewForm
    , editCouponForm = CouponAdmin.initialEditForm
    , surchargeForm = SurchargesAdmin.initialForm
    , shippingMethodForm = ShippingAdmin.initialForm
    , settingsForm = SettingsAdmin.initialForm
    , cartItemCount = 0
    , maybeSessionToken = Nothing
    , currentUser = User.unauthorized
    , zone = Time.utc
    , key = key
    }
