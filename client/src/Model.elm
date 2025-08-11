module Model exposing
    ( Model
    , initial
    )

import Api exposing (Endpoint(..))
import BootstrapGallery as Gallery
import Categories.AdminViews as CategoryAdmin
import Components.ProfileNavbar as ProfileNavbar
import Dict exposing (Dict)
import Models.Fields exposing (ImageData)
import PageData exposing (PageData)
import Pages.Cart.Cart as Cart
import Pages.Cart.Type as Cart
import Pages.Checkout as Checkout
import Pages.CreateAccount as CreateAccount
import Pages.EditAddress as EditAddress
import Pages.EditLogin as EditLogin
import Pages.Login as Login
import Pages.QuickOrder as QuickOrder
import Pages.ResetPassword as ResetPassword
import Pages.VerificationRequired as VerificationRequired
import Pages.VerifyEmail as VerifyEmail
import Product exposing (ProductVariantId)
import Products.AdminViews as ProductAdmin
import RemoteData exposing (WebData)
import Routing exposing (Route)
import Search
import SiteUI exposing (CategoryListData, NavigationData)
import Time
import User exposing (AuthStatus)
import Views.AdminDashboard as AdminDashboard
import Views.CategorySalesAdmin as CategorySalesAdmin
import Views.CouponAdmin as CouponAdmin
import Views.CustomerAdmin as CustomerAdmin
import Views.OrderAdmin as OrderAdmin
import Views.ProductSalesAdmin as ProductSalesAdmin
import Views.SettingsAdmin as SettingsAdmin
import Views.ShippingAdmin as ShippingAdmin
import Views.StaticPageAdmin as StaticPageAdmin
import Views.SurchargesAdmin as SurchargesAdmin


type alias Model =
    { navigationData : WebData NavigationData
    , categoryListData : WebData CategoryListData
    , route : Route
    , pageData : PageData
    , searchData : Search.Data
    , advancedSearchData : Search.Data
    , createAccountForm : CreateAccount.Form
    , verifyEmailForm : VerifyEmail.Form
    , verificationRequiredForm : VerificationRequired.Form
    , loginForm : Login.Form
    , editLoginForm : EditLogin.Form
    , editAddressForm : EditAddress.Form
    , resetPasswordForm : ResetPassword.Form
    , addToCartForms : Cart.CartForms
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
    , newProductSaleForm : ProductSalesAdmin.NewForm
    , editProductSaleForm : ProductSalesAdmin.EditForm
    , newCategorySaleForm : CategorySalesAdmin.NewForm
    , editCategorySaleForm : CategorySalesAdmin.EditForm
    , adminDashboard : AdminDashboard.Model
    , settingsForm : SettingsAdmin.Form
    , cartItemCount : Int
    , maybeSessionToken : Maybe String
    , currentUser : AuthStatus
    , zone : Time.Zone
    , key : Routing.Key
    , helcimUrl : String

    -- sub-component
    , profileNavbar : ProfileNavbar.Model
    }

initial : Routing.Key -> Route -> String -> Model
initial key route helcimUrl =
    { navigationData = RemoteData.Loading
    , categoryListData = RemoteData.Loading
    , route = route
    , pageData = PageData.initial
    , searchData = Search.initial
    , advancedSearchData = Search.initial
    , createAccountForm = CreateAccount.initial
    , verifyEmailForm = VerifyEmail.initial
    , verificationRequiredForm = VerificationRequired.initial
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
    , newProductSaleForm = ProductSalesAdmin.initialNewForm
    , editProductSaleForm = ProductSalesAdmin.initialEditForm
    , newCategorySaleForm = CategorySalesAdmin.initialNewForm
    , editCategorySaleForm = CategorySalesAdmin.initialEditForm
    , adminDashboard = AdminDashboard.initialModel
    , settingsForm = SettingsAdmin.initialForm
    , cartItemCount = 0
    , maybeSessionToken = Nothing
    , currentUser = User.unauthorized
    , zone = Time.utc
    , key = key
    , helcimUrl = helcimUrl
    , profileNavbar = ProfileNavbar.init
    }
