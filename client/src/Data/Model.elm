module Data.Model exposing
    ( Model
    , initial
    )

import BootstrapGallery as Gallery
import Components.Admin.AdminDashboard as AdminDashboard
import Components.Admin.CategoryAdmin as CategoryAdmin
import Components.Admin.CategorySalesAdmin as CategorySalesAdmin
import Components.Admin.CouponAdmin as CouponAdmin
import Components.Admin.CustomerAdmin as CustomerAdmin
import Components.Admin.OrderAdmin as OrderAdmin
import Components.Admin.ProductAdmin as ProductAdmin
import Components.Admin.ProductSalesAdmin as ProductSalesAdmin
import Components.Admin.SettingsAdmin as SettingsAdmin
import Components.Admin.ShippingAdmin as ShippingAdmin
import Components.Admin.StaticPageAdmin as StaticPageAdmin
import Components.Admin.SurchargesAdmin as SurchargesAdmin
import Components.ProfileNavbar as ProfileNavbar
import Components.Tooltip as Tooltip
import Data.Api exposing (Endpoint(..))
import Data.Fields exposing (ImageData)
import Data.Msg exposing (Msg(..))
import Data.PageData as PageData exposing (CartDetails, PageData)
import Data.Routing.Routing as Routing exposing (Route)
import Data.Search as Search
import Data.Shared exposing (Shared)
import Data.SiteUI exposing (CategoryListData, NavigationData)
import Data.User as User exposing (AuthStatus)
import Dict exposing (Dict)
import Pages.Cart.Cart as Cart
import Pages.Cart.Type as Cart
import Pages.Checkout as Checkout
import Pages.CreateAccount as CreateAccount
import Pages.EditAddress as EditAddress
import Pages.EditLogin as EditLogin
import Pages.Login as Login
import Pages.ProductList.Type as ProductListPage
import Pages.QuickOrder as QuickOrder
import Pages.ResetPassword as ResetPassword
import Pages.VerificationRequired as VerificationRequired
import Pages.VerifyEmail as VerifyEmail
import RemoteData exposing (WebData)
import Time


type alias Model =
    { navigationData : WebData NavigationData
    , categoryListData : WebData CategoryListData
    , route : Route
    , pageData : PageData
    , createAccountForm : CreateAccount.Form
    , verifyEmailForm : VerifyEmail.Form
    , verificationRequiredForm : VerificationRequired.Form
    , loginForm : Login.Form
    , editLoginForm : EditLogin.Form
    , editAddressForm : EditAddress.Form
    , resetPasswordForm : ResetPassword.Form
    , productDetailsLightbox : Gallery.Model ImageData
    , editCartForm : Cart.Model
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
    , zone : Time.Zone
    , key : Routing.Key
    , helcimUrl : String
    , postgridApiKey : String

    -- sub-component
    , year : Int
    , shared : Shared Msg
    , profileNavbar : ProfileNavbar.Model
    , advancedSearchData : Search.Data
    , cartDetails : WebData CartDetails

    -- page state
    , productListPage : ProductListPage.Model
    }


initShared : Shared Msg
initShared =
    { maybeSessionToken = Nothing
    , currentUser = User.unauthorized
    , tooltips = Tooltip.init
    , tooltipMsg = TooltipMsg
    , navigateToMsg = NavigateTo
    , lightboxMsg = ProductDetailsLightbox
    , productMsg = ProductMsg
    }


initial : Routing.Key -> Route -> String -> String -> Model
initial key route helcimUrl postgridApiKey =
    { navigationData = RemoteData.Loading
    , categoryListData = RemoteData.Loading
    , route = route
    , pageData = PageData.initial
    , createAccountForm = CreateAccount.initial
    , verifyEmailForm = VerifyEmail.initial
    , verificationRequiredForm = VerificationRequired.initial
    , loginForm = Login.initial
    , editLoginForm = EditLogin.initial
    , editAddressForm = EditAddress.initial
    , resetPasswordForm = ResetPassword.initial
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
    , zone = Time.utc
    , key = key
    , helcimUrl = helcimUrl
    , postgridApiKey = postgridApiKey

    -- sub components
    , year = 2024
    , shared = initShared
    , advancedSearchData = Search.initial
    , profileNavbar = ProfileNavbar.init
    , cartDetails = RemoteData.NotAsked

    -- page state
    , productListPage = ProductListPage.init
    }
