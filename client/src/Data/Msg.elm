module Data.Msg exposing (Msg(..))

-- import Components.Tooltip as Tooltip

import BootstrapGallery as Gallery
import Browser exposing (UrlRequest)
import Components.Admin.AdminDashboard as AdminDashboard
import Components.Admin.CategorySalesAdmin as CategorySalesAdmin
import Components.Admin.CouponAdmin as CouponAdmin
import Components.Admin.CustomerAdmin as CustomerAdmin
import Components.Admin.OrderAdmin as OrderAdmin
import Components.Admin.ProductSalesAdmin as ProductSalesAdmin
import Components.Admin.SettingsAdmin as SettingsAdmin
import Components.Admin.ShippingAdmin as ShippingAdmin
import Components.Admin.StaticPageAdmin as StaticPageAdmin
import Components.Admin.SurchargesAdmin as SurchargesAdmin
import Components.AdvancedSearch as AdvancedSearch
import Components.Categories.AdminViews as CategoryAdmin
import Components.Products.AdminViews as ProductAdmin
import Components.ProfileNavbar as ProfileNavbar
import Components.Tooltip as Tooltip
import Components.SiteUI.Search as SiteSearch
import Data.Api as Api
import Data.Fields exposing (ImageData)
import Data.Locations exposing (AddressLocations)
import Data.PageData as PageData exposing (CustomerData, OrderData, ProductData)
import Data.Product exposing (ProductId, ProductVariantId)
import Data.Routing.Routing exposing (Route)
import Data.SiteUI exposing (CategoryListData, NavigationData)
import Data.StaticPage exposing (StaticPage)
import Data.User as User
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
import Paginate
import RemoteData exposing (WebData)
import Time


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | LinkClick UrlRequest
    | NewZone Time.Zone
    | UpdateZone
    | LogOut
    | FocusMainContent
    | FocusMainContentResult
      -- Other Tabs (via localStorage ports)
    | OtherTabLoggedIn Int
    | OtherTabNewCartToken String
    | OtherTabCartItemCountChanged Int
      -- Product List/Details Cart Forms
    | ProductDetailsLightbox (Gallery.Msg ImageData)
    | ChangeCartFormVariantId ProductId ProductVariantId
    | ChangeCartFormQuantity ProductId Int
    | IncreaseCartFormQuantity ProductId
    | DecreaseCartFormQuantity ProductId
    | SubmitAddToCart ProductId ProductVariantId
    | SubmitAddToCartResponse ProductId Int (WebData (Result Api.FormErrors String))
    | ResetCartFormStatus ProductId
      -- My Account Page
    | ShowAllOrders
      -- Sub-Components
    | ProfileNavbarMsg ProfileNavbar.Msg
    | TooltipMsg Tooltip.Msg
      -- Sub-Messages
    | SearchMsg SiteSearch.Msg
    | AdvancedSearchMsg AdvancedSearch.Msg
    | CreateAccountMsg CreateAccount.Msg
    | VerifyEmailMsg VerifyEmail.Msg
    | LoginMsg Login.Msg
    | VerificationRequiredMsg VerificationRequired.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | EditLoginMsg EditLogin.Msg
    | EditAddressMsg EditAddress.Msg
    | EditCartMsg Cart.Msg
    | QuickOrderMsg QuickOrder.Msg
    | CheckoutMsg Checkout.Msg
    | NewCategoryMsg CategoryAdmin.NewMsg
    | EditCategoryMsg CategoryAdmin.EditMsg
    | PageListMsg StaticPageAdmin.ListMsg
    | NewPageMsg StaticPageAdmin.NewMsg
    | EditPageMsg StaticPageAdmin.EditMsg
    | OrderSearchMsg OrderAdmin.SearchMsg
    | OrderDetailsMsg OrderAdmin.DetailsMsg
    | CustomerSearchMsg CustomerAdmin.SearchMsg
    | EditCustomerMsg CustomerAdmin.EditMsg
    | ProductListMsg ProductAdmin.ListMsg
    | NewProductMsg ProductAdmin.NewMsg
    | EditProductMsg ProductAdmin.EditMsg
    | NewCouponMsg CouponAdmin.NewMsg
    | EditCouponMsg CouponAdmin.EditMsg
    | SurchargesMsg SurchargesAdmin.Msg
    | ShippingMsg ShippingAdmin.Msg
    | SettingsMsg SettingsAdmin.Msg
    | NewProductSaleMsg ProductSalesAdmin.NewMsg
    | EditProductSaleMsg ProductSalesAdmin.EditMsg
    | NewCategorySaleMsg CategorySalesAdmin.NewMsg
    | EditCategorySaleMsg CategorySalesAdmin.EditMsg
    | DashboardMsg AdminDashboard.Msg
      -- API Requests
    | ReAuthorize (WebData User.AuthStatus)
    | LogOutResponse (WebData ())
    | GetProductDetailsData (Maybe ProductVariantId) (WebData PageData.ProductDetails)
    | GetNavigationData (WebData NavigationData)
    | GetCategoryListData (WebData CategoryListData)
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
    | GetAdminCategoryList (WebData PageData.AdminCategoryListData)
    | GetAdminNewCategoryData (WebData PageData.AdminNewCategoryData)
    | GetAdminEditCategoryData (WebData PageData.AdminEditCategoryData)
    | GetAdminPageList (WebData PageData.AdminPageListData)
    | GetAdminEditPageData (WebData PageData.AdminEditPageData)
    | AdminOrderPaginationMsg (Paginate.Msg OrderData ())
    | GetAdminOrderDetails (WebData PageData.AdminOrderDetails)
    | AdminCustomerPaginationMsg (Paginate.Msg CustomerData ())
    | GetAdminEditCustomerData (WebData PageData.AdminEditCustomerData)
    | GetAdminProductList (WebData PageData.AdminProductListData)
    | GetAdminSharedProductData (WebData PageData.AdminSharedProductData)
    | GetAdminEditProductData (WebData ( ProductAdmin.Form, ProductId ))
    | GetAdminCouponList (WebData PageData.AdminCouponListData)
    | GetAdminEditCouponData (WebData PageData.AdminEditCouponData)
    | GetAdminProductSaleList (WebData PageData.AdminProductSaleListData)
    | GetAdminProductSaleNew (WebData PageData.AdminProductSaleNewData)
    | GetAdminEditProductSaleData (WebData PageData.AdminEditProductSaleData)
    | GetAdminCategorySaleList (WebData PageData.AdminCategorySaleListData)
    | GetAdminNewCategorySaleData (WebData PageData.AdminNewCategorySaleData)
    | GetAdminEditCategorySaleData (WebData PageData.AdminEditCategorySaleData)
    | GetAdminDashboardData (WebData PageData.AdminDashboardData)
