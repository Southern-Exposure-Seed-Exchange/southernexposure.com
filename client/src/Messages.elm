module Messages exposing (Msg(..))

import AdvancedSearch
import Api
import Auth.CreateAccount as CreateAccount
import Auth.EditAddress as EditAddress
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import Auth.ResetPassword as ResetPassword
import BootstrapGallery as Gallery
import Browser exposing (UrlRequest)
import Cart
import Categories.AdminViews as CategoryAdmin
import Checkout
import Locations exposing (AddressLocations)
import Models.Fields exposing (ImageData)
import PageData exposing (CustomerData, OrderData, ProductData)
import Paginate
import Product exposing (ProductId, ProductVariantId)
import Products.AdminViews as ProductAdmin
import QuickOrder
import RemoteData exposing (WebData)
import Routing exposing (Route)
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch
import StaticPage exposing (StaticPage)
import Time
import User
import Views.CategorySalesAdmin as CategorySalesAdmin
import Views.CouponAdmin as CouponAdmin
import Views.CustomerAdmin as CustomerAdmin
import Views.OrderAdmin as OrderAdmin
import Views.ProductSalesAdmin as ProductSalesAdmin
import Views.SettingsAdmin as SettingsAdmin
import Views.ShippingAdmin as ShippingAdmin
import Views.StaticPageAdmin as StaticPageAdmin
import Views.SurchargesAdmin as SurchargesAdmin


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
    | SubmitAddToCart ProductId ProductVariantId
    | SubmitAddToCartResponse ProductId Int (WebData String)
    | ResetCartFormStatus ProductId
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
      -- API Requests
    | ReAuthorize (WebData User.AuthStatus)
    | LogOutResponse (WebData ())
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
