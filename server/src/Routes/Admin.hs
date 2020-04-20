{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin
    ( AdminAPI
    , adminRoutes
    ) where

import Servant ((:<|>)(..), (:>), ServerT)

import Routes.Admin.Categories
import Routes.Admin.CategorySales
import Routes.Admin.Coupons
import Routes.Admin.Customers
import Routes.Admin.Dashboard
import Routes.Admin.Orders
import Routes.Admin.Products
import Routes.Admin.ProductSales
import Routes.Admin.Settings
import Routes.Admin.ShippingMethods
import Routes.Admin.StaticPages
import Routes.Admin.Surcharges
import Server (App)

type AdminAPI =
         "categories" :> CategoryAPI
    :<|> "pages" :> StaticPageAPI
    :<|> "orders" :> OrderAPI
    :<|> "customers" :> CustomerAPI
    :<|> "products" :> ProductAPI
    :<|> "coupons" :> CouponAPI
    :<|> "surcharges" :> SurchargesAPI
    :<|> "shipping" :> ShippingMethodsAPI
    :<|> "settings" :> SettingsAPI
    :<|> "product-sales" :> ProductSalesAPI
    :<|> "category-sales" :> CategrySalesAPI
    :<|> "dashboard" :> DashboardAPI


adminRoutes :: ServerT AdminAPI App
adminRoutes =
         categoryRoutes
    :<|> staticPageRoutes
    :<|> orderRoutes
    :<|> customerRoutes
    :<|> productRoutes
    :<|> couponRoutes
    :<|> surchargesRoutes
    :<|> shippingMethodsRoutes
    :<|> settingsRoutes
    :<|> productSalesRoutes
    :<|> categorySalesRoutes
    :<|> dashboardRoutes
