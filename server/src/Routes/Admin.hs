{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin
    ( AdminAPI
    , adminRoutes
    ) where

import Servant ((:<|>)(..), (:>), ServerT)

import Routes.Admin.Categories
import Routes.Admin.Customers
import Routes.Admin.Orders
import Routes.Admin.Products
import Routes.Admin.StaticPages
import Server (App)

type AdminAPI =
         "categories" :> CategoryAPI
    :<|> "pages" :> StaticPageAPI
    :<|> "orders" :> OrderAPI
    :<|> "customers" :> CustomerAPI
    :<|> "products" :> ProductAPI


adminRoutes :: ServerT AdminAPI App
adminRoutes =
         categoryRoutes
    :<|> staticPageRoutes
    :<|> orderRoutes
    :<|> customerRoutes
    :<|> productRoutes
