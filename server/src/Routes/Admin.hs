{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin
    ( AdminAPI
    , adminRoutes
    ) where

import Servant ((:<|>)(..), (:>), ServerT)

import Routes.Admin.Categories
import Routes.Admin.Orders
import Routes.Admin.StaticPages
import Server (App)

type AdminAPI =
         "categories" :> CategoryAPI
    :<|> "pages" :> StaticPageAPI
    :<|> "orders" :> OrderAPI


adminRoutes :: ServerT AdminAPI App
adminRoutes =
         categoryRoutes
    :<|> staticPageRoutes
    :<|> orderRoutes
