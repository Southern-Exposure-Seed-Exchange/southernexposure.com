{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin
    ( AdminAPI
    , adminRoutes
    ) where

import Servant ((:<|>)(..), (:>), ServerT)

import Routes.Admin.Categories
import Routes.Admin.StaticPages
import Server (App)

type AdminAPI =
         "categories" :> CategoryAPI
    :<|> "pages" :> StaticPageAPI


adminRoutes :: ServerT AdminAPI App
adminRoutes =
         categoryRoutes
    :<|> staticPageRoutes
