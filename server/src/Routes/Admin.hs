{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin
    ( AdminAPI
    , adminRoutes
    ) where

import Servant ((:>), ServerT)

import Routes.Admin.Categories
import Server (App)

type AdminAPI =
       "categories" :> CategoryAPI


adminRoutes :: ServerT AdminAPI App
adminRoutes =
       categoryRoutes
