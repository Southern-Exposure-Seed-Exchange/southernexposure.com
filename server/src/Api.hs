{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Api
    ( app
    ) where

import Servant.Server.Experimental.Auth (AuthHandler)
import Network.Wai (Request)
import Servant

import Auth
import Config
import Routes
import Server

-- | Turn a `Config` into a WAI `Application` representing our API.
app :: Config -> Application
app cfg =
    let


        readerToHandler :: App a -> Handler a
        readerToHandler x = runApp x cfg
        contextProxy = Proxy :: Proxy '[AuthHandler Request WrappedAuthToken]
        context =
            authServerContext $ getCookieSecret cfg
    in
        case getEnv cfg of
            Production ->
                serveWithContext api context $ hoistServerWithContext api contextProxy readerToHandler server
            Development ->
                serveWithContext devApi context $ hoistServerWithContext devApi contextProxy readerToHandler $
                         server
                    :<|> serveDirectoryWebApp (getMediaDirectory cfg)

api :: Proxy API
api = Proxy

devApi :: Proxy (API :<|> "media" :> Raw)
devApi = Proxy


-- | The `API` type describes the API schema of the entire Application.
type API =
         "categories" :> CategoryAPI
    :<|> "products" :> ProductAPI
    :<|> "pages" :> StaticPageAPI
    :<|> "customers" :> CustomerAPI
    :<|> "carts" :> CartAPI
    :<|> "checkout" :> CheckoutAPI
    :<|> "stone-edge" :> StoneEdgeAPI
    :<|> "admin" :> AdminAPI
    :<|> "redirects" :> RedirectAPI
    :<|> FeedAPI

-- | Return the Handler functions for the `API` type.
server :: ServerT API App
server =
         categoryRoutes
    :<|> productRoutes
    :<|> staticPageRoutes
    :<|> customerRoutes
    :<|> cartRoutes
    :<|> checkoutRoutes
    :<|> stoneEdgeRoutes
    :<|> adminRoutes
    :<|> redirectRoutes
    :<|> feedRoutes
