{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Api
    ( app
    ) where

import Control.Monad.Reader (runReaderT)
import Servant

import Auth
import Config
import Routes
import Server

-- | Turn a `Config` into a WAI `Application` representing our API.
app :: Config -> Application
app cfg =
    let
        readerServer :: Server API
        readerServer =
            enter readerToHandler server

        readerToHandler :: App :~> Handler
        readerToHandler =
            NT $ \x -> runReaderT x cfg

    in
        case getEnv cfg of
            Production ->
                serveWithContext api authServerContext readerServer
            Development ->
                serveWithContext devApi authServerContext $
                         readerServer
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
