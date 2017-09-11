{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Api
    ( app
    ) where

import Control.Monad.Reader (runReaderT)
import Servant

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
        serve api $
                 readerServer
            :<|> serveDirectoryWebApp (getMediaDirectory cfg)

api :: Proxy (API :<|> "media" :> Raw)
api = Proxy


-- | The `API` type describes the API schema of the entire Application.
type API =
         "categories" :> CategoryAPI
    :<|> "products" :> ProductAPI
    :<|> "pages" :> StaticPageAPI
    :<|> "customers" :> CustomerAPI

-- | Return the Handler functions for the `API` type.
server :: ServerT API App
server =
         categoryRoutes
    :<|> productRoutes
    :<|> staticPageRoutes
    :<|> customerRoutes
