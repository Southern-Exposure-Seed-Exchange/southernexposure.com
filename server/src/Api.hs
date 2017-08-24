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
        serve api readerServer

api :: Proxy API
api =
    Proxy


-- | The `API` type describes the API schema of the entire Application.
type API =
    ProductDetailsRoute

-- | Return the Handler functions for the `API` type.
server :: ServerT API App
server =
    productDetailsRoute
