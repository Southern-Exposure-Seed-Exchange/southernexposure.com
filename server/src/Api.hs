{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api
    ( app
    ) where

import Control.Monad.Reader (runReaderT)
import Database.Persist (Entity, selectList)
import Servant

import Config
import Models
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
    Get '[JSON] (JSONList (Entity Product))

-- | Return the Handler functions for the `API` type.
server :: ServerT API App
server = do
    l <- runDB $ selectList [] []
    return $ JSONList l
