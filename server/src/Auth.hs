{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Auth
    ( AuthToken(..)
    , authServerContext
    , validateToken
    ) where

import Control.Monad.Trans (lift)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist (Entity(..), getBy)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth

import Models.DB (Unique(UniqueToken), Customer)
import Server (App, runDB)

import qualified Data.Text as T


newtype AuthToken = AuthToken { fromAuthToken :: T.Text }

type instance AuthServerData (AuthProtect "auth-token") = AuthToken


authHandler :: AuthHandler Request AuthToken
authHandler =
    mkAuthHandler handler
    where handler :: Request -> Handler AuthToken
          handler req =
            case lookup "Auth-Token" (requestHeaders req) of
                Nothing ->
                    throwError $ err401  { errBody = "Missing Auth-Token Header" }
                Just authToken ->
                    return . AuthToken $ decodeUtf8 authToken


authServerContext :: Context (AuthHandler Request AuthToken ': '[])
authServerContext = authHandler :. EmptyContext


validateToken :: AuthToken -> App (Entity Customer)
validateToken (AuthToken token) =
    runDB (getBy $ UniqueToken token)
    >>= maybe (lift . throwError $ err403 { errBody = "Invalid Auth Token" })
            return
