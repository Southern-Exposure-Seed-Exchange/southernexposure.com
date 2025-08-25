module Postgrid
    ( verifyStructuredAddress
    ) where

import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, forM)
import Data.Aeson (ToJSON, encode)
import Servant.Client (ClientM)

import Postgrid.API.Types
import qualified Postgrid.API as API

import Config (Config(..), timedLogStr)
import Server (App)

runPostgrid
    :: (PostgridResponse res, ToJSON req, ToJSON res)
    => (Maybe API.ApiKey -> req -> ClientM res) -> req -> App (Maybe (Either API.PostgridError res))
runPostgrid clientFunc req = do
    apiKey <- asks getPostgridApiKey
    forM apiKey $ \key -> do
        logger <- asks getHelcimLogger
        log_ logger encode req
        res <- liftIO $ API.runPostgridClient (clientFunc (Just key) req)
        case res of
            Left err -> do
                log_ logger displayException err
                return $ Left err
            Right resp -> do
                log_ logger encode resp
                return $ Right resp
    where
        log_ logger encoding logee =
            liftIO $ logger $ timedLogStr $ encoding logee

verifyStructuredAddress
    :: VerifyStructuredAddressRequest
    -> App (Maybe (Either API.PostgridError VerifyStructuredAddressData))
verifyStructuredAddress req = do
    res <- runPostgrid
        (\apiKey r -> API.verifyStructuredAddress apiKey Nothing (Just $ API.BoolFlag True) Nothing r) req
    case res of
        Just (Left err) -> return $ Just (Left err)
        Just (Right resp) -> return $ Just (Right (vsarData resp))
        Nothing -> return Nothing
