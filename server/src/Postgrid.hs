module Postgrid
    ( AddressVerificationResult(..)
    , verifyAddressData
    ) where

import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, forM)
import Data.Aeson (ToJSON, encode)
import Data.ISO3166_CountryCodes (CountryCode(..))
import Servant.Client (ClientM)

import Postgrid.API (PostgridError)
import Postgrid.API.Types
import Postgrid.Cache (CachedApiEndpoint(..), cached)
import Postgrid.Utils (addressDataToVerifyStructuredAddressRequest, correctAddressData)
import qualified Postgrid.API as API

import Config (Config(..), timedLogStr)
import Models.Fields (AddressType(..), Country(..))
import Routes.CommonData (AddressData(..))
import Server (App)

runPostgrid
    :: (PostgridResponse res, ToJSON req, ToJSON res)
    => (Maybe API.ApiKey -> req -> ClientM res) -> req -> Maybe (CachedApiEndpoint req res)
    -> App (Maybe (Either API.PostgridError res))
runPostgrid clientFunc req mbCachedApiEndpoint = do
    apiKey <- asks getPostgridApiKey
    forM apiKey $ \key -> do
        logger <- asks getPostgridLogger
        log_ logger encode req
        res <- case mbCachedApiEndpoint of
            Nothing ->
                liftIO $ API.runPostgridClient (clientFunc (Just key) req)
            Just cachedApiEndpoint ->
                queryWithCache (clientFunc (Just key)) req cachedApiEndpoint
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

        queryWithCache
            :: (PostgridResponse res)
            => (req -> ClientM res) -> req -> CachedApiEndpoint req res
            -> App (Either API.PostgridError res)
        queryWithCache action request cachedApiEndpoint' = do
            cache <- asks getPostgridQueryCache
            let requestAction = API.runPostgridClient (action request)
            liftIO $ cached cache request cachedApiEndpoint' requestAction

data AddressVerificationResult
    = AddressVerifiedSuccessfully
    | AddressCorrected AddressData VerifyStructuredAddressErrors
    | AddressVerificationFailed VerifyStructuredAddressErrors
    | AddressVerificationPostgridApiFailed PostgridError

verifyAddressData
    :: AddressData
    -> AddressType
    -> App AddressVerificationResult
verifyAddressData addrData addrType = do
    case (adCountry addrData, addrType) of
        -- We only verify US shipping addresses at the moment
        (Country US, Shipping) -> proceed
        _ -> return AddressVerifiedSuccessfully
    where
        proceed = do
            res <- runPostgrid
                (\apiKey r -> API.verifyStructuredAddress apiKey Nothing (Just $ API.BoolFlag True) Nothing r)
                    (addressDataToVerifyStructuredAddressRequest addrData)
                    (Just CachedVerifyStructuredAddress)
            case res of
                Just (Left err) -> return $ AddressVerificationPostgridApiFailed err
                Just (Right resp) ->
                    let
                        verificationData = vsarData resp
                    in case vsadStatus verificationData of
                            AVVerified -> return AddressVerifiedSuccessfully
                            AVCorrected -> return $
                                AddressCorrected (correctAddressData addrData verificationData) (vsadErrors verificationData)
                            AVFailed -> return $ AddressVerificationFailed $ vsadErrors verificationData
                Nothing -> return AddressVerifiedSuccessfully
