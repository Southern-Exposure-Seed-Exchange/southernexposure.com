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

import Postgrid.API.Types
import Postgrid.Utils (addressDataToVerifyStructuredAddressRequest, correctAddressData)
import qualified Postgrid.API as API

import Config (Config(..), timedLogStr)
import Models.Fields (AddressType(..), Country(..))
import Routes.CommonData (AddressData(..))
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

data AddressVerificationResult
    = AddressVerifiedSuccessfully
    | AddressCorrected AddressData VerifyStructuredAddressErrors
    | AddressVerificationFailed VerifyStructuredAddressErrors

verifyAddressData
    :: AddressData
    -> AddressType
    -> App (Either API.PostgridError AddressVerificationResult)
verifyAddressData addrData addrType = do
    case (adCountry addrData, addrType) of
        -- We only verify US shipping addresses at the moment
        (Country US, Shipping) -> proceed
        _ -> return $ Right AddressVerifiedSuccessfully
    where
        proceed = do
            res <- runPostgrid
                (\apiKey r -> API.verifyStructuredAddress apiKey Nothing (Just $ API.BoolFlag True) Nothing r)
                    (addressDataToVerifyStructuredAddressRequest addrData)
            case res of
                Just (Left err) -> return $ Left err
                Just (Right resp) ->
                    let
                        verificationData = vsarData resp
                    in case vsadStatus verificationData of
                            AVVerified -> return $ Right AddressVerifiedSuccessfully
                            AVCorrected -> return $ Right $
                                AddressCorrected (correctAddressData addrData verificationData) (vsadErrors verificationData)
                            AVFailed -> return $ Right $ AddressVerificationFailed $ vsadErrors verificationData
                Nothing -> return $ Right AddressVerifiedSuccessfully
