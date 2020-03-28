{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Settings
    ( SettingsAPI
    , settingsRoutes
    ) where

import Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)

import Auth (WrappedAuthToken, Cookied, withAdminCookie)
import Cache (Caches(getSettingsCache))
import Models
import Routes.Utils (sanitize)
import Server (App, readCache, writeSetting)


type SettingsAPI =
         "data" :> SettingsDataRoute
    :<|> "update" :> SettingsUpdateRoute

type SettingsRoutes =
         (WrappedAuthToken -> App (Cookied SettingsData))
    :<|> (WrappedAuthToken -> SettingsData -> App (Cookied ()))

settingsRoutes :: SettingsRoutes
settingsRoutes =
         settingsDataRoute
    :<|> settingsUpdateRoute


-- COMMON

newtype SettingsData =
    SettingsData
        { fromSettingsData :: Settings
        } deriving (Show)

instance ToJSON SettingsData where
    toJSON SettingsData {..} =
        let Settings {..} = fromSettingsData
        in
        object
            [ "disableCheckout" .= settingsDisableCheckout
            , "disabledCheckoutMessage" .= settingsDisabledCheckoutMessage
            ]

instance FromJSON SettingsData where
    parseJSON = withObject "SettingsData" $ \v -> do
        settingsDisableCheckout <- v .: "disableCheckout"
        settingsDisabledCheckoutMessage <- v .: "disabledCheckoutMessage"
        return . SettingsData $ Settings {..}


-- DATA

type SettingsDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied SettingsData)

settingsDataRoute :: WrappedAuthToken -> App (Cookied SettingsData)
settingsDataRoute = flip withAdminCookie $ \_ ->
    SettingsData <$> readCache getSettingsCache


-- UPDATE

type SettingsUpdateRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] SettingsData
    :> Post '[JSON] (Cookied ())

settingsUpdateRoute :: WrappedAuthToken -> SettingsData -> App (Cookied ())
settingsUpdateRoute t SettingsData {..} = withAdminCookie t $ \_ ->
    writeSetting $ const $
        fromSettingsData
            { settingsDisabledCheckoutMessage =
                sanitize $ settingsDisabledCheckoutMessage fromSettingsData
            }
