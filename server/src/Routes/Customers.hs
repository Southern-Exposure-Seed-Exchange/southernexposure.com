{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Customers
    ( CustomerAPI
    , customerRoutes
    ) where

import Control.Monad ((>=>))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), withObject, object)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist (getBy, insertUnique)
import Servant ((:>), (:<|>)(..), ReqBody, JSON, Get, Post, errBody, err500, throwError)

import Models
import Models.Fields (Country(..), Region, ArmedForcesRegionCode, armedForcesRegion)
import Server
import Validation (Validation(..))

import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Models.ProvinceCodes as ProvinceCodes
import qualified Validation as V


type CustomerAPI =
         "locations" :> LocationRoute
    :<|> "register" :> RegisterRoute

type CustomerRoutes =
         App LocationData
    :<|> (RegistrationParameters -> App RegistrationData)

customerRoutes :: CustomerRoutes
customerRoutes =
         locationRoute
    :<|> registrationRoute


-- LOCATIONS


data Location a =
    Location
        { lCode :: a
        , lName :: T.Text
        }

instance (Show a) => ToJSON (Location a) where
    toJSON Location { lCode, lName } =
        object [ "code" .= toJSON (show lCode)
               , "name" .= toJSON lName
               ]

data LocationData =
    LocationData
        { ldCountries :: [Location CountryCodes.CountryCode]
        , ldUSStates :: [Location StateCodes.StateCode]
        , ldAFRegions :: [Location ArmedForcesRegionCode]
        , ldCAProvinces :: [Location ProvinceCodes.ProvinceCode]
        }

instance ToJSON LocationData where
    toJSON LocationData { ldCountries, ldUSStates, ldAFRegions, ldCAProvinces } =
        object [ "countries" .= toJSON ldCountries
               , "states" .= toJSON ldUSStates
               , "armedForces" .= toJSON ldAFRegions
               , "provinces" .= toJSON ldCAProvinces
               ]

type LocationRoute =
    Get '[JSON] LocationData

locationRoute :: App LocationData
locationRoute =
    let
        initialCountries =
            [CountryCodes.US, CountryCodes.CA, CountryCodes.MX]

        countries =
            map (\c -> Location c . T.pack $ CountryCodes.readableCountryName c)
                $ initialCountries
                ++ filter (`notElem` initialCountries)
                    (enumFromTo minBound maxBound)
        states =
            map (uncurry $ flip Location) StateCodes.stateList

        armedForcesRegions =
            map (\r -> Location r $ armedForcesRegion r)
                $ enumFrom minBound

        provinces =
            map (\c -> Location c $ ProvinceCodes.toName c) ProvinceCodes.all
    in
        return LocationData
            { ldCountries = countries
            , ldUSStates = states
            , ldAFRegions = armedForcesRegions
            , ldCAProvinces = provinces
            }



-- REGISTER


data RegistrationParameters =
    RegistrationParameters
        { rpEmail :: T.Text
        , rpPassword :: T.Text
        , rpFirstName :: T.Text
        , rpLastName :: T.Text
        , rpAddressOne :: T.Text
        , rpAddressTwo :: T.Text
        , rpCity :: T.Text
        , rpState :: Region
        , rpZipCode :: T.Text
        , rpCountry :: Country
        , rpTelephone :: T.Text
        } deriving (Show)

instance FromJSON RegistrationParameters where
    parseJSON =
        withObject "RegistrationParameters" $ \v ->
            RegistrationParameters
                <$> v .: "email"
                <*> v .: "password"
                <*> v .: "firstName"
                <*> v .: "lastName"
                <*> v .: "addressOne"
                <*> v .: "addressTwo"
                <*> v .: "city"
                <*> v .: "state"
                <*> v .: "zipCode"
                <*> v .: "country"
                <*> v .: "telephone"

instance Validation RegistrationParameters where
    validators parameters = do
        emailDoesntExist <- V.doesntExist $ UniqueEmail $ rpEmail parameters
        return
            [ ( "email"
              , [ V.required $ rpEmail parameters
                  , ( "An Account with this Email already exists."
                    , emailDoesntExist )
                ]
              )
            , ( "password"
              , [ V.required $ rpPassword parameters
                , V.minimumLength 8 $ rpPassword parameters
                ]
              )
            , ( "firstName", [ V.required $ rpFirstName parameters ])
            , ( "lastName", [ V.required $ rpLastName parameters ])
            , ( "addressOne", [ V.required $ rpAddressOne parameters ])
            , ( "city", [ V.required $ rpCity parameters ])
            , ( "zipCode", [ V.required $ rpZipCode parameters ])
            , ( "telephone", [ V.required $ rpTelephone parameters ])
            ]

data RegistrationData =
    RegistrationData
        { rdId :: CustomerId
        , rdFirstName :: T.Text
        , rdLastName :: T.Text
        , rdEmail :: T.Text
        , rdToken :: T.Text
        } deriving (Show)

instance ToJSON RegistrationData where
    toJSON regData =
        object [ "id" .= toJSON (rdId regData)
               , "firstName" .= toJSON (rdFirstName regData)
               , "lastName" .= toJSON (rdLastName regData)
               , "email" .= toJSON (rdEmail regData)
               , "token" .= toJSON (rdToken regData)
               ]

type RegisterRoute =
       ReqBody '[JSON] RegistrationParameters
    :> Post '[JSON] RegistrationData

registrationRoute :: RegistrationParameters -> App RegistrationData
registrationRoute = validate >=> \parameters -> do
    encryptedPass <- hashPassword $ rpPassword parameters
    authToken <- generateToken
    maybeCustomerId <- runDB . insertUnique $
        Customer
            { customerEmail = rpEmail parameters
            , customerEncryptedPassword = encryptedPass
            , customerAuthToken = authToken
            , customerIsAdmin = False
            , customerFirstName = rpFirstName parameters
            , customerLastName = rpLastName parameters
            , customerAddressOne = rpAddressOne parameters
            , customerAddressTwo = rpAddressTwo parameters
            , customerCity = rpCity parameters
            , customerState = rpState parameters
            , customerZipCode = rpZipCode parameters
            , customerCountry = rpCountry parameters
            , customerTelephone = rpTelephone parameters
            }
    case maybeCustomerId of
        Nothing ->
            lift $ throwError err500
        Just customerId ->
            return RegistrationData
                { rdId = customerId
                , rdFirstName = rpFirstName parameters
                , rdLastName = rpLastName parameters
                , rdEmail = rpEmail parameters
                , rdToken = authToken
                }

generateToken :: App T.Text
generateToken = do
    token <- UUID.toText <$> liftIO UUID4.nextRandom
    maybeCustomer <- runDB . getBy $ UniqueToken token
    case maybeCustomer of
        Just _ ->
            generateToken
        Nothing ->
            return token

hashPassword :: T.Text -> App T.Text
hashPassword password = do
    maybePass <- liftIO . hashPasswordUsingPolicy slowerBcryptHashingPolicy
        $ encodeUtf8 password
    case maybePass of
        Nothing ->
            lift . throwError
                $ err500 { errBody = "Misconfigured Hashing Policy" }
        Just pass ->
            return $ decodeUtf8 pass
