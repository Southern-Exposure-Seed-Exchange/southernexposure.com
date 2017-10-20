{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Customers
    ( CustomerAPI
    , customerRoutes
    ) where

import Control.Monad ((>=>), when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), withObject, object)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Database.Persist
    ( (=.), (==.), Entity(..), get, getBy, insertUnique, insert_, update
    , delete, deleteWhere
    )
import Database.Persist.Sql (toSqlKey)
import Servant
    ( (:>), (:<|>)(..), AuthProtect, ReqBody, JSON, Get, Post, Put, errBody
    , err403, err500, QueryParam
    )

import Auth
import Models
import Models.Fields (Country(..), Region, ArmedForcesRegionCode, armedForcesRegion, Cents(..))
import Routes.CommonData (AuthorizationData, toAuthorizationData, AddressData, toAddressData)
import Routes.Utils (generateUniqueToken, hashPassword)
import Server
import Validation (Validation(..))

import qualified Crypto.BCrypt as BCrypt
import qualified Data.ISO3166_CountryCodes as CountryCodes
import qualified Data.StateCodes as StateCodes
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Esqueleto as E
import qualified Emails
import qualified Models.Fields as Fields
import qualified Models.ProvinceCodes as ProvinceCodes
import qualified Validation as V


type CustomerAPI =
         "locations" :> LocationRoute
    :<|> "register" :> RegisterRoute
    :<|> "login" :> LoginRoute
    :<|> "authorize" :> AuthorizeRoute
    :<|> "reset-request" :> ResetRequestRoute
    :<|> "reset-password" :> ResetPasswordRoute
    :<|> "my-account" :> MyAccountRoute
    :<|> "edit" :> EditDetailsRoute
    :<|> "contact" :> ContactDetailsRoute
    :<|> "contact-edit" :> ContactEditRoute

type CustomerRoutes =
         App LocationData
    :<|> (RegistrationParameters -> App AuthorizationData)
    :<|> (LoginParameters -> App AuthorizationData)
    :<|> (AuthorizeParameters -> App AuthorizationData)
    :<|> (ResetRequestParameters -> App ())
    :<|> (ResetPasswordParameters -> App AuthorizationData)
    :<|> (AuthToken -> Maybe Int64 -> App MyAccountDetails)
    :<|> (AuthToken -> EditDetailsParameters -> App ())
    :<|> (AuthToken -> App ContactDetailsData)
    :<|> (AuthToken -> ContactEditParameters -> App ())

customerRoutes :: CustomerRoutes
customerRoutes =
         locationRoute
    :<|> registrationRoute
    :<|> loginRoute
    :<|> authorizeRoute
    :<|> resetRequestRoute
    :<|> resetPasswordRoute
    :<|> myAccountRoute
    :<|> editDetailsRoute
    :<|> contactDetailsRoute
    :<|> contactEditRoute


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
        , rpCartToken :: Maybe T.Text
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
                <*> v .:? "sessionToken"

instance Validation RegistrationParameters where
    -- TODO: Better validation, validate emails, compare to Zencart
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

type RegisterRoute =
       ReqBody '[JSON] RegistrationParameters
    :> Post '[JSON] AuthorizationData

registrationRoute :: RegistrationParameters -> App AuthorizationData
registrationRoute = validate >=> \parameters -> do
    encryptedPass <- hashPassword $ rpPassword parameters
    authToken <- generateUniqueToken UniqueToken
    let customer = Customer
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
    maybeCustomerId <- runDB $ insertUnique customer
    case maybeCustomerId of
        Nothing ->
            serverError err500
        Just customerId ->
            runDB (maybeMergeCarts customerId $ rpCartToken parameters) >>
            Emails.send (Emails.AccountCreated customer) >>
            return (toAuthorizationData $ Entity customerId customer)


-- LOGIN


data LoginParameters =
    LoginParameters
        { lpEmail :: T.Text
        , lpPassword :: T.Text
        , lpCartToken :: Maybe T.Text
        }

instance FromJSON LoginParameters where
    parseJSON =
        withObject "LoginParameters" $ \v ->
            LoginParameters
                <$> v .: "email"
                <*> v .: "password"
                <*> v .:? "sessionToken"

type LoginRoute =
       ReqBody '[JSON] LoginParameters
    :> Post '[JSON] AuthorizationData

loginRoute :: LoginParameters -> App AuthorizationData
loginRoute LoginParameters { lpEmail, lpPassword, lpCartToken } =
    let
        authorizationError =
            V.singleError "Invalid Email or Password."
        resetRequiredError =
            void . hashAnyways $ V.singleError
                "Sorry, you need to reset your password before logging in."
    in do
        maybeCustomer <- runDB . getBy $ UniqueEmail lpEmail
        case maybeCustomer of
            Just e@(Entity customerId customer) -> do
                isValid <- validatePassword e
                when (customerEncryptedPassword customer == "") resetRequiredError
                if isValid then
                    runDB (maybeMergeCarts customerId lpCartToken) >>
                    return (toAuthorizationData e)
                else
                    authorizationError
            Nothing ->
                hashAnyways authorizationError
    where hashAnyways returnValue = do
            hash <- liftIO . BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy
                $ encodeUtf8 lpPassword
            const returnValue $! hash
          validatePassword (Entity customerId customer) =
            let
                hashedPassword =
                    encodeUtf8 $ customerEncryptedPassword customer
                isValid =
                    BCrypt.validatePassword hashedPassword
                        (encodeUtf8 lpPassword)
                usesPolicy =
                    BCrypt.hashUsesPolicy BCrypt.slowerBcryptHashingPolicy
                        hashedPassword
            in
                if isValid && usesPolicy then
                    return True
                else if isValid then do
                    maybeNewHash <- liftIO . BCrypt.hashPasswordUsingPolicy
                        BCrypt.slowerBcryptHashingPolicy
                        $ encodeUtf8 lpPassword
                    newHash <- maybe
                        (serverError $ err500 { errBody = "Misconfigured Hashing Policy" })
                        (return . decodeUtf8) maybeNewHash
                    runDB $ update customerId [CustomerEncryptedPassword =. newHash]
                    return True
                else
                    return False


-- AUTHORIZE


data AuthorizeParameters =
    AuthorizeParameters
        { apUserId :: Int64
        , apToken :: T.Text
        }

instance FromJSON AuthorizeParameters where
    parseJSON = withObject "AuthorizeParameters" $ \v ->
        AuthorizeParameters
            <$> v .: "userId"
            <*> v .: "token"

type AuthorizeRoute =
       ReqBody '[JSON] AuthorizeParameters
    :> Post '[JSON] AuthorizationData

authorizeRoute :: AuthorizeParameters -> App AuthorizationData
authorizeRoute AuthorizeParameters { apUserId, apToken } =
    let
        userId = toSqlKey apUserId
    in do
        maybeCustomer <- runDB $ get userId
        case maybeCustomer of
            Just customer ->
                if apToken == customerAuthToken customer then
                    return $ toAuthorizationData (Entity userId customer )
                else
                    serverError err403
            Nothing ->
                serverError err403


-- RESET PASSWORD


newtype ResetRequestParameters =
    ResetRequestParameters
        { rrpEmail :: T.Text
        }

instance FromJSON ResetRequestParameters where
    parseJSON = withObject "ResetRequestParameters" $ \v ->
        ResetRequestParameters <$> v .: "email"

instance Validation ResetRequestParameters where
    validators parameters =
        return [ ( "email", [V.required $ rrpEmail parameters] ) ]

type ResetRequestRoute =
       ReqBody '[JSON] ResetRequestParameters
    :> Post '[JSON] ()

resetRequestRoute :: ResetRequestParameters -> App ()
resetRequestRoute = validate >=> \parameters -> do
    maybeCustomer <- runDB . getBy $ UniqueEmail $ rrpEmail parameters
    case maybeCustomer of
        Nothing ->
            (UUID.toText <$> liftIO UUID4.nextRandom)
            >> (addUTCTime (15 * 60) <$> liftIO getCurrentTime)
            >> return ()
        Just (Entity customerId customer) -> do
            runDB $ deleteWhere [PasswordResetCustomerId ==. customerId]
            resetCode <- UUID.toText <$> liftIO UUID4.nextRandom
            expirationTime <- addUTCTime (15 * 60) <$> liftIO getCurrentTime
            let passwordReset =
                    PasswordReset
                        { passwordResetCustomerId = customerId
                        , passwordResetExpirationTime = expirationTime
                        , passwordResetCode = resetCode
                        }
            runDB $ insert_ passwordReset
            void . Emails.send $ Emails.PasswordReset customer passwordReset


data ResetPasswordParameters =
    ResetPasswordParameters
        { rppPassword :: T.Text
        , rppResetCode :: T.Text
        }

instance FromJSON ResetPasswordParameters where
    parseJSON = withObject "ResetPasswordParameters" $ \v ->
        ResetPasswordParameters
            <$> v .: "password"
            <*> v .: "resetCode"

instance Validation ResetPasswordParameters where
    validators parameters =
        return
            [ ( "password"
              , [ V.required $ rppPassword parameters
                , V.minimumLength 8 $ rppPassword parameters
                ]
              )
            ]

type ResetPasswordRoute =
       ReqBody '[JSON] ResetPasswordParameters
    :> Post '[JSON] AuthorizationData

resetPasswordRoute :: ResetPasswordParameters -> App AuthorizationData
resetPasswordRoute = validate >=> \parameters ->
    let
        invalidCodeError =
            V.singleError $
                "Your reset code has expired, please try requesting a new " <>
                "password reset link."
    in do
        maybeResetRequest <- runDB . getBy . UniqueResetCode $ rppResetCode parameters
        case maybeResetRequest of
            Nothing ->
                invalidCodeError
            Just (Entity resetId passwordReset) -> do
                currentTime <- liftIO getCurrentTime
                if currentTime < passwordResetExpirationTime passwordReset then do
                    token <- generateUniqueToken UniqueToken
                    newHash <- hashPassword $ rppPassword parameters
                    let customerId = passwordResetCustomerId passwordReset
                    runDB $ update customerId
                        [ CustomerAuthToken =. token
                        , CustomerEncryptedPassword =. newHash
                        ]
                        >> delete resetId
                    maybeCustomer <- runDB $ get customerId
                    flip (maybe invalidCodeError) maybeCustomer $ \customer -> do
                        void . Emails.send $ Emails.PasswordResetSuccess customer
                        return . toAuthorizationData
                            $ Entity customerId customer
                else
                    runDB (delete resetId) >> invalidCodeError


-- MY ACCOUNT


newtype MyAccountDetails =
    MyAccountDetails
        { madOrderDetails :: [MyAccountOrderDetails]
        }

instance ToJSON MyAccountDetails where
    toJSON details =
        object
            [ "orderDetails" .= madOrderDetails details
            ]

data MyAccountOrderDetails =
    MyAccountOrderDetails
        { maodId :: OrderId
        , maodShippingAddress :: AddressData
        , maodOrderStatus :: Fields.OrderStatus
        , maodOrderTotal :: Cents
        , maodCreated :: UTCTime
        }

instance ToJSON MyAccountOrderDetails where
    toJSON details =
        object
            [ "id" .= maodId details
            , "shippingAddress" .= maodShippingAddress details
            , "status" .= maodOrderStatus details
            , "total" .= maodOrderTotal details
            , "created" .= maodCreated details
            ]

type MyAccountRoute =
       AuthProtect "auth-token"
    :> QueryParam "limit" Int64
    :> Get '[JSON] MyAccountDetails

myAccountRoute :: AuthToken -> Maybe Int64 -> App MyAccountDetails
myAccountRoute authToken maybeLimit = do
    (Entity customerId _) <- validateToken authToken
    MyAccountDetails <$> getOrderDetails customerId
    where getOrderDetails customerId = runDB $ do
            let limit = fromMaybe 4 maybeLimit
            orderData <- E.select $ E.from $
                \(o `E.InnerJoin` op `E.InnerJoin` ol `E.InnerJoin` sa) -> do
                    E.on $ sa E.^. AddressId E.==. o E.^. OrderShippingAddressId
                    E.on $ ol E.^. OrderLineItemOrderId E.==. o E.^. OrderId
                    E.on $ op E.^. OrderProductOrderId E.==. o E.^. OrderId
                    E.groupBy (o E.^. OrderId, sa E.^. AddressId)
                    E.where_ $ o E.^. OrderCustomerId E.==. E.val customerId
                    E.orderBy [E.desc $ o E.^. OrderCreatedAt]
                    when (limit > 0) $ E.limit limit
                    return
                        ( o E.^. OrderId
                        , sa
                        , o E.^. OrderStatus
                        , calculateTotal op ol
                        , o E.^. OrderCreatedAt
                        )
            return $ map makeDetails orderData
          calculateTotal orderProduct orderLineItem =
            let
                productTax =
                    orderProduct E.^. OrderProductTax
                productQuantity =
                    orderProduct E.^. OrderProductQuantity
                productPrice =
                    orderProduct E.^. OrderProductPrice
                subTotal =
                    E.sum_ $ productTax E.+.
                        (E.castNum productQuantity E.*.  productPrice)
                surchargeTotal =
                    E.sum_ $ orderLineItem E.^. OrderLineItemAmount
            in
                subTotal E.+. surchargeTotal
          makeDetails (orderId, shippingAddress, status, total, createdAt) =
              MyAccountOrderDetails
                { maodId = E.unValue orderId
                , maodShippingAddress = toAddressData shippingAddress
                , maodOrderStatus = E.unValue status
                , maodOrderTotal = Cents . maybe 0 round $ (E.unValue total :: Maybe Rational)
                , maodCreated = E.unValue createdAt
                }


-- EDIT DETAILS


data EditDetailsParameters =
    EditDetailsParameters
        { edpEmail :: Maybe T.Text
        , edpPassword :: Maybe T.Text
        }

instance FromJSON EditDetailsParameters where
    parseJSON = withObject "EditDetailsParameters" $ \v ->
        EditDetailsParameters
            <$> v .:? "email"
            <*> v .:? "password"

instance Validation (EditDetailsParameters, Customer) where
    validators (parameters, customer) = do
        maybeEmailDoesntExist <- mapM (V.doesntExist . UniqueEmail) $ edpEmail parameters
        return
            [ ( "email"
              , [ ( "An Account with this Email already exists."
                  , flip (maybe False) (edpEmail parameters) $ \e ->
                        fromMaybe False maybeEmailDoesntExist
                        && (e /= customerEmail customer)
                  )
                ]
              )
            , ( "password"
              , [ maybe ("", False) (V.minimumLength 8) $ edpPassword parameters
                ]
              )
            ]


type EditDetailsRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] EditDetailsParameters
    :> Put '[JSON] ()

editDetailsRoute :: AuthToken -> EditDetailsParameters -> App ()
editDetailsRoute token p = do
    (Entity customerId customer) <- validateToken token
    (parameters, _) <- validate (p, customer)
    maybeHash <- mapM hashPassword $ edpPassword parameters
    void . runDB . update customerId $ updateFields (edpEmail parameters) maybeHash
    where updateFields maybeEmail maybePassword =
            maybe [] (\e -> [CustomerEmail =. e]) maybeEmail
                ++ maybe [] (\e -> [CustomerEncryptedPassword =. e]) maybePassword


-- CONTACT DETAILS


data ContactDetailsData =
    ContactDetailsData
        { cddFirstName :: T.Text
        , cddLastName :: T.Text
        , cddAddressOne :: T.Text
        , cddAddressTwo :: T.Text
        , cddCity :: T.Text
        , cddState :: Region
        , cddZipCode :: T.Text
        , cddCountry :: Country
        , cddTelephone :: T.Text
        }

instance ToJSON ContactDetailsData where
    toJSON contact =
        object [ "firstName" .= cddFirstName contact
               , "lastName" .= cddLastName contact
               , "addressOne" .= cddAddressOne contact
               , "addressTwo" .= cddAddressTwo contact
               , "city" .= cddCity contact
               , "state" .= cddState contact
               , "zipCode" .= cddZipCode contact
               , "country" .= cddCountry contact
               , "telephone" .= cddTelephone contact
               ]

customerToContactDetails :: Entity Customer -> ContactDetailsData
customerToContactDetails (Entity _ customer) =
    ContactDetailsData
        { cddFirstName = customerFirstName customer
        , cddLastName = customerLastName customer
        , cddAddressOne = customerAddressOne customer
        , cddAddressTwo = customerAddressTwo customer
        , cddCity = customerCity customer
        , cddState = customerState customer
        , cddZipCode = customerZipCode customer
        , cddCountry = customerCountry customer
        , cddTelephone = customerTelephone customer
        }

type ContactDetailsRoute =
       AuthProtect "auth-token"
    :> Get '[JSON] ContactDetailsData

contactDetailsRoute :: AuthToken -> App ContactDetailsData
contactDetailsRoute token =
    customerToContactDetails <$> validateToken token


-- EDIT CONTACT DETAILS


data ContactEditParameters =
    ContactEditParameters
        { cepFirstName :: T.Text
        , cepLastName :: T.Text
        , cepAddressOne :: T.Text
        , cepAddressTwo :: T.Text
        , cepCity :: T.Text
        , cepState :: Region
        , cepZipCode :: T.Text
        , cepCountry :: Country
        , cepTelephone :: T.Text
        }

instance FromJSON ContactEditParameters where
    parseJSON =
        withObject "ContactEditParameters" $ \v ->
            ContactEditParameters
                <$> v .: "firstName"
                <*> v .: "lastName"
                <*> v .: "addressOne"
                <*> v .: "addressTwo"
                <*> v .: "city"
                <*> v .: "state"
                <*> v .: "zipCode"
                <*> v .: "country"
                <*> v .: "telephone"

instance Validation ContactEditParameters where
    validators parameters =
        return
            [ ( "firstName", [ V.required $ cepFirstName parameters ])
            , ( "lastName", [ V.required $ cepLastName parameters ])
            , ( "addressOne", [ V.required $ cepAddressOne parameters ])
            , ( "city", [ V.required $ cepCity parameters ])
            , ( "zipCode", [ V.required $ cepZipCode parameters ])
            , ( "telephone", [ V.required $ cepTelephone parameters ])
            ]

type ContactEditRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] ContactEditParameters
    :> Post '[JSON] ()

contactEditRoute :: AuthToken -> ContactEditParameters -> App ()
contactEditRoute token = validate >=> \parameters -> do
    (Entity customerId _) <- validateToken token
    void . runDB $ update customerId
        [ CustomerFirstName =. cepFirstName parameters
        , CustomerLastName =. cepLastName parameters
        , CustomerAddressOne =. cepAddressOne parameters
        , CustomerAddressTwo =. cepAddressTwo parameters
        , CustomerCity =. cepCity parameters
        , CustomerState =. cepState parameters
        , CustomerZipCode =. cepZipCode parameters
        , CustomerCountry =. cepCountry parameters
        , CustomerTelephone =. cepTelephone parameters
        ]


-- UTILS


maybeMergeCarts :: CustomerId -> Maybe T.Text -> AppSQL ()
maybeMergeCarts customerId =
    maybe (return ()) (`mergeCarts` customerId)
