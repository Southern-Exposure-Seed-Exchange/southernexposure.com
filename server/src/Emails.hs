{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Emails
    ( EmailType(..)
    , EmailData
    , getEmailData
    , send
    , sendWithRetries
    )
    where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool (withResource)
import Database.Persist (get)
import Database.Persist.Sql (SqlPersistT)
import GHC.Generics (Generic)
import Network.Mail.SMTP (login, renderAndSend)
import Network.Mail.Mime (Address(..), Mail(..), plainPart, htmlPart)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)
import Text.Pandoc (readHtml, writeMarkdown, runPure)
import Control.Monad.Except

import Config
import Models hiding (Address, PasswordReset)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Emails.AccountCreated as AccountCreated
import qualified Emails.OrderPlaced as OrderPlaced
import qualified Emails.PasswordReset as PasswordReset
import qualified Models.DB as DB
import qualified Emails.VerifyEmail as EmailVerification


data EmailType
    = AccountCreated CustomerId
    | PasswordReset CustomerId DB.PasswordResetId
    | PasswordResetSuccess CustomerId
    | OrderPlaced OrderId
    | EmailVerification CustomerId VerificationId
    deriving (Show, Generic, Eq)

instance FromJSON EmailType
instance ToJSON EmailType

data EmailData
    = AccountCreatedData Customer
    | PasswordResetData Customer DB.PasswordReset
    | PasswordResetSuccessData Customer
    | OrderPlacedData OrderPlaced.Parameters
    | EmailVerificationData Customer T.Text

getEmailData :: MonadIO m => EmailType -> SqlPersistT m (Either T.Text EmailData)
getEmailData = runExceptT . \case
    AccountCreated cId ->
        AccountCreatedData <$> tryGet "Could not find customer" cId

    PasswordReset cId prId -> do
        customer <- tryGet "Could not find customer" cId
        reset <- tryGet "Could not find password reset" prId
        pure $ PasswordResetData customer reset

    PasswordResetSuccess cId ->
        PasswordResetSuccessData <$> tryGet "Could not find customer" cId

    OrderPlaced oId -> do
        mOrder <- lift $ OrderPlaced.fetchData oId
        case mOrder of
            Nothing -> throwError "Could not find order"
            Just order -> pure $ OrderPlacedData order

    EmailVerification cId vId ->  do
        customer <- tryGet "Could not find customer" cId
        verification <- tryGet "Could not find ongoing verification" vId
        pure (EmailVerificationData customer (verificationCode verification))
  where
    tryGet errMsg sqlId =
        ExceptT $ maybe (Left errMsg) Right <$> get sqlId

-- TODO: Make Configurable
customerServiceAddress :: Address
customerServiceAddress =
    Address
        { addressName = Just "Southern Exposure Seed Exchange"
        , addressEmail = "gardens@southernexposure.com"
        }


-- | Send an email, catching errors & retrying up to 5 times.
sendWithRetries :: Config -> EmailData -> IO (Async ())
sendWithRetries cfg email =
    async $ sendEmail 5
  where
    sendEmail :: Int -> IO ()
    sendEmail retries =
        try (send cfg email) >>= \case
            Left (e :: SomeException) -> do
                print e
                when (retries > 0) $
                    threadDelay (1000 * 500) >> sendEmail (retries - 1)
            Right _ ->
                return ()


send :: Config -> EmailData -> IO ()
send cfg email =
    let
        env =
            getEnv cfg

        bcc =
            case email of
                OrderPlacedData _  | env == Production ->
                    [ Address
                        { addressName = Just "Southern Exposure Seed Exchange"
                        , addressEmail = "gardens@southernexposure.com"
                        }
                    ]
                _ ->
                    []

        domainName = L.fromStrict $ getBaseUrl cfg

        (subject, message) =
            case email of
                AccountCreatedData _ ->
                    AccountCreated.get
                PasswordResetData _ passwordReset ->
                    PasswordReset.get domainName
                        (L.fromStrict $ passwordResetCode passwordReset)
                PasswordResetSuccessData _ ->
                    PasswordReset.getSuccess
                OrderPlacedData parameters ->
                    OrderPlaced.get domainName parameters
                EmailVerificationData _ vCode ->
                    EmailVerification.get domainName (L.fromStrict vCode)

        (plainMessage, htmlMessage) =
            case email of
                OrderPlacedData {} ->
                    (htmlToMarkdown message, message)
                _ ->
                    (message, renderHtml $ markdown def message)

        htmlToMarkdown =
            either (const "") L.fromStrict . runPure . (writeMarkdown def <=< readHtml def) . L.toStrict

        mail =
            Mail
                { mailFrom = customerServiceAddress
                , mailTo = [makeRecipient (getDeveloperEmail cfg) email]
                , mailCc = []
                , mailBcc = bcc
                , mailHeaders = [("Subject", T.pack subject)]
                , mailParts = [[plainPart plainMessage, htmlPart htmlMessage]]
                }
        logger =
            getServerLogger cfg . timedLogStr
    in
    withResource (getSmtpPool cfg) $ \conn -> do
        let user = getSmtpUser cfg
        loginResponse <- login conn user (getSmtpPass cfg)
        if fst loginResponse == 235 then
            renderAndSend conn mail
        else
            logger $ "SMTP Authentication Failed for User `" <> T.pack user <> "`: " <> T.pack (show loginResponse)


makeRecipient :: Maybe T.Text -> EmailData -> Address
makeRecipient devMail email =
    let
        recipientEmail =
            customerEmail $ case email of
                AccountCreatedData customer ->
                    customer
                PasswordResetData customer _ ->
                    customer
                PasswordResetSuccessData customer ->
                    customer
                OrderPlacedData parameters ->
                    OrderPlaced.customer parameters
                EmailVerificationData customer _ ->
                    customer

        recipientName =
            case email of
                OrderPlacedData parameters ->
                    Just $
                        Models.addressFirstName (OrderPlaced.shippingAddress parameters)
                        <> " "
                        <> Models.addressLastName (OrderPlaced.shippingAddress parameters)
                _ ->
                    Nothing
    in
    case devMail of
        Just m ->
            Address
                { addressName = Nothing
                , addressEmail = m
                }
        Nothing ->
            Address
                { addressName = recipientName
                , addressEmail = recipientEmail
                }
