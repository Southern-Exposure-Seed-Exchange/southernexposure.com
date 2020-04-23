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
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Database.Persist (get)
import Database.Persist.Sql (SqlReadT)
import GHC.Generics (Generic)
import Network.HaskellNet.SMTP.SSL (authenticate, sendMimeMail2, AuthType(PLAIN))
import Network.Mail.Mime (Address(..), Mail(..), plainPart, htmlPart)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)
import Text.Pandoc (readHtml, writeMarkdown, runPure)

import Config
import Models hiding (Address, PasswordReset)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Emails.AccountCreated as AccountCreated
import qualified Emails.OrderPlaced as OrderPlaced
import qualified Emails.PasswordReset as PasswordReset
import qualified Models.DB as DB


data EmailType
    = AccountCreated CustomerId
    | PasswordReset CustomerId DB.PasswordResetId
    | PasswordResetSuccess CustomerId
    | OrderPlaced OrderId
    deriving (Show, Generic, Eq)

instance FromJSON EmailType
instance ToJSON EmailType

data EmailData
    = AccountCreatedData Customer
    | PasswordResetData Customer DB.PasswordReset
    | PasswordResetSuccessData Customer
    | OrderPlacedData OrderPlaced.Parameters

getEmailData :: MonadIO m => EmailType -> SqlReadT m (Either T.Text EmailData)
getEmailData = \case
    AccountCreated cId ->
        tryGet AccountCreatedData "Could not find customer" cId
    PasswordReset cId prId ->
        get cId >>= \case
            Nothing ->
                return $ Left "Could not find customer"
            Just customer ->
                get prId >>= \case
                    Nothing ->
                        return $ Left "Could not find password reset"
                    Just reset ->
                        return . Right $ PasswordResetData customer reset
    PasswordResetSuccess cId ->
        tryGet PasswordResetSuccessData "Could not find customer" cId
    OrderPlaced oId ->
        maybe (Left "Could not find order") (Right . OrderPlacedData)
            <$> OrderPlaced.fetchData oId
  where
    tryGet constr errMsg sqlId =
        maybe (Left errMsg) (Right . constr) <$> get sqlId

-- TODO: Make Configurable
developmentEmailRecipient :: Address
developmentEmailRecipient =
    Address
        { addressName = Nothing
        , addressEmail = "pavan@acorncommunity.org"
        }

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
            Left (_ :: SomeException) ->
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
                OrderPlacedData _ ->
                    if env /= Production then
                        []
                    else
                        [ Address
                            { addressName = Just "Southern Exposure Seed Exchange"
                            , addressEmail = "gardens@southernexposure.com"
                            }
                        ]
                _ ->
                    []


        -- TODO: Make this configurable - real domain is needed in several
        -- places(here & Feeds)
        domainName =
            case getEnv cfg of
                Development ->
                    "http://localhost:7000"
                Production ->
                    "https://www.southernexposure.com"


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
                    OrderPlaced.get parameters

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
                , mailTo = [makeRecipient env email]
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
        authSucceeded <- authenticate PLAIN user (getSmtpPass cfg) conn
        if authSucceeded then
            sendMimeMail2 mail conn
        else
            logger $ "SMTP Authentication Failed for User `" <> T.pack user <> "`"


makeRecipient :: Environment -> EmailData -> Address
makeRecipient env email =
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
    if env /= Production then
        developmentEmailRecipient
    else
        Address
            { addressName = recipientName
            , addressEmail = recipientEmail
            }
