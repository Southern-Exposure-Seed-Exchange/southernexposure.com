{-# LANGUAGE OverloadedStrings #-}
module Emails
    ( send
    , EmailType(..)
    )
    where

import Control.Concurrent.Async (Async, async)
import Control.Monad ((<=<))
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Pool (withResource)
import Network.HaskellNet.SMTP.SSL (authenticate, sendMimeMail2, AuthType(PLAIN))
import Network.Mail.Mime (Address(..), Mail(..), plainPart, htmlPart)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)
import Text.Pandoc (readHtml, writeMarkdown, runPure)

import Config
import Models hiding (Address, PasswordReset)
import Server

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Emails.AccountCreated as AccountCreated
import qualified Emails.OrderPlaced as OrderPlaced
import qualified Emails.PasswordReset as PasswordReset
import qualified Models.DB as DB


data EmailType
    = AccountCreated Customer
    | PasswordReset Customer DB.PasswordReset
    | PasswordResetSuccess Customer
    | OrderPlaced OrderPlaced.Parameters


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



send :: EmailType -> App (Async ())
send email = ask >>= \cfg ->
    let
        env =
            getEnv cfg

        bcc =
            case email of
                OrderPlaced _ ->
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
                AccountCreated _ ->
                    AccountCreated.get
                PasswordReset _ passwordReset ->
                    PasswordReset.get domainName
                        (L.fromStrict $ passwordResetCode passwordReset)
                PasswordResetSuccess _ ->
                    PasswordReset.getSuccess
                OrderPlaced parameters ->
                    OrderPlaced.get parameters

        (plainMessage, htmlMessage) =
            case email of
                OrderPlaced {} ->
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
    in do
        logger <- msgLoggerIO
        liftIO $ async $ withResource (getSmtpPool cfg) $ \conn -> do
            let user = getSmtpUser cfg
            authSucceeded <- authenticate PLAIN user (getSmtpPass cfg) conn
            if authSucceeded then
                sendMimeMail2 mail conn
            else
                logger $ "SMTP Authentication Failed for User `" <> T.pack user <> "`"


makeRecipient :: Environment -> EmailType -> Address
makeRecipient env email =
    let
        recipientEmail =
            customerEmail $ case email of
                AccountCreated customer ->
                    customer
                PasswordReset customer _ ->
                    customer
                PasswordResetSuccess customer ->
                    customer
                OrderPlaced parameters ->
                    OrderPlaced.customer parameters

        recipientName =
            case email of
                OrderPlaced parameters ->
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
