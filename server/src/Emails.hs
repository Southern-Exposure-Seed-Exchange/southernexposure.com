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
import Data.Pool (withResource)
import Network.HaskellNet.SMTP.SSL (authenticate, sendMimeMail, AuthType(PLAIN))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)
import Text.Pandoc (readHtml, writeMarkdown, runPure)

import Config
import Models hiding (PasswordReset)
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
developmentEmailRecipient :: String
developmentEmailRecipient = "pavan@acorncommunity.org"


send :: EmailType -> App (Async ())
send email = ask >>= \cfg ->
    let
        -- TODO: Add a Name to the sender address(see source for sendMimeMail)
        -- TODO: Make Configurable
        sender =
            "gardens@southernexposure.com"

        recipient env =
            if env /= Production then
                developmentEmailRecipient
            else
                T.unpack . customerEmail $ case email of
                    AccountCreated customer ->
                        customer
                    PasswordReset customer _ ->
                        customer
                    PasswordResetSuccess customer ->
                        customer
                    OrderPlaced parameters ->
                        OrderPlaced.customer parameters

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
    in
        liftIO $ async $ withResource (getSmtpPool cfg) $ \conn -> do
            authSucceeded <- authenticate PLAIN (getSmtpUser cfg) (getSmtpPass cfg) conn
            if authSucceeded then
                sendMimeMail (recipient $ getEnv cfg) sender subject plainMessage
                    htmlMessage [] conn
            else
                -- TODO: Properly Log SMTP Auth Error
                print ("SMTP AUTHENTICATION FAILED" :: T.Text)
