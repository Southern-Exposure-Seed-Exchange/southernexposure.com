{-# LANGUAGE OverloadedStrings #-}
module Emails
    ( send
    , EmailType(..)
    )
    where

import Control.Concurrent.Async (Async, async)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (withResource)
import Network.HaskellNet.SMTP.SSL (authenticate, sendMimeMail, AuthType(PLAIN))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (markdown, def)

import Config
import Models
import Server

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Emails.AccountCreated as AccountCreated


newtype EmailType =
    AccountCreated Customer


developmentEmailRecipient :: String
developmentEmailRecipient = "pavan@acorncommunity.org"


send :: EmailType -> App (Async ())
send email =
    let
        -- TODO: Add a Name to the sender address(see source for sendMimeMail)
        sender =
            "gardens@southernexposure.com"

        recipient env =
            if env /= Production then
                developmentEmailRecipient
            else
                case email of
                    AccountCreated customer ->
                        T.unpack $ customerEmail customer

        (subject, message) =
            case email of
                AccountCreated customer ->
                    AccountCreated.get
                        (L.fromStrict $ customerFirstName customer)
    in do
        cfg <- ask
        liftIO $ async $ withResource (getSmtpPool cfg) $ \conn -> do
            authSucceeded <- authenticate PLAIN (getSmtpUser cfg) (getSmtpPass cfg) conn
            if authSucceeded then
                sendMimeMail (recipient $ getEnv cfg) sender subject message
                    (renderHtml $ markdown def message) [] conn
            else
                -- TODO: Properly Log SMTP Auth Error
                print ("SMTP AUTHENTICATION FAILED" :: T.Text)
