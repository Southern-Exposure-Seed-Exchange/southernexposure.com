{-# LANGUAGE OverloadedStrings #-}
module Emails.PasswordReset (get, getSuccess) where

import Data.Monoid ((<>))

import qualified Data.Text.Lazy as L


get :: L.Text -> L.Text -> L.Text -> (String, L.Text)
get firstName domainName resetCode =
    ( "Southern Exposure Seed Exchange - Password Reset Link"
    , render firstName domainName resetCode
    )


render :: L.Text -> L.Text -> L.Text -> L.Text
render firstName domainName resetCode =
    "Hello " <> firstName <> ",\n\n" <>
    "We have received a Password Reset request for your [Southern Exposure][1] " <>
    "account. You can change your password by following [this link to our Password " <>
    "Reset Page][2].\n\n" <>
    "If you did not initiate this request, you can ignore this email & the request " <>
    "link will automatically expire in 15 minutes.\n\n" <>
    "Thank You,\n\nSouthern Exposure Seed Exchange\n\n"
        <> "[1]: http://www.southernexposure.com/\n"
        <> "[2]: " <> domainName <> "/account/reset-password/?code=" <> resetCode <> "\n"


getSuccess :: L.Text -> (String, L.Text)
getSuccess firstName =
    ( "Your Password was Successfully Reset"
    , renderSuccess firstName
    )

renderSuccess :: L.Text -> L.Text
renderSuccess firstName =
    "Hello " <> firstName  <> ",\n\n" <>
    "The password for your [Southern Exposure][1] account has been changed by " <>
    "a password reset request. If you did not initiate this request, we recommend " <>
    "changing the password for both your email account & your Southern Exposure account, " <>
    "[contact our support][2] for more help.\n\n" <>
    "Thank You,\n\nSouthern Exposure Seed Exchange\n\n"
        <> "[1]: http://www.southernexposure.com/\n"
        <> "[2]: mailto:gardens@southernexposure.com\n"

