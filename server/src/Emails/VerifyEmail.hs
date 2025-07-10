{-# LANGUAGE OverloadedStrings #-}

module Emails.VerifyEmail (get) where


import qualified Data.Text.Lazy as L


get :: L.Text -> L.Text -> (String, L.Text)
get domainName verificationCode =
    ( "Southern Exposure Seed Exchange - Email Verification"
    , renderVerification domainName verificationCode
    )

renderVerification :: L.Text -> L.Text -> L.Text
renderVerification domainName verificationId =
    "Hello,\n\n" <>
    "Thank you for registering with [Southern Exposure][1]. To complete your registration, " <>
    "please verify your email address by clicking [this link to verify your email][2].\n\n" <>
    "If you did not create an account, you can safely ignore this email. The verification link " <>
    "will expire in 24 hours.\n\n" <>
    "Thank You,\n\nSouthern Exposure Seed Exchange\n\n"
        <> "[1]: http://www.southernexposure.com/\n"
        <> "[2]: " <> domainName <> "/account/verify/" <> verificationId <> "\n"

