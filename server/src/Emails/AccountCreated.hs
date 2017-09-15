{-# LANGUAGE OverloadedStrings #-}
module Emails.AccountCreated where

import Data.Monoid ((<>))

import qualified Data.Text.Lazy as L


get :: L.Text -> (String, L.Text)
get firstName =
    ( "Welcome to Southern Exposure Seed Exchange"
    , render firstName
    )


render :: L.Text -> L.Text
render firstName =
    "Dear " <> firstName <> ",\n\n" <>
    "Thanks for creating an account with [Southern Exposure][1]. When you're " <>
    "signed in, you can:\n\n" <>
        "* Check out faster\n" <>
        "* Keep products in your cart as long as you like\n" <>
        "* Store multiple addresses\n" <>
        "* View your order history\n" <>
        "* Change your email, password, & other contact information\n\n" <>
    "If you have questions, you can email us at [gardens@southernexposure.com][2], " <>
    "or call us at 540-894-9480.\n\n"
        <> "[1]: http://www.southernexposure.com/\n"
        <> "[2]: mailto:gardens@southernexposure.com\n"
