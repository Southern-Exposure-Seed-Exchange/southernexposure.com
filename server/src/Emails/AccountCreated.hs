{-# LANGUAGE OverloadedStrings #-}
module Emails.AccountCreated (get) where

import Data.Monoid ((<>))

import qualified Data.Text.Lazy as L


get :: (String, L.Text)
get =
    ( "Welcome to Southern Exposure Seed Exchange"
    , render
    )


render :: L.Text
render =
    "Hello,\n\n" <>
    "Thanks for creating an account with [Southern Exposure][1]. When you're " <>
    "signed in, you can:\n\n" <>
        "* Check out faster\n" <>
        "* Keep products in your cart as long as you like\n" <>
        "* Store multiple addresses\n" <>
        "* View your order history\n" <>
        "* Change your email, password, & address information\n\n" <>
    "If you have questions, you can email us at [gardens@southernexposure.com][2], " <>
    "or call us at [540-894-9480][3].\n\n"
        <> "[1]: http://www.southernexposure.com/\n"
        <> "[2]: mailto:gardens@southernexposure.com\n"
        <> "[3]: tel://5408949480\n"
