{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module AvalaraFixtures where

import Text.RawString.QQ (r)

import qualified Data.ByteString.Lazy as LBS


errorInfoJson :: LBS.ByteString
errorInfoJson =
    [r|
{
  "error": {
    "code": "AuthenticationIncomplete",
    "message": "Authentication Incomplete.",
    "target": "HttpRequestHeaders",
    "details": [
      {
        "code": "AuthenticationIncomplete",
        "number": 34,
        "message": "Authentication Incomplete.",
        "description": "You must provide an Authorization header of the type Basic or Bearer to authenticate correctly.",
        "faultCode": "Client",
        "helpLink": "/avatax/errors/AuthenticationIncomplete",
        "severity": "Exception"
      }
    ]
  }
}
    |]


unauthenticatedPingResponse :: LBS.ByteString
unauthenticatedPingResponse =
    [r|
{"version":"19.11.0","authenticated":false,"authenticationType":"None"}
    |]

authenticatedPingResponse :: LBS.ByteString
authenticatedPingResponse =
    [r|
{
    "version": "1.0.0.0",
    "authenticated": true,
    "authenticationType": "UsernamePassword",
    "authenticatedUserName": "TestUser",
    "authenticatedUserId": 98765,
    "authenticatedAccountId": 123456789,
    "crmid": "1111"
}
    |]
