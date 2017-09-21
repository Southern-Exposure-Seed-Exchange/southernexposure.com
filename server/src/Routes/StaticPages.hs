{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Routes.StaticPages
    ( StaticPageAPI
    , staticPageRoutes
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Database.Persist (Entity(..), getBy)
import Servant ((:>), Capture, Get, JSON, throwError, err404)

import Models
import Server

import qualified Data.Text as T


type StaticPageAPI =
    "details" :> PageDetailsRoute

type StaticPageRoutes =
    (T.Text -> App PageDetailsData)

staticPageRoutes :: StaticPageRoutes
staticPageRoutes =
    pageDetailsRoute


-- TODO: Just use Entity Page instead of creating new type?
newtype PageDetailsData =
    StaticPageDetailsData
        { spddPage :: Entity Page
        } deriving (Show)

instance ToJSON PageDetailsData where
    toJSON StaticPageDetailsData { spddPage } =
        object [ "page" .= toJSON spddPage ]

type PageDetailsRoute =
    Capture "slug" T.Text :> Get '[JSON] PageDetailsData

pageDetailsRoute :: T.Text -> App PageDetailsData
pageDetailsRoute slug = do
    maybePage <- runDB . getBy $ UniquePageSlug slug
    case maybePage of
        Nothing ->
            throwError err404
        Just pageEntity ->
            return $ StaticPageDetailsData pageEntity
