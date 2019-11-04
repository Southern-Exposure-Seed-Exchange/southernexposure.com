{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.StaticPages
    ( StaticPageAPI
    , staticPageRoutes
    ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Database.Persist (Entity(..), SelectOpt(Asc), selectList)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (WrappedAuthToken, Cookied, withAdminCookie)
import Models (Page(..), PageId, EntityField(PageName))
import Server (App, runDB)

import qualified Data.Text as T


type StaticPageAPI =
         "list" :> PageListRoute

type StaticPageRoutes =
         (WrappedAuthToken -> App (Cookied PageListData))

staticPageRoutes :: StaticPageRoutes
staticPageRoutes =
         pageListRoute


type PageListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied PageListData)

newtype PageListData =
    PageListData
        { pldPages :: [ListPage]
        } deriving (Show)

instance ToJSON PageListData where
    toJSON PageListData {..} =
        object
            [ "pages" .= pldPages
            ]

data ListPage =
    ListPage
        { lpId :: PageId
        , lpName :: T.Text
        , lpSlug :: T.Text
        , lpContent :: T.Text
        } deriving (Show)

instance ToJSON ListPage where
    toJSON ListPage {..} =
        object
            [ "id" .= lpId
            , "name" .= lpName
            , "slug" .= lpSlug
            , "content" .= lpContent
            ]

pageListRoute :: WrappedAuthToken -> App (Cookied PageListData)
pageListRoute = flip withAdminCookie $ const $
    PageListData . map makeListPage <$> runDB (selectList [] [Asc PageName])
  where
    makeListPage :: Entity Page -> ListPage
    makeListPage (Entity pageId page) =
        ListPage
            { lpId = pageId
            , lpName = pageName page
            , lpSlug = pageSlug page
            , lpContent = pageContent page
            }
