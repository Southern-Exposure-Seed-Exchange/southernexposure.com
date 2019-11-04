{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.StaticPages
    ( StaticPageAPI
    , staticPageRoutes
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), withObject, object)
import Database.Persist (Entity(..), SelectOpt(Asc), selectList, insert)
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Get, Post, JSON)
import Text.HTML.SanitizeXSS (sanitize)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Models (Page(..), PageId, EntityField(PageName), Unique(UniquePageSlug), slugify)
import Server (App, runDB)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Validation as V


type StaticPageAPI =
         "list" :> PageListRoute
    :<|> "new" :> NewPageRoute

type StaticPageRoutes =
         (WrappedAuthToken -> App (Cookied PageListData))
    :<|> (WrappedAuthToken -> NewPageParameters -> App (Cookied PageId))

staticPageRoutes :: StaticPageRoutes
staticPageRoutes =
         pageListRoute
    :<|> newPageRoute


-- LIST


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


-- NEW


type NewPageRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] NewPageParameters
    :> Post '[JSON] (Cookied PageId)

data NewPageParameters =
    NewPageParameters
        { nppTitle :: T.Text
        , nppSlug :: T.Text
        , nppContent :: T.Text
        } deriving (Show)

instance FromJSON NewPageParameters where
    parseJSON = withObject "NewPageParameters" $ \v -> do
        nppTitle <- v .: "title"
        nppSlug <- v .: "slug"
        nppContent <- v .: "content"
        return NewPageParameters {..}

instance Validation NewPageParameters where
    validators NewPageParameters {..} = do
        slugDoesntExist <- V.doesntExist $ UniquePageSlug nppSlug
        return
            [ ( "title"
              , [ V.required nppTitle ]
              )
            , ( "slug"
              , [ V.required nppSlug
                , ( "A Page with this slug already exists.", slugDoesntExist )
                ]
              )
            , ( "content"
              , [ V.required nppContent ]
              )
            ]

newPageRoute :: WrappedAuthToken -> NewPageParameters -> App (Cookied PageId)
newPageRoute = validateAdminAndParameters $ \_ NewPageParameters {..} -> do
    let newPage =
            Page
                { pageName = sanitize nppTitle
                , pageSlug = slugify nppSlug
                , pageContent = sanitize nppContent
                }
    runDB $ insert newPage
