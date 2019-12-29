{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.StaticPages
    ( StaticPageAPI
    , staticPageRoutes
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), withObject, object)
import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Database.Persist
    ( (=.), Entity(..), SelectOpt(Asc), Update, selectList, insert, get, update
    , getBy
    )
import Servant ((:<|>)(..), (:>), AuthProtect, ReqBody, Capture, Get, Post, Patch, JSON, err404)
import Text.HTML.SanitizeXSS (sanitize)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Models (Page(..), PageId, EntityField(..), Unique(UniquePageSlug), slugify)
import Routes.Utils (mapUpdateWith)
import Server (App, runDB, serverError)
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Validation as V


type StaticPageAPI =
         "list" :> PageListRoute
    :<|> "new" :> NewPageRoute
    :<|> "edit" :> EditPageDataRoute
    :<|> "edit" :> EditPageRoute

type StaticPageRoutes =
         (WrappedAuthToken -> App (Cookied PageListData))
    :<|> (WrappedAuthToken -> NewPageParameters -> App (Cookied PageId))
    :<|> (WrappedAuthToken -> PageId -> App (Cookied EditPageData))
    :<|> (WrappedAuthToken -> EditPageParameters -> App (Cookied ()))

staticPageRoutes :: StaticPageRoutes
staticPageRoutes =
         pageListRoute
    :<|> newPageRoute
    :<|> editPageDataRoute
    :<|> editPageRoute


-- LIST


type PageListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied PageListData)

data PageListData =
    PageListData
        { pldPages :: [ListPage]
        , pldHomePageId :: Maybe PageId
        } deriving (Show)

instance ToJSON PageListData where
    toJSON PageListData {..} =
        object
            [ "pages" .= pldPages
            , "homePageId" .= pldHomePageId
            ]

data ListPage =
    ListPage
        { lpId :: PageId
        , lpName :: T.Text
        , lpSlug :: T.Text
        } deriving (Show)

instance ToJSON ListPage where
    toJSON ListPage {..} =
        object
            [ "id" .= lpId
            , "name" .= lpName
            , "slug" .= lpSlug
            ]

pageListRoute :: WrappedAuthToken -> App (Cookied PageListData)
pageListRoute = flip withAdminCookie $ const $ runDB $ do
    pldPages <- map makeListPage <$> selectList [] [Asc PageName]
    pldHomePageId <- fmap entityKey <$> getBy (UniquePageSlug "home")
    return PageListData {..}
  where
    makeListPage :: Entity Page -> ListPage
    makeListPage (Entity pageId page) =
        ListPage
            { lpId = pageId
            , lpName = pageName page
            , lpSlug = pageSlug page
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
    time <- liftIO getCurrentTime
    let newPage =
            Page
                { pageName = nppTitle
                , pageSlug = slugify nppSlug
                , pageContent = sanitize nppContent
                , pageUpdatedAt = time
                }
    runDB $ insert newPage


-- EDIT


type EditPageDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" PageId
    :> Get '[JSON] (Cookied EditPageData)

data EditPageData =
    EditPageData
        { epdId :: PageId
        , epdTitle :: T.Text
        , epdSlug :: T.Text
        , epdContent :: T.Text
        } deriving (Show)

instance ToJSON EditPageData where
    toJSON EditPageData {..} =
        object
            [ "id" .= epdId
            , "title" .= epdTitle
            , "slug" .= epdSlug
            , "content" .= epdContent
            ]

editPageDataRoute :: WrappedAuthToken -> PageId -> App (Cookied EditPageData)
editPageDataRoute token pageId = withAdminCookie token $ \_ -> do
    mPage <- runDB $ get pageId
    case mPage of
        Nothing ->
            serverError err404
        Just Page {..} ->
            return EditPageData
                { epdId = pageId
                , epdTitle = pageName
                , epdSlug = pageSlug
                , epdContent = pageContent
                }


type EditPageRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] EditPageParameters
    :> Patch '[JSON] (Cookied ())

data EditPageParameters =
    EditPageParameters
        { eppId :: PageId
        , eppTitle :: Maybe T.Text
        , eppSlug :: Maybe T.Text
        , eppContent :: Maybe T.Text
        } deriving (Show)

instance FromJSON EditPageParameters where
    parseJSON = withObject "EditPageParameters" $ \v -> do
        eppId <- v .: "id"
        eppTitle <- v .: "title"
        eppSlug <- v .: "slug"
        eppContent <- v .: "content"
        return EditPageParameters {..}

instance Validation EditPageParameters where
    validators EditPageParameters {..} = do
        slugCheck <- case eppSlug of
            Nothing ->
                return []
            Just slug -> do
                doesntExist <- V.doesntExist $ UniquePageSlug slug
                return
                    [ ( "slug"
                      , [ V.required slug
                        , ( "A Page with this slug already exists."
                          , doesntExist
                          )
                        ]
                      )
                    ]
        return $ catMaybes
            [ V.mapCheck eppTitle $ \title ->
                ( "title", [ V.required title ] )
            , V.mapCheck eppContent $ \content ->
                ( "content", [ V.required content ] )
            ]
            ++ slugCheck

editPageRoute :: WrappedAuthToken -> EditPageParameters -> App (Cookied ())
editPageRoute = validateAdminAndParameters $ \_ parameters -> do
    time <- liftIO getCurrentTime
    let updates = makeUpdates parameters
    unless (null updates) $
        runDB $ update (eppId parameters) $ (PageUpdatedAt =. time) : updates
  where
    makeUpdates :: EditPageParameters -> [Update Page]
    makeUpdates EditPageParameters {..} =
        catMaybes
            [ mapUpdateWith PageName eppTitle sanitize
            , mapUpdateWith PageSlug eppSlug (slugify . sanitize)
            , mapUpdateWith PageContent eppContent sanitize
            ]
