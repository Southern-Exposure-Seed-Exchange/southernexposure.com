{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Categories
    ( CategoryAPI
    , categoryRoutes
    ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist (Entity(..), selectList)
import Servant ((:>), AuthProtect, Get, JSON)

import Auth (Cookied, WrappedAuthToken, withAdminCookie)
import Models (Category(..), CategoryId)
import Server (App, runDB)

import qualified Data.Text as T
import qualified Data.Map.Strict as M


type CategoryAPI =
        "list" :> CategoryListRoute

type CategoryRoutes =
        (WrappedAuthToken -> App (Cookied CategoryListData))

categoryRoutes :: CategoryRoutes
categoryRoutes =
        categoryListRoute


-- LIST


type CategoryListRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CategoryListData)

newtype CategoryListData =
    CategoryListData
        { cldRoots :: [ListCategory]
        } deriving (Show)

instance ToJSON CategoryListData where
    toJSON CategoryListData {..} =
        object
            [ "roots" .= cldRoots
            ]

data ListCategory =
    ListCategory
        { lcId :: CategoryId
        , lcName :: T.Text
        , lcChildren :: [ListCategory]
        } deriving (Show)

instance ToJSON ListCategory where
    toJSON ListCategory {..} =
        object
            [ "id" .= lcId
            , "name" .= lcName
            , "children" .= lcChildren
            ]

categoryListRoute :: WrappedAuthToken -> App (Cookied CategoryListData)
categoryListRoute token = withAdminCookie token $ const $
    CategoryListData . makeData . foldl makeRootsAndMap ([], M.empty)
        <$> runDB (selectList [] [])
  where
    makeRootsAndMap
        :: ([Entity Category], M.Map CategoryId [Entity Category])
        -> Entity Category
        -> ([Entity Category], M.Map CategoryId [Entity Category])
    makeRootsAndMap (roots, cMap) e@(Entity _ category) =
        case categoryParentId category of
            Nothing ->
                (e : roots, cMap)
            Just parentId ->
                (roots, M.insertWith (<>) parentId [e] cMap)
    makeData :: ([Entity Category], M.Map CategoryId [Entity Category]) -> [ListCategory]
    makeData (roots, cMap) =
        map (makeWithChildren cMap) $ sortByOrder roots
    makeWithChildren :: M.Map CategoryId [Entity Category] -> Entity Category -> ListCategory
    makeWithChildren cMap (Entity cId category) =
        let children = fromMaybe [] $ M.lookup cId cMap in
        ListCategory
            { lcId = cId
            , lcName = categoryName category
            , lcChildren = map (makeWithChildren cMap) $ sortByOrder children
            }
    sortByOrder :: [Entity Category] -> [Entity Category]
    sortByOrder = sortOn $ categoryOrder . entityVal
