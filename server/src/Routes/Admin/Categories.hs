{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Categories
    ( CategoryAPI
    , categoryRoutes
    ) where

import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Database.Persist (Entity(..), SelectOpt(Asc), selectList, insert)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, Get, Post, JSON)
import System.FilePath ((</>), takeFileName)
import Text.HTML.SanitizeXSS (sanitize)

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Config (Config(getMediaDirectory))
import Images (makeImageConfig, scaleImage)
import Models (Category(..), CategoryId, EntityField(CategoryName), Unique(UniqueCategorySlug), slugify)
import Server (App, runDB)
import Validation (Validation(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Validation as V


type CategoryAPI =
         "list" :> CategoryListRoute
    :<|> "new" :> NewCategoryDataRoute
    :<|> "new" :> NewCategoryRoute

type CategoryRoutes =
         (WrappedAuthToken -> App (Cookied CategoryListData))
    :<|> (WrappedAuthToken -> App (Cookied [NewCategoryData]))
    :<|> (WrappedAuthToken -> NewCategoryParameters -> App (Cookied CategoryId))

categoryRoutes :: CategoryRoutes
categoryRoutes =
         categoryListRoute
    :<|> newCategoryDataRoute
    :<|> newCategoryRoute


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


-- NEW


type NewCategoryDataRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied [NewCategoryData])

data NewCategoryData =
    NewCategoryData
        { ncdId :: CategoryId
        , ncdName :: T.Text
        } deriving (Show)

instance ToJSON NewCategoryData where
    toJSON NewCategoryData {..} =
        object [ "id" .= ncdId, "name" .= ncdName ]

newCategoryDataRoute :: WrappedAuthToken -> App (Cookied [NewCategoryData])
newCategoryDataRoute token = withAdminCookie token $ \_ ->
    map (\(Entity cId c) -> NewCategoryData cId (categoryName c))
        <$> runDB (selectList [] [Asc CategoryName])


type NewCategoryRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] NewCategoryParameters
    :> Post '[JSON] (Cookied CategoryId)


data NewCategoryParameters =
    NewCategoryParameters
        { ncpName :: T.Text
        , ncpSlug :: T.Text
        , ncpParentId :: Maybe CategoryId
        , ncpDescription :: T.Text
        , ncpImageData :: BS.ByteString
        -- ^ Base64 Encoded
        , ncpImageName :: T.Text
        , ncpOrder :: Int
        } deriving (Show)

instance FromJSON NewCategoryParameters where
    parseJSON = withObject "NewCategoryParameters" $ \v -> do
        ncpName <- v .: "name"
        ncpSlug <- v .: "slug"
        ncpParentId <- v .: "parentId"
        ncpDescription <- v .: "description"
        ncpImageData <- encodeUtf8 <$> v .: "imageData"
        ncpImageName <- v .: "imageName"
        ncpOrder <- v .: "order"
        return NewCategoryParameters {..}

instance Validation NewCategoryParameters where
    validators NewCategoryParameters {..} = do
        slugDoesntExist <- V.doesntExist $ UniqueCategorySlug ncpSlug
        parentExists <- fmap (fromMaybe False) . sequence
            $ V.exists <$> ncpParentId
        return
            [ ( "name"
              , [ V.required ncpName ]
              )
            , ( "slug"
              , [ V.required ncpSlug
                , ( "A Category with this Slug already exists."
                  , slugDoesntExist
                  )
                ]
              )
            , ( "parentId"
              , [ ( "Could not find this Parent Category in the database."
                  , parentExists
                  )
                ]
              )
            ]

newCategoryRoute :: WrappedAuthToken -> NewCategoryParameters -> App (Cookied CategoryId)
newCategoryRoute = validateAdminAndParameters $ \_ NewCategoryParameters {..} -> do
    imageFileName <- makeImage ncpImageName ncpImageData
    let newCategory = Category
            { categoryName = sanitize ncpName
            , categorySlug = slugify $ sanitize ncpSlug
            , categoryParentId = ncpParentId
            , categoryDescription = sanitize ncpDescription
            , categoryImageUrl = imageFileName
            , categoryOrder = ncpOrder
            }
    runDB $ insert newCategory
  where
    makeImage :: T.Text -> BS.ByteString -> App T.Text
    makeImage fileName imageData =
        if BS.null imageData then
            return ""
        else case Base64.decode imageData of
            Left _ ->
                return ""
            Right rawImageData -> do
                imageConfig <- makeImageConfig
                mediaDirectory <- asks getMediaDirectory
                T.pack . takeFileName
                    <$> scaleImage imageConfig fileName (mediaDirectory </> "categories") rawImageData
