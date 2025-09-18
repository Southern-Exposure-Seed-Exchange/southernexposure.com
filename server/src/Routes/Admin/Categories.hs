{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Categories
    ( CategoryAPI
    , categoryRoutes
    ) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad.Reader (asks, liftIO, unless)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), Value(Null), Object, object, withObject)
import Data.Aeson.Types (Parser)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time (getCurrentTime)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist
    ( (=.), Entity(..), SelectOpt(Asc), Update, selectList, insert, get, update
    )
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, Capture, Get, Post, Patch, JSON, err404)

import Auth (Cookied, WrappedAuthToken, withAdminCookie, validateAdminAndParameters)
import Cache (Caches(..), syncCategoryPredecessorCache, queryCategoryPredecessorCache)
import Config (Config(getCaches))
import Images (ImageSourceSet, makeSourceSet)
import Models (Category(..), CategoryId, EntityField(..), Unique(UniqueCategorySlug), slugify)
import Routes.Utils (mapUpdate, mapUpdateWith, makeImageFromBase64, sanitize)
import Server (App, runDB, serverError)
import Validation (Validation(..))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Validation as V


type CategoryAPI =
         "list" :> CategoryListRoute
    :<|> "new" :> NewCategoryDataRoute
    :<|> "new" :> NewCategoryRoute
    :<|> "edit" :> EditCategoryDataRoute
    :<|> "edit" :> EditCategoryRoute

type CategoryRoutes =
         (WrappedAuthToken -> App (Cookied CategoryListData))
    :<|> (WrappedAuthToken -> App (Cookied [NewCategoryData]))
    :<|> (WrappedAuthToken -> NewCategoryParameters -> App (Cookied CategoryId))
    :<|> (WrappedAuthToken -> CategoryId -> App (Cookied EditCategoryData))
    :<|> (WrappedAuthToken -> EditCategoryParameters -> App (Cookied ()))

categoryRoutes :: CategoryRoutes
categoryRoutes =
         categoryListRoute
    :<|> newCategoryDataRoute
    :<|> newCategoryRoute
    :<|> editCategoryDataRoute
    :<|> editCategoryRoute


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
            , ( "order"
              , [ V.zeroOrPositive ncpOrder ]
              )
            ]

newCategoryRoute :: WrappedAuthToken -> NewCategoryParameters -> App (Cookied CategoryId)
newCategoryRoute = validateAdminAndParameters $ \_ NewCategoryParameters {..} -> do
    time <- liftIO getCurrentTime
    imageFileName <- makeImageFromBase64 "categories" ncpImageName ncpImageData
    let newCategory = Category
            { categoryName = ncpName
            , categorySlug = slugify $ sanitize ncpSlug
            , categoryParentId = ncpParentId
            , categoryDescription = sanitize ncpDescription
            , categoryImageUrl = imageFileName
            , categoryOrder = ncpOrder
            , categoryUpdatedAt = time
            }
    newCategoryId <- runDB $ insert newCategory
    updateCategoryCaches
    return newCategoryId


-- EDIT


type EditCategoryDataRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" CategoryId
    :> Get '[JSON] (Cookied EditCategoryData)

data EditCategoryData =
    EditCategoryData
        { ecdId :: CategoryId
        , ecdName :: T.Text
        , ecdSlug :: T.Text
        , ecdParentId :: Maybe CategoryId
        , ecdDescription :: T.Text
        , ecdImage :: ImageSourceSet
        , ecdOrder :: Int
        } deriving (Show)

instance ToJSON EditCategoryData where
    toJSON EditCategoryData {..} =
        object
            [ "id" .= ecdId
            , "name" .= ecdName
            , "slug" .= ecdSlug
            , "parentId" .= ecdParentId
            , "description" .= ecdDescription
            , "image" .= ecdImage
            , "order" .= ecdOrder
            ]

editCategoryDataRoute :: WrappedAuthToken -> CategoryId -> App (Cookied EditCategoryData)
editCategoryDataRoute token categoryId = withAdminCookie token $ \_ -> do
    mCategory <- runDB $ get categoryId
    case mCategory  of
        Nothing ->
            serverError err404
        Just Category {..} -> do
            image <- makeSourceSet "categories" $ T.unpack categoryImageUrl
            return EditCategoryData
                { ecdId = categoryId
                , ecdName = categoryName
                , ecdSlug = categorySlug
                , ecdParentId = categoryParentId
                , ecdDescription = categoryDescription
                , ecdImage = image
                , ecdOrder = categoryOrder
                }


type EditCategoryRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] EditCategoryParameters
    :> Patch '[JSON] (Cookied ())

data EditCategoryParameters =
    EditCategoryParameters
        { ecpId :: CategoryId
        , ecpName :: Maybe T.Text
        , ecpSlug :: Maybe T.Text
        , ecpParentId :: Either () (Maybe CategoryId)
        -- ^ Left == no change, Right == update
        , ecpDescription :: Maybe T.Text
        , ecpImageName :: Maybe T.Text
        , ecpImageData :: Maybe BS.ByteString
        -- ^ Base64 Encoded
        , ecpOrder :: Maybe Int
        } deriving (Show)

instance FromJSON EditCategoryParameters where
    -- | A missing @parentId@ key does no update for the CategoryParentId,
    -- while a @null@ value deletes it.
    parseJSON = withObject "EditCategoryParameters" $ \v -> do
        ecpId <- v .: "id"
        ecpName <- v .: "name"
        ecpSlug <- v .: "slug"
        ecpParentId <- parseParentId v
        ecpDescription <- v .: "description"
        ecpImageName <- v .: "imageName"
        ecpImageData <- fmap encodeUtf8 <$> v .: "imageData"
        ecpOrder <- v .: "order"
        return EditCategoryParameters {..}
      where
        parseParentId :: Object -> Parser (Either () (Maybe CategoryId))
        parseParentId v =
            case HM.lookup "parentId" v of
                Nothing ->
                    return $ Left ()
                Just Null ->
                    return $ Right Nothing
                Just val ->
                    Right . Just <$> parseJSON val

instance Validation EditCategoryParameters where
    validators EditCategoryParameters {..} = do
        slugCheck <- case ecpSlug of
            Nothing -> return []
            Just slug -> do
                doesntExist <- V.doesntExist $ UniqueCategorySlug slug
                return
                    [ ( "slug"
                      , [ V.required slug
                        , ( "A Category with this Slug already exists."
                          , doesntExist
                          )
                        ]
                      )
                    ]
        parentCheck <- case ecpParentId of
            Left _ -> return []
            Right Nothing -> return []
            Right (Just parentId) -> do
                exists <- V.exists parentId
                noCycle <- checkCycle ecpId parentId
                return
                    [ ( "parentId"
                      , [ ( "Could not find this Parent Category in the database."
                          , exists
                          )
                        , ( "Can not set Parent Category to a Child of the Category."
                          , noCycle
                          )
                        ]
                      )
                    ]
        return $ catMaybes
            [ V.mapCheck ecpName $ \name ->
                ( "name"
                , [ V.required name ]
                )
            , V.mapCheck ecpOrder $ \order ->
                ( "order"
                , [ V.zeroOrPositive order ]
                )
            ]
            ++ slugCheck
            ++ parentCheck
      where
        -- Ensure that the new parent Category is not an eventual descendant of
        -- the Category.
        checkCycle :: CategoryId -> CategoryId -> App Bool
        checkCycle targetId parentId = do
            caches <- asks getCaches
            liftIO . atomically $ do
                parentPredecessors <- queryCategoryPredecessorCache parentId
                    . getCategoryPredecessorCache
                    <$> readTVar caches
                return $ targetId `elem` map entityKey parentPredecessors

editCategoryRoute :: WrappedAuthToken -> EditCategoryParameters -> App (Cookied ())
editCategoryRoute = validateAdminAndParameters $ \_ parameters -> do
    imageUpdate <-
        case (,) <$> ecpImageName parameters <*> ecpImageData parameters of
            Nothing ->
                return []
            Just (imageName, imageData) -> do
                newImageName <- makeImageFromBase64 "categories" imageName imageData
                return [CategoryImageUrl =. newImageName]
    time <- liftIO getCurrentTime
    let updates = makeUpdates parameters ++ imageUpdate ++ [CategoryUpdatedAt =. time]
    unless (null updates) $ do
        runDB $ update (ecpId parameters) updates
        updateCategoryCaches
  where
    makeUpdates :: EditCategoryParameters -> [Update Category]
    makeUpdates EditCategoryParameters {..} =
        catMaybes
            [ mapUpdate CategoryName ecpName
            , mapUpdateWith CategorySlug ecpSlug (slugify . sanitize)
            , mapUpdate CategoryParentId $ either (const Nothing) Just ecpParentId
            , mapUpdateWith CategoryDescription ecpDescription sanitize
            , mapUpdate CategoryOrder ecpOrder
            ]



-- UTILS


-- | Rebuild the CategoryPredecessorCache.
updateCategoryCaches :: App ()
updateCategoryCaches = do
    newCPCache <- runDB syncCategoryPredecessorCache
    caches <- asks getCaches
    liftIO . atomically . modifyTVar' caches $ \c ->
        c { getCategoryPredecessorCache = newCPCache }
