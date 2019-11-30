{-| This module is responsible for designating the various caches we use to
improve server performance, as well as functions for initializing
& querying them.
-}
module Cache
    ( Caches(..)
    , emptyCache
    , initializeCaches
    , CategoryPredecessorCache
    , syncCategoryPredecessorCache
    , queryCategoryPredecessorCache
    )
    where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe, maybeToList)
import Database.Persist (Entity(..), selectList)
import Database.Persist.Sql (SqlPersistT)

import Models.DB (Category(..), CategoryId)

import qualified Data.Map.Strict as M

-- | Each cache is available as a field of the Caches type.
newtype Caches
    = Caches
        { getCategoryPredecessorCache :: CategoryPredecessorCache
        }

-- | An empty set of caches.
emptyCache :: Caches
emptyCache =
    Caches (CategoryPredecessorCache M.empty)

-- | Build all the caches.
initializeCaches :: MonadIO m => SqlPersistT m Caches
initializeCaches =
    Caches <$> syncCategoryPredecessorCache



-- | A map from 'CategoryId' to a list of all predecessor 'Category' entities.
-- This prevents the excessive/recursive SQL queries required for fetching
-- the parent tree for a Category.
newtype CategoryPredecessorCache
    = CategoryPredecessorCache (M.Map CategoryId [Entity Category])

-- | Build the cache from a single SQL query.
syncCategoryPredecessorCache :: MonadIO m => SqlPersistT m CategoryPredecessorCache
syncCategoryPredecessorCache = do
    categories <- selectList [] []
    let categoryMap = foldl (\m e@(Entity key _) -> M.insert key e m) M.empty categories
    return . CategoryPredecessorCache $ makeCacheMap categoryMap categories
    where
        makeCacheMap
            :: M.Map CategoryId (Entity Category)
            -> [Entity Category]
            -> M.Map CategoryId [Entity Category]
        makeCacheMap categoryMap =
            foldl
                (\m (Entity categoryId category) ->
                    M.insert categoryId
                        (getParents categoryMap $ maybeToList $ categoryParentId category)
                        m
                ) M.empty
        getParents :: M.Map CategoryId (Entity Category) -> [CategoryId] -> [Entity Category]
        getParents categoryMap parents = case parents of
            [] ->
                []
            nextParentId : rest ->
                case M.lookup nextParentId categoryMap of
                    Nothing ->
                        getParents categoryMap rest
                    Just e@(Entity _ parent) ->
                        e : getParents categoryMap (rest ++ maybeToList (categoryParentId parent))

-- | Query the cache for all predecessors(parents, grandparents, etc.) of
-- a 'Category'.
--
-- Note: The returned list will be ordered by increasing depth. I.e.,
-- @[parent, grandparent, great-grandparent, etc.]@
queryCategoryPredecessorCache :: CategoryId -> CategoryPredecessorCache -> [Entity Category]
queryCategoryPredecessorCache categoryId (CategoryPredecessorCache cache) =
    fromMaybe [] $ M.lookup categoryId cache
