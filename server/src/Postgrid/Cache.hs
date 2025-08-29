{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Postgrid.Cache
    ( CachedApiEndpoint(..)
    , QueryCache(..)
    , cached
    , newPostgridQueryCache
    ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.IORef (atomicModifyIORef')
import Data.Hashable (hash)
import qualified Data.LruCache as Lru (lookup, insert)
import Data.LruCache.IO (LruHandle(..), StripedLruHandle(..), newStripedLruHandle)
import qualified Data.Vector as Vector

import Postgrid.API (PostgridError)
import Postgrid.API.Types

data CachedApiEndpoint req resp where
    CachedVerifyStructuredAddress :: CachedApiEndpoint VerifyStructuredAddressRequest VerifyStructuredAddressResponse

cacheKey :: CachedApiEndpoint req resp -> req -> ByteString
cacheKey CachedVerifyStructuredAddress = encode

data CacheEntry where
    CacheEntry :: CachedApiEndpoint req resp -> resp -> CacheEntry

-- | In-memory cache for Postgrid API query results. It stores either a successful encoded response or an error.
-- Encoded response is stored for the sake of memory efficiency.
newtype QueryCache = QueryCache { unQueryCache :: StripedLruHandle ByteString CacheEntry }

newPostgridQueryCache :: Int -> Int -> IO QueryCache
newPostgridQueryCache numStripes size = QueryCache <$> newStripedLruHandle numStripes size

-- | A helper for 'QueryCache' that checks the cache for a given request
-- and either returns the cached response or runs the provided action to fetch it.
-- Errors are not cached.
cached :: QueryCache -> req -> CachedApiEndpoint req resp -> IO (Either PostgridError resp) -> IO (Either PostgridError resp)
cached (QueryCache (StripedLruHandle handle)) req cachedApiEndpoint apiAction = do
    let key = cacheKey cachedApiEndpoint req
        idx = hash key `mod` Vector.length handle
    cachedInternal (handle Vector.! idx) req cachedApiEndpoint apiAction

-- | A custom version of Data.LruCache.IO.cache that doesn't cache errors.
cachedInternal :: LruHandle ByteString CacheEntry -> req -> CachedApiEndpoint req resp -> IO (Either PostgridError resp) -> IO (Either PostgridError resp)
cachedInternal (LruHandle ref) req cachedApiEndpoint apiAction = do
    let key = cacheKey cachedApiEndpoint req
    lookupRes <- atomicModifyIORef' ref $ \c ->
        case Lru.lookup key c of
            Nothing      -> (c,  Nothing)
            Just (v, c') -> (c', Just v)
    case (cachedApiEndpoint, lookupRes) of
        (CachedVerifyStructuredAddress, Just (CacheEntry CachedVerifyStructuredAddress v)) -> return $ Right v
        (_, Nothing) -> do
            v <- apiAction
            case v of
                Left _ -> return v
                Right resp -> do
                    atomicModifyIORef' ref $ \c ->
                        (Lru.insert key (CacheEntry cachedApiEndpoint resp) c, ())
                    return v
