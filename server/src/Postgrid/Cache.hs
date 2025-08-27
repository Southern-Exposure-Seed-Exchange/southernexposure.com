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
import Data.LruCache.IO (StripedLruHandle, newStripedLruHandle, stripedCached)

import Postgrid.API (PostgridError)
import Postgrid.API.Types

data CachedApiEndpoint req resp where
    CachedVerifyStructuredAddress :: CachedApiEndpoint VerifyStructuredAddressRequest VerifyStructuredAddressResponse

cacheKey :: CachedApiEndpoint req resp -> req -> ByteString
cacheKey CachedVerifyStructuredAddress = encode

data CacheEntry where
    CacheEntry :: CachedApiEndpoint req resp -> Either PostgridError resp -> CacheEntry

-- | In-memory cache for Postgrid API query results. It stores either a successful encoded response or an error.
-- Encoded response is stored for the sake of memory efficiency.
newtype QueryCache = QueryCache { unQueryCache :: StripedLruHandle ByteString CacheEntry }

newPostgridQueryCache :: Int -> Int -> IO QueryCache
newPostgridQueryCache numStripes size = QueryCache <$> newStripedLruHandle numStripes size

cached :: QueryCache -> req -> CachedApiEndpoint req resp -> IO (Either PostgridError resp) -> IO (Either PostgridError resp)
cached (QueryCache handle) req cachedApiEndpoint apiCall = do
    let key = cacheKey cachedApiEndpoint req
    result <- stripedCached handle key $ CacheEntry cachedApiEndpoint <$> apiCall
    case (cachedApiEndpoint, result) of
        (CachedVerifyStructuredAddress, CacheEntry CachedVerifyStructuredAddress res) ->
            return res
