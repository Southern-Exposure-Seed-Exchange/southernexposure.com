{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{- | This module is responsible for defining routes that redirect ID-based
routes to slug-based client routes.

These ID-based routes are used by the webserver for redirecting URLs from
our previous website, which had ID numbers in the slugs.

-}
module Routes.Redirects
    ( RedirectAPI
    , redirectRoutes
    ) where

import Database.Persist.Sql (PersistEntity, PersistEntityBackend, SqlBackend, get)
import GHC.TypeLits (Nat)
import Servant
    ( (:<|>)(..), (:>), Verb, StdMethod(GET), JSON, PlainText, Headers, Header
    , Capture, NoContent(..), addHeader, err404
    )

import Models
import Server (App, runDB, serverError)

import qualified Data.Text as T


type RedirectAPI =
         "category" :> CategoryRoute
    :<|> "product" :> ProductRoute
    :<|> "page" :> PageRoute

type RedirectRoutes =
         (CategoryId -> App RedirectResponse)
    :<|> (ProductId -> App RedirectResponse)
    :<|> (PageId -> App RedirectResponse)

redirectRoutes :: RedirectRoutes
redirectRoutes =
         categoryRedirectRoute
    :<|> productRedirectRoute
    :<|> pageRedirectRoute


-- CATEGORY

type CategoryRoute =
       Capture "id" CategoryId
    :> Redirect 301

categoryRedirectRoute :: CategoryId -> App RedirectResponse
categoryRedirectRoute =
    getOr404 $ \category ->
        redirect $ "/categories/" <> categorySlug category <> "/"


-- PRODUCT

type ProductRoute =
       Capture "id" ProductId
    :> Redirect 301

productRedirectRoute :: ProductId -> App RedirectResponse
productRedirectRoute =
    getOr404 $ \prod ->
        redirect $ "/products/" <> productSlug prod <> "/"


-- PAGE

type PageRoute =
       Capture "id" PageId
    :> Redirect 301

pageRedirectRoute :: PageId -> App RedirectResponse
pageRedirectRoute =
    getOr404 $ \page ->
        redirect $ "/" <> pageSlug page <> "/"


-- UTILS

-- | Helper to fetch an Entity by it's ID or throw a 404 error.
getOr404 :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => (e -> App a) -> Key e -> App a
getOr404 handler entityId =
    runDB (get entityId) >>= \case
        Nothing ->
            serverError err404
        Just entity ->
            handler entity

-- | Response type of the redirect routes.
type RedirectResponse
    = Headers '[Header "Location" T.Text] NoContent

-- Custom Verb For Redirecting GET Requests
type Redirect (code :: Nat)
    = Verb 'GET code '[JSON, PlainText] RedirectResponse

-- Return a Redirect to the given URL.
redirect :: T.Text -> App (Headers '[Header "Location" T.Text] NoContent)
redirect location =
    return $ addHeader location NoContent
