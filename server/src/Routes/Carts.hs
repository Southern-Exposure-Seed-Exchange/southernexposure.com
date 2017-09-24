{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Routes.Carts
    ( CartAPI
    , cartRoutes
    ) where

import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.Persist ( (+=.), (=.), (==.), Entity(..), getBy, insert, insertEntity
                        , update, updateWhere, upsertBy, deleteWhere)
import Database.Persist.Sql (toSqlKey)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, JSON, PlainText, Get, Post, throwError, err404)

import Auth
import Models
import Server
import Validation (Validation(..))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Esqueleto as E
import qualified Validation as V


type CartAPI =
         "customer" :> CustomerCartAPI
    :<|> "anonymous" :> AnonymousCartAPI

type CustomerCartAPI =
         "add" :> CustomerAddRoute
    :<|> "details" :> CustomerDetailsRoute
    :<|> "update" :> CustomerUpdateRoute

type AnonymousCartAPI =
         "add" :> AnonymousAddRoute
    :<|> "details" :> AnonymousDetailsRoute
    :<|> "update" :> AnonymousUpdateRoute


type CartRoutes =
         CustomerCartRoutes
    :<|> AnonymousCartRoutes

cartRoutes :: CartRoutes
cartRoutes =
         customerRoutes
    :<|> anonymousRoutes


type CustomerCartRoutes =
         (AuthToken -> CustomerAddParameters -> App ())
    :<|> (AuthToken -> App CartDetailsData)
    :<|> (AuthToken -> CustomerUpdateParameters -> App CartDetailsData)

customerRoutes :: CustomerCartRoutes
customerRoutes =
         customerAddRoute
    :<|> customerDetailsRoute
    :<|> customerUpdateRoute


type AnonymousCartRoutes =
         (AnonymousAddParameters -> App T.Text)
    :<|> (AnonymousDetailsParameters -> App CartDetailsData)
    :<|> (AnonymousUpdateParameters -> App CartDetailsData)

anonymousRoutes :: AnonymousCartRoutes
anonymousRoutes =
         anonymousAddRoute
    :<|> anonymousDetailsRoute
    :<|> anonymousUpdateRoute


-- COMMON DATA


newtype CartDetailsData =
    CartDetailsData
        { cddItems :: [ ItemData ]
        }

instance ToJSON CartDetailsData where
    toJSON details =
        object [ "items" .= cddItems details
               ]

data ItemData =
    ItemData
        { idItemId :: CartItemId
        , idProduct :: Entity Product
        , idVariant :: Entity ProductVariant
        , idMaybeSeedAttribute :: Maybe (Entity SeedAttribute)
        , idQuantity :: Int64
        }

instance ToJSON ItemData where
    toJSON item =
        object [ "id" .= idItemId item
               , "product" .= idProduct item
               , "variant" .= idVariant item
               , "seedAttribute" .= idMaybeSeedAttribute item
               , "quantity" .= idQuantity item
               ]


-- ADD ITEM


data CustomerAddParameters =
    CustomerAddParameters
        { capVariantId :: ProductVariantId
        , capQuantity :: Int64
        }

instance FromJSON CustomerAddParameters where
    parseJSON = withObject "CustomerAddParameters" $ \v ->
        CustomerAddParameters
            <$> v .: "variant"
            <*> v .: "quantity"

instance Validation CustomerAddParameters where
    validators parameters = do
        variantExists <- V.exists $ capVariantId parameters
        return [ ( "variant"
                 , [ ( "This Product Variant Does Not Exist.", variantExists )
                   ]
                 )
               , ( "quantity"
                 , [ V.zeroOrPositive $ capQuantity parameters
                   ]
                 )
               ]

type CustomerAddRoute =
        AuthProtect "auth-token"
     :> ReqBody '[JSON] CustomerAddParameters
     :> Post '[JSON] ()

customerAddRoute :: AuthToken -> CustomerAddParameters -> App ()
customerAddRoute token = validate >=> \parameters ->
    let
        productVariant =
            capVariantId parameters
        quantity =
            capQuantity parameters
        item cartId =
            CartItem
                { cartItemCartId = cartId
                , cartItemProductVariantId = productVariant
                , cartItemQuantity = quantity
                }
    in do
        (Entity customerId _) <- validateToken token
        maybeCart <- runDB . getBy . UniqueCustomerCart $ Just customerId
        case maybeCart of
            Just (Entity cartId _) ->
                void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
                    (item cartId) [CartItemQuantity +=. quantity]
            Nothing ->
                let
                    cart =
                        Cart
                            { cartCustomerId = Just customerId
                            , cartSessionToken = Nothing
                            , cartExpirationTime = Nothing
                            }
                in
                    void . runDB $ insert cart >>= insert . item


data AnonymousAddParameters =
    AnonymousAddParameters
        { aapVariantId :: ProductVariantId
        , aapQuantity :: Int64
        , aapSessionToken :: Maybe T.Text
        }

instance FromJSON AnonymousAddParameters where
    parseJSON = withObject "AnonymousAddParameters" $ \v ->
        AnonymousAddParameters
            <$> v .: "variant"
            <*> v .: "quantity"
            <*> v .:? "sessionToken"

instance Validation AnonymousAddParameters where
    validators parameters = do
        variantExists <- V.exists $ aapVariantId parameters
        return [ ( "variant"
                 , [ ( "This Product Variant Does Not Exist.", variantExists )
                   ]
                 )
               , ( "quantity"
                 , [ V.zeroOrPositive $ aapQuantity parameters
                   ]
                 )
               ]

type AnonymousAddRoute =
       ReqBody '[JSON] AnonymousAddParameters
    :> Post '[PlainText] T.Text

anonymousAddRoute :: AnonymousAddParameters -> App T.Text
anonymousAddRoute = validate >=> \parameters ->
    let
        productVariant =
            aapVariantId parameters
        quantity =
            aapQuantity parameters
        maybeSessionToken =
            aapSessionToken parameters
        item cartId =
            CartItem
                { cartItemCartId = cartId
                , cartItemProductVariantId = productVariant
                , cartItemQuantity = quantity
                }
    in do
        (token, Entity cartId _) <- getOrCreateCart maybeSessionToken
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            (item cartId) [CartItemQuantity +=. quantity]
        return token
    where getOrCreateCart maybeToken =
            case maybeToken of
                Nothing ->
                    createAnonymousCart
                Just token -> do
                    maybeCart <- runDB . getBy $ UniqueAnonymousCart maybeToken
                    case maybeCart of
                        Nothing ->
                            createAnonymousCart
                        Just cart@(Entity cartId _) -> do
                            expirationTime <- getExpirationTime
                            runDB $ update cartId [CartExpirationTime =. Just expirationTime]
                            return (token, cart)
          createAnonymousCart = do
            token <- generateToken
            expiration <- getExpirationTime
            (,) token <$> runDB (insertEntity Cart
                { cartCustomerId = Nothing
                , cartSessionToken = Just token
                , cartExpirationTime = Just expiration
                })
          getExpirationTime =
            let
                sixteenWeeksInSeconds = 16 * 7 * 24 * 60 * 60
            in
                addUTCTime sixteenWeeksInSeconds <$> liftIO getCurrentTime
          generateToken = do
            token <- UUID.toText <$> liftIO UUID4.nextRandom
            maybeCart <- runDB . getBy . UniqueAnonymousCart $ Just token
            case maybeCart of
                Just _ ->
                    generateToken
                Nothing ->
                    return token


-- DETAILS


type CustomerDetailsRoute =
       AuthProtect "auth-token"
    :> Get '[JSON] CartDetailsData

customerDetailsRoute :: AuthToken -> App CartDetailsData
customerDetailsRoute = validateToken >=> \(Entity customerId _) ->
    getCartDetailsData $ \c ->
        c E.^. CartCustomerId E.==. E.just (E.val customerId)


newtype AnonymousDetailsParameters =
    AnonymousDetailsParameters
        { adpCartToken :: T.Text
        }

instance FromJSON AnonymousDetailsParameters where
    parseJSON = withObject "AnonymousDetailsParameters" $ \v ->
        AnonymousDetailsParameters
            <$> v .: "sessionToken"

type AnonymousDetailsRoute =
       ReqBody '[JSON] AnonymousDetailsParameters
    :> Post '[JSON] CartDetailsData

anonymousDetailsRoute :: AnonymousDetailsParameters -> App CartDetailsData
anonymousDetailsRoute parameters =
    getCartDetailsData $ \c ->
        c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)


getCartDetailsData :: (E.SqlExpr (Entity Cart) -> E.SqlExpr (E.Value Bool))
                   -> App CartDetailsData
getCartDetailsData whereQuery = do
    items <- runDB $ E.select $ E.from
        $ \(ci `E.InnerJoin` c `E.InnerJoin` v `E.InnerJoin` p `E.LeftOuterJoin` sa) -> do
            E.on (sa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId))
            E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
            E.on (v E.^. ProductVariantId E.==. ci E.^. CartItemProductVariantId)
            E.on (c E.^. CartId E.==. ci E.^. CartItemCartId)
            E.where_ $ whereQuery c
            E.orderBy [E.asc $ p E.^. ProductName]
            return (ci E.^. CartItemId, p, v, sa, ci E.^. CartItemQuantity)
    return $ CartDetailsData $ map toItemData items
    where toItemData (i, p, v, sa, q) =
            ItemData (E.unValue i) p v sa (E.unValue q)


-- UPDATE


newtype CustomerUpdateParameters =
    CustomerUpdateParameters
        { cupQuantities :: M.Map Int64 Int64
        }

instance FromJSON CustomerUpdateParameters where
    parseJSON = withObject "CustomerUpdateParameters" $ \v ->
        CustomerUpdateParameters
            <$> v .: "quantities"

instance Validation CustomerUpdateParameters where
    validators parameters = return $
        V.validateMap [V.zeroOrPositive] $ cupQuantities parameters

type CustomerUpdateRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerUpdateParameters
    :> Post '[JSON] CartDetailsData

customerUpdateRoute :: AuthToken -> CustomerUpdateParameters -> App CartDetailsData
customerUpdateRoute token = validate >=> \parameters -> do
    (Entity customerId _) <- validateToken token
    maybeCart <- runDB . getBy . UniqueCustomerCart $ Just customerId
    case maybeCart of
        Nothing ->
            lift . throwError $ err404
        Just (Entity cartId _) ->
            updateOrDeleteItems cartId $ cupQuantities parameters
    customerDetailsRoute token


data AnonymousUpdateParameters =
    AnonymousUpdateParameters
        { aupCartToken :: T.Text
        , aupQuantities :: M.Map Int64 Int64
        }

instance FromJSON AnonymousUpdateParameters where
    parseJSON = withObject "AnonymousUpdateParameters" $ \v ->
        AnonymousUpdateParameters
            <$> v .: "sessionToken"
            <*> v .: "quantities"

instance Validation AnonymousUpdateParameters where
    validators parameters = return $
        V.validateMap [ V.zeroOrPositive ] $ aupQuantities parameters

type AnonymousUpdateRoute =
       ReqBody '[JSON] AnonymousUpdateParameters
    :> Post '[JSON] CartDetailsData

anonymousUpdateRoute :: AnonymousUpdateParameters -> App CartDetailsData
anonymousUpdateRoute parameters = do
    let token = aupCartToken parameters
    maybeCart <- runDB . getBy . UniqueAnonymousCart $ Just token
    case maybeCart of
        Nothing ->
            lift . throwError $ err404
        Just (Entity cartId _) ->
            updateOrDeleteItems cartId $ aupQuantities parameters
    anonymousDetailsRoute $ AnonymousDetailsParameters token


updateOrDeleteItems :: CartId -> M.Map Int64 Int64 -> App ()
updateOrDeleteItems cartId =
    runDB . M.foldlWithKey
        (\acc key val -> acc >>
            if val == 0 then
                deleteWhere
                    [ CartItemId ==. toSqlKey key, CartItemCartId ==. cartId ]
            else
                updateWhere
                    [ CartItemId ==. toSqlKey key, CartItemCartId ==. cartId ]
                    [ CartItemQuantity =. val ]
        )
        (return ())
