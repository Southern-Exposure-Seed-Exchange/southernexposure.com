{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Carts
    ( CartAPI
    , cartRoutes
    ) where

import Control.Monad ((>=>), void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.:?), FromJSON(..), withObject)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.Persist ((+=.), (=.), Entity(..), getBy, insert, insertEntity, update, upsertBy)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, JSON, Post)

import Auth
import Models
import Server
import Validation (Validation(..))

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Validation as V


type CartAPI =
         "customer-add" :> CustomerAddRoute
    :<|> "anonymous-add" :> AnonymousAddRoute

type CartRoutes =
         (AuthToken -> CustomerAddParameters -> App ())
    :<|> (AnonymousAddParameters -> App T.Text)

cartRoutes :: CartRoutes
cartRoutes =
         customerAddRoute
    :<|> anonymousAddRoute


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
    :> Post '[JSON] T.Text

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
