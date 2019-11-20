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
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.Persist ( (+=.), (=.), (==.), Entity(..), getBy, insert
                        , insertEntity, update, updateWhere, upsertBy
                        , deleteWhere )
import Database.Persist.Sql (toSqlKey)
import Numeric.Natural (Natural)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, JSON, PlainText, Get, Post, err404)

import Auth
import Models
import Models.Fields (AddressType(..))
import Routes.CommonData (CartItemData(..), CartCharges(..), getCartItems, getCharges)
import Routes.Utils (generateUniqueToken)
import Server
import Validation (Validation(..))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Esqueleto as E
import qualified Validation as V


type CartAPI =
         "customer" :> CustomerCartAPI
    :<|> "anonymous" :> AnonymousCartAPI

type CustomerCartAPI =
         "add" :> CustomerAddRoute
    :<|> "details" :> CustomerDetailsRoute
    :<|> "update" :> CustomerUpdateRoute
    :<|> "count" :> CustomerCountRoute
    :<|> "quick-order" :> CustomerQuickOrderRoute

type AnonymousCartAPI =
         "add" :> AnonymousAddRoute
    :<|> "details" :> AnonymousDetailsRoute
    :<|> "update" :> AnonymousUpdateRoute
    :<|> "quick-order" :> AnonymousQuickOrderRoute


type CartRoutes =
         CustomerCartRoutes
    :<|> AnonymousCartRoutes

cartRoutes :: CartRoutes
cartRoutes =
         customerRoutes
    :<|> anonymousRoutes


type CustomerCartRoutes =
         (WrappedAuthToken -> CustomerAddParameters -> App (Cookied ()))
    :<|> (WrappedAuthToken -> App (Cookied CartDetailsData))
    :<|> (WrappedAuthToken -> CustomerUpdateParameters -> App (Cookied CartDetailsData))
    :<|> (WrappedAuthToken -> App (Cookied ItemCountData))
    :<|> (WrappedAuthToken -> CustomerQuickOrderParameters -> App (Cookied ()))

customerRoutes :: CustomerCartRoutes
customerRoutes =
         customerAddRoute
    :<|> customerDetailsRoute
    :<|> customerUpdateRoute
    :<|> customerCountRoute
    :<|> customerQuickOrderRoute


type AnonymousCartRoutes =
         (AnonymousAddParameters -> App T.Text)
    :<|> (AnonymousDetailsParameters -> App CartDetailsData)
    :<|> (AnonymousUpdateParameters -> App CartDetailsData)
    :<|> (AnonymousQuickOrderParameters -> App T.Text)

anonymousRoutes :: AnonymousCartRoutes
anonymousRoutes =
         anonymousAddRoute
    :<|> anonymousDetailsRoute
    :<|> anonymousUpdateRoute
    :<|> anonymousQuickOrderRoute


-- COMMON DATA


data CartDetailsData =
    CartDetailsData
        { cddItems :: [ CartItemData ]
        , cddCharges :: CartCharges
        }

instance ToJSON CartDetailsData where
    toJSON details =
        object [ "items" .= cddItems details
               , "charges" .= cddCharges details
               ]




-- ADD ITEM


data CustomerAddParameters =
    CustomerAddParameters
        { capVariantId :: ProductVariantId
        , capQuantity :: Natural
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
        AuthProtect "cookie-auth"
     :> ReqBody '[JSON] CustomerAddParameters
     :> Post '[JSON] (Cookied ())

customerAddRoute :: WrappedAuthToken -> CustomerAddParameters -> App (Cookied ())
customerAddRoute = validateCookieAndParameters $ \(Entity customerId _) parameters ->
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
        (Entity cartId _) <- getOrCreateCustomerCart customerId
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            (item cartId) [CartItemQuantity +=. quantity]


data AnonymousAddParameters =
    AnonymousAddParameters
        { aapVariantId :: ProductVariantId
        , aapQuantity :: Natural
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
        (token, Entity cartId _) <- getOrCreateAnonymousCart maybeSessionToken
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            (item cartId) [CartItemQuantity +=. quantity]
        return token


-- DETAILS


type CustomerDetailsRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CartDetailsData)

customerDetailsRoute :: WrappedAuthToken -> App (Cookied CartDetailsData)
customerDetailsRoute token = withValidatedCookie token getCustomerCartDetails

getCustomerCartDetails :: Entity Customer -> App CartDetailsData
getCustomerCartDetails (Entity customerId customer) =
    runDB $ do
        shippingCountry <- getShippingCountry
        items <- getCartItems $ \c ->
            c E.^. CartCustomerId E.==. E.just (E.val customerId)
        charges <- getCharges shippingCountry items
            (not . T.null $ customerMemberNumber customer)
            Nothing False
        return $ CartDetailsData items charges
    where getShippingCountry = do
            maybeShippingAddress <-
                listToMaybe . map P.entityVal
                    <$> P.selectList
                        [ AddressType ==. Shipping
                        , AddressIsDefault ==. True
                        , AddressCustomerId ==. customerId
                        ]
                        [ P.LimitTo 1
                        ]
            return $ addressCountry <$> maybeShippingAddress


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
    runDB $ do
        items <- getCartItems $ \c ->
            c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
        charges <- getCharges Nothing items False Nothing False
        return $ CartDetailsData items charges



-- UPDATE


newtype CustomerUpdateParameters =
    CustomerUpdateParameters
        { cupQuantities :: M.Map Int64 Natural
        }

instance FromJSON CustomerUpdateParameters where
    parseJSON = withObject "CustomerUpdateParameters" $ \v ->
        CustomerUpdateParameters
            <$> v .: "quantities"

instance Validation CustomerUpdateParameters where
    validators parameters = return $
        V.validateMap [V.zeroOrPositive] $ cupQuantities parameters

type CustomerUpdateRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerUpdateParameters
    :> Post '[JSON] (Cookied CartDetailsData)

customerUpdateRoute :: WrappedAuthToken -> CustomerUpdateParameters -> App (Cookied CartDetailsData)
customerUpdateRoute = validateCookieAndParameters $ \customer@(Entity customerId _) parameters -> do
    maybeCart <- runDB . getBy . UniqueCustomerCart $ Just customerId
    case maybeCart of
        Nothing ->
            serverError err404
        Just (Entity cartId _) ->
            updateOrDeleteItems cartId $ cupQuantities parameters
    getCustomerCartDetails customer


data AnonymousUpdateParameters =
    AnonymousUpdateParameters
        { aupCartToken :: T.Text
        , aupQuantities :: M.Map Int64 Natural
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
            serverError err404
        Just (Entity cartId _) ->
            updateOrDeleteItems cartId $ aupQuantities parameters
    anonymousDetailsRoute $ AnonymousDetailsParameters token


updateOrDeleteItems :: CartId -> M.Map Int64 Natural -> App ()
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


-- COUNT


newtype ItemCountData =
    ItemCountData
        { icdItemCount :: Natural
        }

instance ToJSON ItemCountData where
    toJSON countData =
        object [ "itemCount" .= icdItemCount countData
               ]

type CustomerCountRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied ItemCountData)

customerCountRoute :: WrappedAuthToken -> App (Cookied ItemCountData)
customerCountRoute token = withValidatedCookie token $ \(Entity customerId _) ->
    fmap (ItemCountData . round . fromMaybe (0 :: Rational) . (E.unValue =<<) . listToMaybe) . runDB $ do
        maybeCart <- getBy . UniqueCustomerCart $ Just customerId
        case maybeCart of
            Nothing ->
                return [E.Value Nothing]
            Just (Entity cartId _) ->
                E.select $ E.from $ \ci -> do
                    E.where_ $ ci E.^. CartItemCartId E.==. E.val cartId
                    return . E.sum_ $ ci E.^. CartItemQuantity


-- QUICK ORDER


newtype CustomerQuickOrderParameters =
    CustomerQuickOrderParameters
        { cqopItems :: [QuickOrderItem]
        }

instance FromJSON CustomerQuickOrderParameters where
    parseJSON = withObject "CustomerQuickOrderParameters" $ \v ->
        CustomerQuickOrderParameters <$> v .: "items"

instance Validation CustomerQuickOrderParameters where
    validators parameters =
        validateItems $ cqopItems parameters


data QuickOrderItem =
    QuickOrderItem
        { qoiSku :: T.Text
        , qoiQuantity :: Natural
        , qoiIndex :: Int64
        }

instance FromJSON QuickOrderItem where
    parseJSON = withObject "QuickOrderItem" $ \v ->
        QuickOrderItem
            <$> v .: "sku"
            <*> v .: "quantity"
            <*> v .: "index"

type CustomerQuickOrderRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerQuickOrderParameters
    :> Post '[JSON] (Cookied ())

customerQuickOrderRoute :: WrappedAuthToken -> CustomerQuickOrderParameters -> App (Cookied ())
customerQuickOrderRoute =
    validateCookieAndParameters $ \(Entity customerId _) parameters -> do
        (Entity cartId _) <- getOrCreateCustomerCart customerId
        mapM_ (upsertQuickOrderItem cartId) $ cqopItems parameters


data AnonymousQuickOrderParameters =
    AnonymousQuickOrderParameters
        { aqopItems :: [QuickOrderItem]
        , aqopSessionToken :: Maybe T.Text
        }

instance FromJSON AnonymousQuickOrderParameters where
    parseJSON = withObject "AnonymousQuickOrderParameters" $ \v ->
        AnonymousQuickOrderParameters
            <$> v .: "items"
            <*> v .:? "sessionToken"

instance Validation AnonymousQuickOrderParameters where
    validators parameters =
        validateItems $ aqopItems parameters

type AnonymousQuickOrderRoute =
       ReqBody '[JSON] AnonymousQuickOrderParameters
    :> Post '[PlainText] T.Text

anonymousQuickOrderRoute :: AnonymousQuickOrderParameters -> App T.Text
anonymousQuickOrderRoute = validate >=> \parameters -> do
    (token, Entity cartId _) <- getOrCreateAnonymousCart $ aqopSessionToken parameters
    mapM_ (upsertQuickOrderItem cartId) $ aqopItems parameters
    return token


validateItems :: [QuickOrderItem] -> App V.Validators
validateItems =
    mapM validateItem
    where validateItem item = do
            variants <- getVariantsByItem item
            return
                ( T.pack . show $ qoiIndex item
                , [ ("Not a Valid Item Number", null variants) ]
                )

upsertQuickOrderItem :: CartId -> QuickOrderItem -> App ()
upsertQuickOrderItem cartId item = do
    variants <- getVariantsByItem item
    case variants of
        [] ->
            return ()
        Entity variantId _ : _ ->
            let
                quantity =
                    qoiQuantity item
                cartItem =
                    CartItem
                        { cartItemCartId = cartId
                        , cartItemProductVariantId = variantId
                        , cartItemQuantity = quantity
                        }
            in
                void . runDB $ upsertBy (UniqueCartItem cartId variantId)
                    cartItem [CartItemQuantity +=. quantity]

getVariantsByItem :: QuickOrderItem -> App [Entity ProductVariant]
getVariantsByItem item =
    runDB $ E.select $ E.from $ \(v `E.InnerJoin` p) -> do
        E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
        let fullSku = E.lower_ $ p E.^. ProductBaseSku E.++. v E.^. ProductVariantSkuSuffix
        E.where_ $ fullSku E.==. E.val (T.toLower $ qoiSku item)
        return v


-- UTILS


getOrCreateCustomerCart :: CustomerId -> App (Entity Cart)
getOrCreateCustomerCart customerId = runDB $ do
    maybeCart <- getBy . UniqueCustomerCart $ Just customerId
    case maybeCart of
        Just cart ->
            return cart
        Nothing ->
            let
                cart =
                    Cart
                        { cartCustomerId = Just customerId
                        , cartSessionToken = Nothing
                        , cartExpirationTime = Nothing
                        }
            in
            insert cart >>= \cartId -> return (Entity cartId cart)


getOrCreateAnonymousCart :: Maybe T.Text -> App (T.Text, Entity Cart)
getOrCreateAnonymousCart maybeToken =
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
    where createAnonymousCart = do
            expiration <- getExpirationTime
            runDB $ do
                token <- generateUniqueToken (UniqueAnonymousCart . Just)
                cart <- insertEntity Cart
                    { cartCustomerId = Nothing
                    , cartSessionToken = Just token
                    , cartExpirationTime = Just expiration
                    }
                return (token, cart)
          getExpirationTime =
            let
                sixteenWeeksInSeconds = 16 * 7 * 24 * 60 * 60
            in
                addUTCTime sixteenWeeksInSeconds <$> liftIO getCurrentTime
