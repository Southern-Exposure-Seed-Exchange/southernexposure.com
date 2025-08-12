{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Routes.Carts
    ( CartAPI
    , ValidateCartParameters(..)
    , cartRoutes
    ) where

import Control.Monad ((>=>), forM, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.Persist
    ( (+=.), (=.), (==.), Entity(..), get, getBy, insert, insertEntity, update
    , updateWhere, upsertBy, deleteWhere
    )
import Database.Persist.Sql (toSqlKey)
import Numeric.Natural (Natural)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, JSON, PlainText, Get, Post, err404)

import Auth
import Models
import Models.Fields (AddressType(..), AvalaraCustomerCode(..))
import Routes.CommonData (CartItemData(..), CartCharges(..), getCartItems, getCharges, toAddressData)
import Routes.Utils (generateUniqueToken, getDisabledCheckoutDetails)
import Server
import Validation (Validation(..))

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Esqueleto.Experimental as E
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
        , cddIsDisabled :: Bool
        , cddDisabledMessage :: T.Text
        }

instance ToJSON CartDetailsData where
    toJSON details =
        object [ "items" .= cddItems details
               , "charges" .= cddCharges details
               , "disabled" .= cddIsDisabled details
               , "disabledMessage" .= cddDisabledMessage details
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

data CustomerAddCartParameters = CustomerAddCartParameters
    { cacpVariantId :: ProductVariantId
    , cacpQuantity :: Natural
    , cacpCartId :: CartId
    }

instance Validation CustomerAddCartParameters where
    validators parameters = do
        variantErrors <- validateVariant (cacpVariantId parameters) (cacpQuantity parameters) (cacpCartId parameters)
        return [ ( "variant", variantErrors )
               , ( "quantity"
                 , [ V.zeroOrPositive $ cacpQuantity parameters
                   ]
                 )
               ]

type CustomerAddRoute =
        AuthProtect "cookie-auth"
     :> ReqBody '[JSON] CustomerAddParameters
     :> Post '[JSON] (Cookied ())

customerAddRoute :: WrappedAuthToken -> CustomerAddParameters -> App (Cookied ())
customerAddRoute token parameters = withCookie token $ \authToken -> do
    (Entity customerId _) <- validateToken authToken
    (Entity cartId _) <- getOrCreateCustomerCart customerId
    validate (CustomerAddCartParameters (capVariantId parameters) (capQuantity parameters) cartId) >> do
        let
            productVariant =
                capVariantId parameters
            quantity =
                capQuantity parameters
            item =
                CartItem
                    { cartItemCartId = cartId
                    , cartItemProductVariantId = productVariant
                    , cartItemQuantity = fromIntegral quantity
                    }
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            item [CartItemQuantity +=. fromIntegral quantity]

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

data AnonymousAddCartParameters = AnonymousAddCartParameters
    { aacpVariantId :: ProductVariantId
    , aacpQuantity :: Natural
    , aacpCartId :: CartId
    }

instance Validation AnonymousAddCartParameters where
    validators parameters = do
        variantErrors <- validateVariant (aacpVariantId parameters) (aacpQuantity parameters) (aacpCartId parameters)
        return [ ( "variant", variantErrors )
               , ( "quantity"
                 , [ V.zeroOrPositive $ aacpQuantity parameters
                   ]
                 )
               ]

type AnonymousAddRoute =
       ReqBody '[JSON] AnonymousAddParameters
    :> Post '[PlainText] T.Text

anonymousAddRoute :: AnonymousAddParameters -> App T.Text
anonymousAddRoute parameters = do
    (token, Entity cartId _) <- getOrCreateAnonymousCart (aapSessionToken parameters)
    validate (AnonymousAddCartParameters (aapVariantId parameters) (aapQuantity parameters) cartId) >> do
        let
            productVariant =
                aapVariantId parameters
            quantity =
                aapQuantity parameters
            item =
                CartItem
                    { cartItemCartId = cartId
                    , cartItemProductVariantId = productVariant
                    , cartItemQuantity = fromIntegral quantity
                    }
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            item [CartItemQuantity +=. fromIntegral quantity]
        return token


-- | Ensure a Variant exists, is active, and is not sold out.
validateVariant :: ProductVariantId -> Natural -> CartId -> App [(T.Text, Bool)]
validateVariant variantId purchaseAmount cartId =
    runDB $ get variantId >>= \case
        Nothing ->
            return
                [ ( "This Product Does Not Exist.", True ) ]
        Just variant -> do
            mbCartQuantity <- fmap (fmap E.unValue . listToMaybe) <$> E.select $ do
                (c E.:& ci) <- E.from $ E.table @Cart
                    `E.innerJoin` E.table @CartItem
                    `E.on` \(c E.:& ci) -> ci E.^. CartItemCartId E.==. c E.^. CartId
                E.where_ $ c E.^. CartId E.==. E.val cartId
                E.where_ $ ci E.^. CartItemProductVariantId E.==. E.val variantId
                return $ ci E.^. CartItemQuantity

            let inactiveError =
                    [ ( "This Product is Inactive."
                        , not (productVariantIsActive variant)
                        )
                    ]
                soldOutError =
                    [ ( "This Product has less stock than requested. " <> "Available: " <> T.pack (show $ productVariantQuantity variant) <>
                        ". Requested: " <> T.pack (show (purchaseAmount + maybe 0 fromIntegral mbCartQuantity)) <> "."
                        , productVariantQuantity variant < fromIntegral purchaseAmount + maybe 0 fromIntegral mbCartQuantity
                        )
                    ]
            return $ inactiveError <> soldOutError


-- DETAILS


type CustomerDetailsRoute =
       AuthProtect "cookie-auth"
    :> Get '[JSON] (Cookied CartDetailsData)

customerDetailsRoute :: WrappedAuthToken -> App (Cookied CartDetailsData)
customerDetailsRoute token = withValidatedCookie token getCustomerCartDetails

getCustomerCartDetails :: Entity Customer -> App CartDetailsData
getCustomerCartDetails (Entity customerId customer) =
    runDB $ do
        (checkoutDisabled, disabledMsg) <- lift getDisabledCheckoutDetails
        shippingAddress <- getShipping
        items <- getCartItems $ \c ->
            c E.^. CartCustomerId E.==. E.just (E.val customerId)
        let avalaraCode = fromAvalaraCustomerCode <$> customerAvalaraCode customer
        charges <- getCharges shippingAddress avalaraCode items Nothing False False
        return $ CartDetailsData items charges checkoutDisabled disabledMsg
    where getShipping =
            fmap toAddressData . listToMaybe
                    <$> P.selectList
                        [ AddressType ==. Shipping
                        , AddressIsDefault ==. True
                        , AddressCustomerId ==. customerId
                        ]
                        [ P.LimitTo 1
                        ]


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
        (checkoutDisabled, disabledMsg) <- lift getDisabledCheckoutDetails
        items <- getCartItems $ \c ->
            c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
        charges <- getCharges Nothing Nothing items Nothing False False
        return $ CartDetailsData items charges checkoutDisabled disabledMsg



-- UPDATE


newtype CustomerUpdateParameters =
    CustomerUpdateParameters
        { cupQuantities :: M.Map Int64 Natural
        }

instance FromJSON CustomerUpdateParameters where
    parseJSON = withObject "CustomerUpdateParameters" $ \v ->
        CustomerUpdateParameters
            <$> v .: "quantities"

type CustomerUpdateRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] CustomerUpdateParameters
    :> Post '[JSON] (Cookied CartDetailsData)

customerUpdateRoute :: WrappedAuthToken -> CustomerUpdateParameters -> App (Cookied CartDetailsData)
customerUpdateRoute token parameters = withCookie token $ \authToken -> do
    customer@(Entity customerId _) <- validateToken authToken
    maybeCart <- runDB . getBy . UniqueCustomerCart $ Just customerId
    case maybeCart of
        Nothing ->
            serverError err404
        Just (Entity cartId _) -> do
            validate (ValidateCartParameters cartId (cupQuantities parameters)) >> do
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
            validate (ValidateCartParameters cartId (aupQuantities parameters)) >> do
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
                    [ CartItemQuantity =. fromIntegral val ]
        )
        (return ())


data ValidateCartParameters = ValidateCartParameters CartId (M.Map Int64 Natural)

instance Validation ValidateCartParameters where
    validators (ValidateCartParameters cartId quantities) =
        runDB $ forM (M.toList quantities) $ \(itemId, quantity) -> (T.pack $ show itemId,) <$> do
            let nonNegativeQuantityValidation = V.zeroOrPositive quantity
            mbVariantQuantity <- fmap (fmap E.unValue . listToMaybe) <$> E.select $ do
                (c E.:& ci E.:& pv) <- E.from $ E.table @Cart
                    `E.innerJoin` E.table @CartItem
                        `E.on` (\(c E.:& ci) -> ci E.^. CartItemCartId E.==. c E.^. CartId)
                    `E.innerJoin` E.table @ProductVariant
                        `E.on` (\(_c E.:& ci E.:& pv) -> ci E.^. CartItemProductVariantId E.==. pv E.^. ProductVariantId)
                E.where_ $ c E.^. CartId E.==. E.val cartId
                E.where_ $ ci E.^. CartItemId E.==. E.val (toSqlKey itemId)
                return $ pv E.^. ProductVariantQuantity
            let variantStockValidation = case mbVariantQuantity of
                    Nothing ->
                        ("Product does not exist.", True)
                    Just variantQuantity ->
                        ( "This Product has less stock than requested. " <>
                          "Available: " <> T.pack (show variantQuantity) <>
                          ". Requested: " <> T.pack (show quantity) <> "."
                        , variantQuantity < fromIntegral quantity
                        )
            return [nonNegativeQuantityValidation, variantStockValidation]

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
                E.select $ E.from E.table >>= \ci -> do
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
                , [ ("Product is Invalid, Inactive, or Sold Out", null variants) ]
                )

upsertQuickOrderItem :: CartId -> QuickOrderItem -> App ()
upsertQuickOrderItem cartId item = do
    variants <- getVariantsByItem item
    case variants of
        [] ->
            return ()
        Entity variantId variant : _ ->
            let
                boundedQuantity =
                    fromIntegral $ max 0
                        $ min (productVariantQuantity variant)
                        $ fromIntegral $ qoiQuantity item
                cartItem =
                    CartItem
                        { cartItemCartId = cartId
                        , cartItemProductVariantId = variantId
                        , cartItemQuantity = boundedQuantity
                        }
            in
                when (boundedQuantity > 0) $
                    void . runDB $ upsertBy (UniqueCartItem cartId variantId)
                        cartItem [CartItemQuantity +=. boundedQuantity]

getVariantsByItem :: QuickOrderItem -> App [Entity ProductVariant]
getVariantsByItem item =
    runDB $ E.select $ do
        (v E.:& p) <- E.from $ E.table `E.innerJoin` E.table
            `E.on` \(v E.:& p) -> v E.^. ProductVariantProductId E.==. p E.^. ProductId
        let fullSku = E.lower_ $ p E.^. ProductBaseSku E.++. v E.^. ProductVariantSkuSuffix
        E.where_ $
            fullSku E.==. E.val (T.toLower $ qoiSku item) E.&&.
            v E.^. ProductVariantIsActive E.&&.
            v E.^. ProductVariantQuantity E.>. E.val 0
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
