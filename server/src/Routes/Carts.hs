{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Routes.Carts
    ( CartAPI
    , cartRoutes
    ) where

import Control.Monad ((>=>), void, join)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.:?), (.=), FromJSON(..), ToJSON(..), object, withObject)
import Data.Int (Int64)
import Data.List (intersect)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.Persist ( (+=.), (=.), (==.), Entity(..), SelectOpt(Asc), getBy
                        , insert, insertEntity, update, updateWhere, upsertBy
                        , deleteWhere, selectList )
import Database.Persist.Sql (toSqlKey)
import Numeric.Natural (Natural)
import Servant ((:>), (:<|>)(..), AuthProtect, ReqBody, JSON, PlainText, Get, Post, err404)

import Auth
import Models
import Models.Fields (Cents(..), Country, Region, ShippingRate(..))
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
         (AuthToken -> CustomerAddParameters -> App ())
    :<|> (AuthToken -> App CartDetailsData)
    :<|> (AuthToken -> CustomerUpdateParameters -> App CartDetailsData)
    :<|> (AuthToken -> App ItemCountData)
    :<|> (AuthToken -> CustomerQuickOrderParameters -> App ())

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
        { cddItems :: [ ItemData ]
        , cddCharges :: CartCharges
        }

instance ToJSON CartDetailsData where
    toJSON details =
        object [ "items" .= cddItems details
               , "charges" .= cddCharges details
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

data CartCharges =
    CartCharges
        { ccTax :: Maybe CartCharge
        , ccSurcharges :: [CartCharge]
        , ccShippingMethods :: [CartCharge]
        }

instance ToJSON CartCharges where
    toJSON charges =
        object [ "tax" .= ccTax charges
               , "surcharges" .= ccSurcharges charges
               , "shippingMethods" .= ccShippingMethods charges
               ]

data CartCharge =
    CartCharge
        { ccDescription :: T.Text
        , ccAmount :: Cents
        }

instance ToJSON CartCharge where
    toJSON charge =
        object [ "description" .= ccDescription charge
               , "amount" .= ccAmount charge
               ]


getCartItems :: (E.SqlExpr (Entity Cart) -> E.SqlExpr (E.Value Bool)) -> App [ItemData]
getCartItems whereQuery = do
    items <- runDB $ E.select $ E.from
        $ \(ci `E.InnerJoin` c `E.InnerJoin` v `E.InnerJoin` p `E.LeftOuterJoin` sa) -> do
            E.on (sa E.?. SeedAttributeProductId E.==. E.just (p E.^. ProductId))
            E.on (p E.^. ProductId E.==. v E.^. ProductVariantProductId)
            E.on (v E.^. ProductVariantId E.==. ci E.^. CartItemProductVariantId)
            E.on (c E.^. CartId E.==. ci E.^. CartItemCartId)
            E.where_ $ whereQuery c
            E.orderBy [E.asc $ p E.^. ProductName]
            return (ci E.^. CartItemId, p, v, sa, ci E.^. CartItemQuantity)
    return $ map toItemData items
    where toItemData (i, p, v, sa, q) =
            ItemData (E.unValue i) p v sa (E.unValue q)

getCharges :: Maybe Country -> Maybe Region -> [ItemData] -> App CartCharges
getCharges maybeCountry maybeRegion items =
    let
        variant item =
            (\(Entity _ v) -> v) $ idVariant item
        subTotal =
            foldl (\acc item -> acc + itemTotal item) 0 items
        itemTotal item =
            fromIntegral (idQuantity item) * fromCents (productVariantPrice $ variant item)
    in do
        surcharges <- getSurcharges items
        shippingMethods <- getShippingMethods maybeCountry items subTotal
        taxCharge <- getTaxCharge maybeCountry maybeRegion subTotal surcharges shippingMethods
        return $ CartCharges taxCharge surcharges shippingMethods


getSurcharges :: [ItemData] -> App [CartCharge]
getSurcharges items =
    mapMaybe (getSurcharge $ foldl buildQuantityMap M.empty items)
        <$> runDB (selectList [SurchargeIsActive ==. True] [])
    where buildQuantityMap initialMap item =
            foldl (\acc cId -> M.insertWith (+) cId (idQuantity item) acc)
                initialMap
                ((\(Entity _ p) -> productCategoryIds p) $ idProduct item)
          getSurcharge categories (Entity _ surcharge) =
            let
                quantity =
                    foldl (\acc catId -> maybe acc (+ acc) $ M.lookup catId categories)
                        0 (surchargeCategoryIds surcharge)
                amount =
                    if quantity == 1 then
                        surchargeSingleFee surcharge
                    else
                        surchargeMultipleFee surcharge
            in
                if quantity == 0 then
                    Nothing
                else
                    Just $ CartCharge (surchargeDescription surcharge) amount

getShippingMethods :: Maybe Country -> [ItemData] -> Natural -> App [CartCharge]
getShippingMethods maybeCountry items subTotal =
    case maybeCountry of
        Nothing ->
            return []
        Just country ->
            mapMaybe (getMethod country)
                <$> runDB (selectList [ShippingMethodIsActive ==. True] [Asc ShippingMethodPriority])
    where getMethod country (Entity _ method) =
            let
                validCountry =
                    country `elem` shippingMethodCountries method
                validProducts =
                    null (shippingMethodCategoryIds method)
                        || all isValidProduct (map idProduct items)
                isValidProduct (Entity _ prod) =
                     not . null $ productCategoryIds prod `intersect` shippingMethodCategoryIds method
                thresholdAmount rates =
                    case rates of
                        [] ->
                            Cents 0
                        [r] ->
                            applyRate r
                        r1 : r2 : rest ->
                            if getThreshold r1 <= Cents subTotal && getThreshold r2 > Cents subTotal then
                                applyRate r1
                            else
                                thresholdAmount $ r2 : rest
                getThreshold rate =
                    case rate of
                        Flat t _ ->
                            t
                        Percentage t _ ->
                            t
                applyRate rate =
                    case rate of
                        Flat _ amount ->
                            amount
                        Percentage _ percentage ->
                            Cents . round $ (toRational percentage / 100) * toRational subTotal
            in
                if validCountry && validProducts then
                    Just . CartCharge (shippingMethodDescription method)
                        . thresholdAmount $ shippingMethodRates method
                else
                    Nothing

-- TODO: Simplify the maybe logic here; Natural -> Cents
getTaxCharge :: Maybe Country -> Maybe Region -> Natural -> [CartCharge] -> [CartCharge] -> App (Maybe CartCharge)
getTaxCharge maybeCountry maybeRegion subTotal surcharges shippingMethods =
    case maybeCountry of
        Just country -> do
            maybeTaxRate <- runDB . getBy $ UniqueTaxRate country maybeRegion
            case maybeTaxRate of
                Nothing ->
                    return Nothing
                Just (Entity _ taxRate) ->
                    if taxRateIsActive taxRate then
                        return . Just $ CartCharge
                            (taxRateDescription taxRate)
                            (Cents . round $ taxAmount taxRate
                                $ subTotal + surchargeAmount + shippingAmount)
                    else
                        return Nothing
        Nothing ->
            return Nothing
    where taxAmount taxRate taxableTotal =
            (toRational . toInteger $ taxRateRate taxRate) / 1000 * toRational taxableTotal
          surchargeAmount =
            foldl (\a c -> a + fromCents (ccAmount c)) 0 surcharges
          shippingAmount =
              case shippingMethods of
                [] ->
                    0
                cc : _ ->
                    fromCents $ ccAmount cc

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
        (Entity cartId _) <- getOrCreateCustomerCart customerId
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            (item cartId) [CartItemQuantity +=. quantity]


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
        (token, Entity cartId _) <- getOrCreateAnonymousCart maybeSessionToken
        void . runDB $ upsertBy (UniqueCartItem cartId productVariant)
            (item cartId) [CartItemQuantity +=. quantity]
        return token


-- DETAILS


type CustomerDetailsRoute =
       AuthProtect "auth-token"
    :> Get '[JSON] CartDetailsData

customerDetailsRoute :: AuthToken -> App CartDetailsData
customerDetailsRoute = validateToken >=> \(Entity customerId customer) -> do
    cartItems <- getCartItems $ \c ->
        c E.^. CartCustomerId E.==. E.just (E.val customerId)
    charges <- getCharges (Just $ customerCountry customer) (Just $ customerState customer)
        cartItems
    return $ CartDetailsData cartItems charges


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
anonymousDetailsRoute parameters = do
    cartItems <- getCartItems $ \c ->
        c E.^. CartSessionToken E.==. E.just (E.val $ adpCartToken parameters)
    charges <- getCharges Nothing Nothing cartItems
    return $ CartDetailsData cartItems charges



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
            serverError err404
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
            serverError err404
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


-- COUNT


newtype ItemCountData =
    ItemCountData
        { icdItemCount :: Int64
        }

instance ToJSON ItemCountData where
    toJSON countData =
        object [ "itemCount" .= icdItemCount countData
               ]

type CustomerCountRoute =
       AuthProtect "auth-token"
    :> Get '[JSON] ItemCountData

customerCountRoute :: AuthToken -> App ItemCountData
customerCountRoute = validateToken >=> \(Entity customerId _) ->
    fmap (ItemCountData . round . fromMaybe (0 :: Rational) . join . fmap E.unValue . listToMaybe) . runDB $ do
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
        , qoiQuantity :: Int64
        , qoiIndex :: Int64
        }

instance FromJSON QuickOrderItem where
    parseJSON = withObject "QuickOrderItem" $ \v ->
        QuickOrderItem
            <$> v .: "sku"
            <*> v .: "quantity"
            <*> v .: "index"

type CustomerQuickOrderRoute =
       AuthProtect "auth-token"
    :> ReqBody '[JSON] CustomerQuickOrderParameters
    :> Post '[JSON] ()

customerQuickOrderRoute :: AuthToken -> CustomerQuickOrderParameters -> App ()
customerQuickOrderRoute token parameters = do
    (Entity customerId _) <- validateToken token
    _ <- validate parameters
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
