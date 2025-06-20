{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Routes.AvalaraUtils
    ( -- * Avalara
      getOrCreateAvalaraCustomer
    , createAvalaraCustomer
    , createAvalaraTransaction
    , renderAvalaraError
    ) where

import Control.Monad.Reader (MonadReader, MonadIO, asks, liftIO, lift)
import Data.Maybe (listToMaybe)
import Database.Persist
    ( (==.), (!=.), (=.), Entity(..), PersistEntityBackend, PersistEntity
    , selectList, getBy, update, OverflowNatural(..)
    )
import Database.Persist.Sql (SqlBackend)

import Avalara (CreateTransactionRequest(..), LineItem(..))
import Config (Config(..))
import Models
import Models.Fields
    ( LineItemType(..), AvalaraCustomerCode(..), creditLineItemTypes
    , renderLotSize, toDollars, Country(..)
    )
import Routes.CommonData (AddressData(..), addressToAvalara, toAddressData)
import Server
import UnliftIO (MonadUnliftIO)

import qualified Avalara
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Esqueleto as E


-- | Render a prettified version of an 'Avalara.ErrorInfo'.
renderAvalaraError :: Avalara.ErrorInfo -> T.Text
renderAvalaraError errorInfo =
    let detailsMessage detail =
            Avalara.edMessage detail <> "(" <> Avalara.edDescription detail <> ")"
    in
    "Error #" <> Avalara.eiCode errorInfo <> " - " <> Avalara.eiMessage errorInfo
        <> ": "
        <> T.intercalate ";\n\t " (map detailsMessage $ Avalara.eiDetails errorInfo)
        <> "."


-- | If the customer already has an 'AvalaraCustomerCode', return it.
-- Otherwise send a 'createCustomer' request to Avalara and update the
-- Customer.
--
-- Throws a 'PlaceOrderError' on failure.
getOrCreateAvalaraCustomer
    :: (MonadReader Config m, MonadUnliftIO m)
    => Entity Customer -> Maybe (Entity Address) -> Entity Address
    -> E.SqlPersistT m (Maybe AvalaraCustomerCode)
getOrCreateAvalaraCustomer (Entity customerId customer) maybeBilling shipping =
    case customerAvalaraCode customer of
        Just code ->
            return $ Just code
        Nothing -> do
            let addressData = maybe (toAddressData shipping) toAddressData maybeBilling
            createAvalaraCustomer (customerEmail customer) addressData >>= \case
                Just customerCode -> do
                    update customerId [CustomerAvalaraCode =. Just customerCode]
                    return $ Just customerCode
                _ ->
                    return Nothing


-- | Attempt to create an Avalara Customer for the Customer.
--
-- Returns 'Nothing' if the request returns an error or no Customers.
createAvalaraCustomer
    :: (MonadUnliftIO m, MonadReader Config m)
    => T.Text -> AddressData -> E.SqlPersistT m (Maybe AvalaraCustomerCode)
createAvalaraCustomer email address = do
    companyId <- lift $ asks getAvalaraCompanyId
    customerCode <- fmap Avalara.CustomerCode . generateUniqueToken
        $ UniqueAvalaraCustomer . Just . AvalaraCustomerCode . Avalara.CustomerCode
    let newCustomer =
            Avalara.Customer
                { Avalara.cId = Nothing
                , Avalara.cCompanyId = companyId
                , Avalara.cCustomerCode = customerCode
                , Avalara.cAlternateId = Nothing
                , Avalara.cName =
                    adFirstName address <> " " <> adLastName address
                , Avalara.cLineOne = adAddressOne address
                , Avalara.cLineTwo = Just $ adAddressTwo address
                , Avalara.cCity = adCity address
                , Avalara.cPostalCode = adZipCode address
                , Avalara.cRegion = avalaraRegion $ adState address
                , Avalara.cCountry = T.pack . show . fromCountry $ adCountry address
                , Avalara.cPhoneNumber = Nothing
                , Avalara.cEmailAddress = Just email
                }
        request =
            Avalara.CreateCustomersRequest [ newCustomer ]
    lift (avalaraRequest (Avalara.createCustomers companyId request)) >>= \case
        Avalara.SuccessfulResponse customers ->
            case listToMaybe customers of
                Nothing ->
                    return Nothing
                Just c ->
                    return $ Just $ AvalaraCustomerCode $ Avalara.cCustomerCode c
        _ ->
            return Nothing
  where
    generateUniqueToken
        :: (PersistEntityBackend r ~ SqlBackend, PersistEntity r, MonadIO m)
        => (T.Text -> Unique r) -> E.SqlPersistT m T.Text
    generateUniqueToken uniqueConstraint = do
        token <- UUID.toText <$> liftIO UUID4.nextRandom
        maybeCustomer <- getBy $ uniqueConstraint token
        case maybeCustomer of
            Just _ ->
                generateUniqueToken uniqueConstraint
            Nothing ->
                return token


createAvalaraTransaction
    :: (MonadReader Config m, MonadUnliftIO m)
    => Entity Order
    -- ^ The Order for the Transaction
    -> Entity Address
    -- ^ The Shipping Address
    -> Maybe (Entity Address)
    -- ^ The Billing Address
    -> Entity Customer
    -- ^ The Order's Customer
    -> Bool
    -- ^ Have we alrady charged the Customer for the Order?
    --
    -- If so, mark line items as having tax included and commit the
    -- transaction. If not, tax will be calculated and the transactin will
    -- be uncommitted.
    -> E.SqlPersistT m (Maybe Avalara.Transaction)
createAvalaraTransaction (Entity orderId order) shippingAddress billingAddress customer alreadyCharged = do
    items <- selectList [OrderLineItemOrderId ==. orderId, OrderLineItemType !=. TaxLine] []
    products <- E.select $ E.from $ \(op `E.InnerJoin` v `E.InnerJoin` p) -> do
        E.on $ v E.^. ProductVariantProductId E.==. p E.^. ProductId
        E.on $ op E.^. OrderProductProductVariantId E.==. v E.^. ProductVariantId
        E.where_ $ op E.^. OrderProductOrderId E.==. E.val orderId
        return (op, p, v)
    sourceAddress <- fmap Avalara.addressFromLocation . lift
        $ asks getAvalaraSourceLocationCode
    mCustomerCode <- getOrCreateAvalaraCustomer customer billingAddress shippingAddress
    let (credits, debits) =
            L.partition
                (\i -> orderLineItemType (entityVal i) `elem` creditLineItemTypes)
                items
        discount =
            toDollars $ sum
                $ map orderLineItemAmount
                $ filter ((/= StoreCreditLine) . orderLineItemType)
                $ map entityVal credits
        debitLines =
            map makeDebitLine debits
        productLines =
            map makeProductLine products
        address =
            Avalara.Address
                { Avalara.addrSingleLocation = Nothing
                , Avalara.addrShipFrom = Just sourceAddress
                , Avalara.addrShipTo = Just $ addressToAvalara $ toAddressData shippingAddress
                , Avalara.addrPointOfOrderOrigin = Nothing
                , Avalara.addrPointOfOrderAcceptance = Nothing
                }
    case mCustomerCode of
        Nothing ->
            return Nothing
        Just customerCode ->
            let request =
                    CreateTransactionRequest
                        { ctrCode = Nothing
                        , ctrLines = productLines <> debitLines
                        , ctrType = Just Avalara.SalesInvoice
                        , ctrCompanyCode = Nothing
                        , ctrDate = orderCreatedAt order
                        , ctrCustomerCode = fromAvalaraCustomerCode customerCode
                        , ctrDiscount =
                            if discount /= 0 then
                                Just discount
                            else
                                Nothing
                        , ctrAddresses = Just address
                        , ctrCommit = Just alreadyCharged
                        }
            in
            lift (avalaraRequest $ Avalara.createTransaction request) >>= \case
                Avalara.SuccessfulResponse transaction ->
                    return $ Just transaction
                _ ->
                    return Nothing
  where
    -- | Turn a debit OrderLineItem into a LineItem for Avalara.
    makeDebitLine :: Entity OrderLineItem -> Avalara.LineItem
    makeDebitLine (Entity _ lineItem) =
        Avalara.LineItem
            { liNumber = Nothing
            , liQuantity = Just 1
            , liTotalAmount = Just $ toDollars $ orderLineItemAmount lineItem
            , liAddresses = Nothing
            , liTaxCode = lineItemToTaxCode $ orderLineItemType lineItem
            , liItemCode = Nothing
            , liDiscounted = Just True
            , liTaxIncluded = Just alreadyCharged
            , liDescription = Just $ orderLineItemDescription lineItem
            }
    -- | Turn an OrderProduct into a LineItem for Avalara.
    makeProductLine :: (Entity OrderProduct, Entity Product, Entity ProductVariant) -> Avalara.LineItem
    makeProductLine (Entity _ orderProd, Entity _ prod, Entity _ variant) =
        let quantity =
                fromIntegral $ unOverflowNatural $ orderProductQuantity orderProd
            singlePrice =
                toDollars $ orderProductPrice orderProd
            fullSku =
                productBaseSku prod <> productVariantSkuSuffix variant
            lotSize =
                maybe "" (\ls -> ", " <> renderLotSize ls) $ productVariantLotSize variant
        in
        Avalara.LineItem
            { liNumber = Nothing
            , liQuantity = Just quantity
            , liTotalAmount = Just $ quantity * singlePrice
            , liAddresses = Nothing
            , liTaxCode = Nothing
            , liItemCode = Just fullSku
            , liDiscounted = Just True
            , liTaxIncluded = Just alreadyCharged
            , liDescription = Just $ productName prod <> lotSize
            }
