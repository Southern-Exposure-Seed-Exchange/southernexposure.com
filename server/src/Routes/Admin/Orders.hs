{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Routes.Admin.Orders
    ( OrderAPI
    , orderRoutes
    ) where

import Control.Exception.Safe (MonadThrow, Exception, try, throwM)
import Control.Monad ((>=>), forM, join, void)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Scientific (Scientific, fromRationalRepetend)
import Data.Time
    ( UTCTime, LocalTime, getCurrentTimeZone, getCurrentTime, formatTime
    , defaultTimeLocale, utcToLocalTime
    )
import Database.Persist
    ( (==.), (=.), Entity(..), Filter, SelectOpt(..), count, get, selectList
    , getEntity, getJustEntity, insert, update
    )
import Servant
    ( (:<|>)(..), (:>), AuthProtect, QueryParam, Capture, ReqBody, Get, Post
    , JSON, err404
    )
import Text.Read (readMaybe)
import Web.Stripe ((-&-), StripeError(..))
import Web.Stripe.Refund (Amount(..), createRefund)

import Auth (WrappedAuthToken, Cookied, withAdminCookie, validateAdminAndParameters)
import Avalara (RefundTransactionRequest(..))
import Models
    ( OrderId, Order(..), OrderProduct(..), OrderLineItem(..), Customer(customerEmail)
    , Address(..), EntityField(..), CustomerId
    )
import Models.Fields
    ( OrderStatus, Region, Cents(..), LineItemType(..), StripeChargeId(..)
    , AdminOrderComment(..), AvalaraTransactionCode(..), creditLineItemTypes
    , formatCents
    )
import Routes.AvalaraUtils (renderAvalaraError)
import Routes.CommonData
    ( OrderDetails(..), toCheckoutOrder, getCheckoutProducts, toAddressData
    )
import Routes.Utils (extractRowCount, buildWhereQuery)
import Server (App, AppSQL, runDB, serverError, stripeRequest, avalaraRequest)
import Validation (Validation(..))
import Workers (Task(Avalara), AvalaraTask(RefundTransaction), enqueueTask)

import qualified Avalara
import qualified Data.List as L
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Validation as V


type OrderAPI =
         "list" :> OrderListRoute
    :<|> "details" :> OrderDetailsRoute
    :<|> "comment" :> OrderCommentRoute
    :<|> "refund" :> OrderRefundRoute

type OrderRoutes =
         (WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied OrderListData))
    :<|> (WrappedAuthToken -> OrderId -> App (Cookied AdminOrderDetails))
    :<|> (WrappedAuthToken -> OrderCommentParameters -> App (Cookied OrderId))
    :<|> (WrappedAuthToken -> OrderRefundParameters -> App (Cookied OrderId))

orderRoutes :: OrderRoutes
orderRoutes =
         orderListRoute
    :<|> orderDetailsRoute
    :<|> orderCommentRoute
    :<|> orderRefundRoute


-- LIST


type OrderListRoute =
       AuthProtect "cookie-auth"
    :> QueryParam "page" Int
    :> QueryParam "perPage" Int
    :> QueryParam "query" T.Text
    :> Get '[JSON] (Cookied OrderListData)

data OrderListData =
    OrderListData
        { oldOrders :: [ListOrder]
        , oldTotalOrders :: Int
        } deriving (Show)

instance ToJSON OrderListData where
    toJSON OrderListData {..} =
        object
            [ "orders" .= oldOrders
            , "total" .= oldTotalOrders
            ]

data ListOrder =
    ListOrder
        { loId :: OrderId
        , loDate :: UTCTime
        , loCustomerName :: T.Text
        , loCustomerEmail :: T.Text
        , loShippingStreet :: T.Text
        , loShippingRegion :: Maybe Region
        , loOrderStatus :: OrderStatus
        , loOrderTotal :: Integer
        } deriving (Show)

instance ToJSON ListOrder where
    toJSON ListOrder {..} =
        object
            [ "id" .= loId
            , "date" .= loDate
            , "customerName" .= loCustomerName
            , "customerEmail" .= loCustomerEmail
            , "shippingStreet" .= loShippingStreet
            , "shippingRegion" .= loShippingRegion
            , "orderStatus" .= loOrderStatus
            , "orderTotal" .= loOrderTotal
            ]

orderListRoute :: WrappedAuthToken -> Maybe Int -> Maybe Int -> Maybe T.Text -> App (Cookied OrderListData)
orderListRoute token maybePage maybePerPage maybeQuery = withAdminCookie token $ \_ -> do
    let perPage = fromMaybe 50 maybePerPage
        page = fromMaybe 1 maybePage
        offset = perPage * (page - 1)
        query = fromMaybe "" maybeQuery
    (orders, orderCount) <- runDB $ do
        orderCount <-
            if T.null query then
                count ([] :: [Filter Order])
            else
                extractRowCount . E.select $ E.from $ \(o `E.LeftOuterJoin` c `E.LeftOuterJoin` sa) -> do
                    E.on $ E.just (o E.^. OrderShippingAddressId) E.==. sa E.?. AddressId
                    E.on $ E.just (o E.^. OrderCustomerId) E.==. c E.?. CustomerId
                    E.where_ $ makeQuery o c sa query
                    return E.countRows
        orders <- do
            orders <-
                if T.null query then
                    map (,Nothing, Nothing)
                        <$> selectList []
                            [ Desc OrderCreatedAt
                            , LimitTo perPage
                            , OffsetBy $ fromIntegral offset
                            ]
                else
                    E.select $ E.from $ \(o `E.LeftOuterJoin` c `E.LeftOuterJoin` sa) -> do
                        E.on $ E.just (o E.^. OrderShippingAddressId) E.==. sa E.?. AddressId
                        E.on $ E.just (o E.^. OrderCustomerId) E.==. c E.?. CustomerId
                        E.limit $ fromIntegral perPage
                        E.offset $ fromIntegral offset
                        E.orderBy [E.desc $ o E.^. OrderCreatedAt]
                        E.where_ $ makeQuery o c sa query
                        return (o, c, sa)
            forM orders $ \(o@(Entity orderId order), c, sa) -> do
                total <- getOrderTotal orderId
                customer <- getIfNothing (orderCustomerId order) c
                shipAddr <- getIfNothing (orderShippingAddressId order) sa
                return (o, customer, shipAddr, total)
        return (orders, orderCount)
    return OrderListData
        { oldOrders = map convertOrder orders
        , oldTotalOrders = orderCount
        }
  where
    -- | Search the Order ID, Customer Email, Name, Street Line 1,
    -- & ZipCode.
    makeQuery
        :: E.SqlExpr (Entity Order)
        -> E.SqlExpr (Maybe (Entity Customer))
        -> E.SqlExpr (Maybe (Entity Address))
        -> T.Text
        -> E.SqlExpr (E.Value Bool)
    makeQuery o c sa =
        buildWhereQuery $ \term ->
            let wildQuery = E.just $ E.concat_ [(E.%), E.val term, (E.%)]
                idQuery = case readMaybe (T.unpack term) of
                    Nothing -> E.val False
                    Just num -> o E.^. OrderId E.==. E.valkey num
            in  [ idQuery
                , c E.?. CustomerEmail `E.ilike` wildQuery
                , sa E.?. AddressFirstName `E.ilike` wildQuery
                , sa E.?. AddressLastName `E.ilike` wildQuery
                , sa E.?. AddressAddressOne `E.ilike` wildQuery
                , sa E.?. AddressZipCode `E.ilike` wildQuery
                ]
    -- | Try fetching a row if we haven't yet.
    getIfNothing :: (E.PersistEntityBackend a ~ E.SqlBackend, E.PersistEntity a)
        => E.Key a -> Maybe (Entity a) -> AppSQL (Maybe a)
    getIfNothing key =
        maybe (get key) (return . Just . entityVal)
    -- | Build the ListOrder from the queried Order data.
    convertOrder :: (Entity Order, Maybe Customer, Maybe Address, Integer) -> ListOrder
    convertOrder (Entity orderId order, mCustomer, mShipping, orderTotal) =
        let name =
                maybe "" (\shipping -> addressFirstName shipping <> " " <> addressLastName shipping)
                    mShipping
        in  ListOrder
            { loId = orderId
            , loDate = orderCreatedAt order
            , loCustomerName = name
            , loCustomerEmail = maybe "" customerEmail mCustomer
            , loShippingStreet = maybe "" addressAddressOne mShipping
            , loShippingRegion = addressState <$> mShipping
            , loOrderStatus = orderStatus order
            , loOrderTotal = orderTotal
            }


-- DETAILS


type OrderDetailsRoute =
       AuthProtect "cookie-auth"
    :> Capture "id" OrderId
    :> Get '[JSON] (Cookied AdminOrderDetails)

data AdminOrderDetails =
    AdminOrderDetails
        { aodDetails :: OrderDetails
        , aodAdminComments :: [AdminOrderComment]
        , aodStripeId :: Maybe StripeChargeId
        , aodCustomerId :: CustomerId
        } deriving (Show)

instance ToJSON AdminOrderDetails where
    toJSON AdminOrderDetails {..} =
        object
            [ "details" .= aodDetails
            , "adminComments" .= aodAdminComments
            , "stripeId" .= aodStripeId
            , "customerId" .= aodCustomerId
            ]

orderDetailsRoute :: WrappedAuthToken -> OrderId -> App (Cookied AdminOrderDetails)
orderDetailsRoute token orderId = withAdminCookie token $ \_ -> runDB $ do
    order <- get orderId >>= maybe (lift $ serverError err404) return
    lineItems <- selectList [OrderLineItemOrderId ==. orderId] []
    products <- getCheckoutProducts orderId
    shipping <- getJustEntity $ orderShippingAddressId order
    maybeBilling <- sequence $ getEntity <$> orderBillingAddressId order
    let details = OrderDetails
            { odOrder = toCheckoutOrder $ Entity orderId order
            , odLineItems = lineItems
            , odProducts = products
            , odShippingAddress = toAddressData shipping
            , odBillingAddress = toAddressData <$> join maybeBilling
            }
    return AdminOrderDetails
        { aodDetails = details
        , aodAdminComments = sortOn adminCommentTime $ orderAdminComments order
        , aodStripeId = orderStripeChargeId order
        , aodCustomerId = orderCustomerId order
        }


-- COMMENT


type OrderCommentRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] OrderCommentParameters
    :> Post '[JSON] (Cookied OrderId)

data OrderCommentParameters =
    OrderCommentParameters
        { ocpId :: OrderId
        , ocpComment :: T.Text
        } deriving (Show)

instance FromJSON OrderCommentParameters where
    parseJSON = withObject "OrderCommentParameters" $ \v -> do
        ocpId <- v .: "id"
        ocpComment <- v .: "comment"
        return OrderCommentParameters {..}

instance Validation OrderCommentParameters where
    validators OrderCommentParameters {..} = do
        orderExists <- V.exists ocpId
        return
            [ ( ""
              , [ ("Could not find this order in the database.", orderExists) ]
              )
            , ( "comment"
              , [ V.required ocpComment ]
              )
            ]

orderCommentRoute :: WrappedAuthToken -> OrderCommentParameters -> App (Cookied OrderId)
orderCommentRoute = validateAdminAndParameters $ \_ parameters -> do
    time <- liftIO getCurrentTime
    let orderId = ocpId parameters
        comment = AdminOrderComment
            { adminCommentContent = ocpComment parameters
            , adminCommentTime = time
            }
    runDB $ get orderId >>= \case
        Nothing ->
            lift $ serverError err404
        Just order ->
            update orderId
                [ OrderAdminComments =. comment : orderAdminComments order
                ]
                >> return orderId


-- REFUND


type OrderRefundRoute =
       AuthProtect "cookie-auth"
    :> ReqBody '[JSON] OrderRefundParameters
    :> Post '[JSON] (Cookied OrderId)

data OrderRefundParameters =
    OrderRefundParameters
        { orpId :: OrderId
        , orpAmount :: Cents
        } deriving (Show)

instance FromJSON OrderRefundParameters where
    parseJSON = withObject "OrderRefundParameters" $ \v -> do
        orpId <- v .: "id"
        orpAmount <- v .: "amount"
        return OrderRefundParameters {..}

instance Validation OrderRefundParameters where
    validators OrderRefundParameters {..} = do
        orderExists <- V.exists orpId
        amountUnderBalance <- do
            total <- runDB $ getOrderTotal orpId
            return
                [ ( "The amount must be less than the current order total."
                  , toInteger (fromCents orpAmount) > total
                  )
                ]
        return
            [ ( ""
              , [ ("Could not find this order in the database.", orderExists) ]
              )
            , ( "amount"
              ,  amountUnderBalance
              )
            ]

data OrderRefundError
    = OrderNotFound
    | NoStripeCharge
    | StripeRefundError StripeError
    | AvalaraError Avalara.ErrorInfo
    deriving (Show)

instance Exception OrderRefundError

handleOrderRefundErrors :: App a -> App a
handleOrderRefundErrors =
    try >=> either handler return
  where
    handler :: OrderRefundError -> App a
    handler = V.singleError . \case
        OrderNotFound ->
            "Could not find this order in the database."
        NoStripeCharge ->
            "There is no charge available to refund."
        StripeRefundError stripeError -> T.intercalate ""
            [ "We encountered an error while trying to process the refund: "
            , T.pack $ show (errorType stripeError)
            , ":"
            , errorMsg stripeError
            ]
        AvalaraError errInfo ->
            renderAvalaraError errInfo


orderRefundRoute :: WrappedAuthToken -> OrderRefundParameters -> App (Cookied OrderId)
orderRefundRoute = validateAdminAndParameters $ \_ parameters -> do
    let orderId = orpId parameters
        refundAmount = orpAmount parameters
    mOrder <- runDB $ get orderId
    handleOrderRefundErrors $ maybeM mOrder OrderNotFound $ \order ->
        maybeM (orderStripeChargeId order) NoStripeCharge $ \chargeId -> do
        refundResult <- stripeRequest $ createRefund (fromStripeChargeId chargeId)
            -&- Amount (fromIntegral $ fromCents refundAmount)
        case refundResult of
            Left stripeError ->
                throwM $ StripeRefundError stripeError
            Right _ -> runDB $ do
                addRefundLineItem orderId refundAmount
                makeAvalaraRefund (Entity orderId order) refundAmount
                return orderId
  where
    maybeM :: (MonadThrow m, Exception e) => Maybe a -> e -> (a -> m b) -> m b
    maybeM val exception justAction = maybe (throwM exception) justAction val
    -- | Add the refund OrderLineItem & add an admin comment to the Order.
    addRefundLineItem :: OrderId -> Cents -> AppSQL ()
    addRefundLineItem orderId refundAmount = do
        time <- liftIO getCurrentTime
        shortDate <- getRefundDateDescription
        let lineDescription = "Refund (" <> shortDate <> ")"
            comment = AdminOrderComment
                { adminCommentContent = "Refunded " <> formatCents refundAmount <> "."
                , adminCommentTime = time
                }
        void $ insert OrderLineItem
            { orderLineItemOrderId = orderId
            , orderLineItemType = RefundLine
            , orderLineItemDescription = lineDescription
            , orderLineItemAmount = refundAmount
            }
        get orderId >>= \case
            Nothing -> return ()
            Just order_ ->
                update orderId
                    [ OrderAdminComments =.
                        comment : orderAdminComments order_
                    ]
    -- | Get short & long representations of the current date.
    getRefundDateDescription :: MonadIO m => m T.Text
    getRefundDateDescription =
        T.pack . formatTime defaultTimeLocale "%m/%d/%y" <$> getCurrentLocalTime
    -- | Make an Avalara Refund for the Order.
    makeAvalaraRefund :: Entity Order -> Cents -> AppSQL ()
    makeAvalaraRefund (Entity orderId order) refundAmount =
        case orderAvalaraTransactionCode order of
            Nothing ->
                return ()
            Just trans@(AvalaraTransactionCode companyCode transactionCode) -> do
                orderTotal <- getOrderTotal orderId
                refunds <- sum . map (orderLineItemAmount . entityVal)
                    <$> selectList
                        [ OrderLineItemOrderId ==. orderId
                        , OrderLineItemType ==. RefundLine
                        ]
                        []
                date <- liftIO getCurrentTime
                let originalTotal =
                        orderTotal + fromIntegral (fromCents refunds)
                    (refundType, refundPercent) =
                        ( Avalara.RefundPercentage
                        , Just $ makeScientific $
                            toInteger (fromCents refundAmount) % originalTotal
                        )
                    request =
                        RefundTransactionRequest
                            { rtrTransctionCode = Nothing
                            , rtrDate = date
                            , rtrType = refundType
                            , rtrPercentage = refundPercent
                            , rtrLines = []
                            , rtrReferenceCode = Nothing
                            }
                lift (avalaraRequest $ Avalara.refundTransaction companyCode transactionCode request)
                    >>= \case
                        Avalara.SuccessfulResponse _ ->
                            return ()
                        _ ->
                            enqueueTask Nothing . Avalara
                                $ RefundTransaction trans refundType refundPercent
    -- | Turn a Rational Percentage into a Decimal, shifting it from
    -- between 0 and 1 to 0 and 100.
    makeScientific :: Rational -> Scientific
    makeScientific num =
        (* 100) $ either fst fst $ fromRationalRepetend (Just 4) num



-- UTILS


-- | Get the final total for an order.
getOrderTotal :: OrderId -> AppSQL Integer
getOrderTotal orderId = do
    products <- selectList [OrderProductOrderId ==. orderId] []
    lineItems <- selectList [OrderLineItemOrderId ==. orderId] []
    let (credits, debits) =
            L.partition isCreditLine lineItems
        productTotal =
            sum $ map finalProductPrice products
        creditTotal =
            sum $ map (orderLineItemAmount . entityVal) credits
        debitTotal =
            sum $ map (orderLineItemAmount . entityVal) debits

    return $ centsInt productTotal + centsInt debitTotal - centsInt creditTotal
  where
    -- | Is the line item a credit?
    isCreditLine :: Entity OrderLineItem -> Bool
    isCreditLine =
        (`elem` creditLineItemTypes) . orderLineItemType . entityVal
    -- | Caclulate the total price + tax for a Product.
    finalProductPrice :: Entity OrderProduct -> Cents
    finalProductPrice (Entity _ p) =
        fromIntegral (orderProductQuantity p) * orderProductPrice p
    -- | Convert Cents to an Integer so it can handle negative numbers.
    centsInt :: Cents -> Integer
    centsInt (Cents c) =
        toInteger c


-- | Get the current local time.
getCurrentLocalTime :: MonadIO m => m LocalTime
getCurrentLocalTime = liftIO $
    utcToLocalTime
        <$> getCurrentTimeZone
        <*> getCurrentTime
