{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Emails.OrderStatusUpdated (Parameters(..), fetchData, get) where

import Control.Monad.Cont (MonadIO)
import Data.Bool (bool)
import qualified Database.Esqueleto.Experimental as E
import qualified Data.Text.Lazy as L

import Models.DB
    (Customer(..), Key(..), EntityField (OrderDeliveryId), Order(..), OrderDelivery(..), OrderDeliveryId, OrderId)
import Database.Persist (selectList, (<-.))
import Routes.CommonData (toOrderStatus)

data Parameters = Parameters
    { order :: E.Entity Order
    , customer :: Customer
    , trackData :: [OrderDelivery]
    }


showOrderId :: E.Entity Order -> String
showOrderId (E.Entity orderId _) = show $ E.unSqlBackendKey $ unOrderKey orderId

get :: L.Text -> Parameters -> (String, L.Text)
get domainName parameters =
    ( "Southern Exposure Seed Exchange - Order #" <> showOrderId (order parameters) <> " Status Update"
    , render domainName parameters
    )

renderOrderDelivery :: OrderDelivery -> L.Text
renderOrderDelivery (OrderDelivery _ trackNum carrier pickupDate) =
    "* Tracking number: " <> L.fromStrict trackNum <> " | Carrier: " <> L.fromStrict carrier <> " | Pickup date: " <> L.fromStrict pickupDate

render :: L.Text -> Parameters -> L.Text
render domainName Parameters {..} =
    "Hello,\n\n" <>
    "Order #" <> L.pack (showOrderId order) <> " has been updated.\n\n" <>
    "Order status is: " <> L.fromStrict (toOrderStatus (E.entityVal order)) <> ".\n\n" <>
    bool ("New delivery tracking data:\n\n" <> L.unlines (map renderOrderDelivery trackData) <> "\n\n") "" (null trackData) <>
    "[View order status](" <> orderLink <> ")\n\n" <>

    "Best regards,\n" <>
    "Southern Exposure Seed Exchange"
    where
        orderLink =
            domainName <>
            "/account/order/" <> L.pack (showOrderId order) <>
            maybe "" (\token -> L.fromStrict "?token=" <> L.fromStrict token) (orderGuestToken $ E.entityVal order)

fetchData :: (Monad m, MonadIO m) => OrderId -> [OrderDeliveryId] -> E.SqlPersistT m (Maybe Parameters)
fetchData orderId orderDeliveryIds = do
    orderEntity <- E.get orderId
    case orderEntity of
        Nothing -> return Nothing
        Just order -> do
            trackData <- map E.entityVal <$> selectList [ OrderDeliveryId <-. orderDeliveryIds ] []
            customerEntity <- E.get (orderCustomerId order)
            case customerEntity of
                Nothing -> return Nothing
                Just customer -> return $ Just Parameters { order = E.Entity orderId order, customer = customer, trackData = trackData }
