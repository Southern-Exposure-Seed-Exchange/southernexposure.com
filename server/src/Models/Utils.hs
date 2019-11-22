{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Utils
    ( -- * Products
      slugify
    , truncateDescription
    , truncateHtml
    , getChildCategoryIds
    , getParentCategories
    , categorySalePrice
      -- * Carts
    , mergeAnonymousCart
    , mergeCarts
      -- * Customers
    , getCustomerByEmail
      -- * Addresses
    , insertOrActivateAddress
      -- * Orders
    , applyVATax
    , getOrderTax
    , getLineItemTotal
    , getOrderTotal
      -- * Avalara
    , lineItemToTaxCode
    , avalaraRegion
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Database.Persist
    ( (==.), (=.), (+=.), Entity(..), Key(..), getBy, update, upsert
    , delete, deleteWhere, selectList, selectKeysList, insertEntity
    )
import Database.Persist.Sql (SqlPersistT)
import Text.HTML.TagSoup (Tag(..), parseTags, innerText, renderTags)

import Cache
import Models.DB
import Models.Fields
import Server

import qualified Avalara
import qualified Data.Text as T
import qualified Database.Esqueleto as E


-- | Turn a name into a URL-safe string.
slugify :: T.Text -> T.Text
slugify =
    T.intercalate "-" . T.words . T.toLower . replaceBy isValidChar
    where replaceBy predicate text =
            flip T.map text $ \c -> if predicate c then c else ' '
          isValidChar =
            isAny [ isAlphaNum, (==) '-', (==) '_' ]
          isAny preds value =
              case preds of
                predicate:ps ->
                    predicate value || isAny ps value
                [] ->
                    False


-- | Trim a Product's Description.
truncateDescription :: Entity Product -> Entity Product
truncateDescription (Entity pId p) =
    let
        (description, wasTruncated) =
            truncateHtml 40 $ productLongDescription p
        newDescription =
            if wasTruncated then
                description <> "..."
            else
                description
    in
        Entity pId $ p { productLongDescription = newDescription }

-- | Trim text containing HTML to the specified number of words, preserving
-- any tags. Tags that are unclosed when the word count is reached will be
-- closed in order.
--
-- The resulting HTML is returned, along with whether or not the HTML
-- required trimming.
truncateHtml :: Int -> T.Text -> (T.Text, Bool)
truncateHtml targetWordCount =
    run [] 0 [] . parseTags
  where
    run processed counted tagStack remaining =
        if counted == targetWordCount then
            case tagStack of
                [] ->
                    (renderTags $ reverse processed, innerText remaining /= "")
                closeTag : newStack ->
                    run (closeTag : processed) counted newStack remaining
        else
            case (remaining, tagStack) of
                ([], []) ->
                    (renderTags $ reverse processed, False)
                ([], closeTag : newStack) ->
                    run (closeTag : processed) counted newStack remaining
                (t@(TagOpen node _) : rest, _) ->
                    run (t : processed) counted (TagClose node : tagStack) rest
                (t@(TagClose _) : rest, []) ->
                    run (t : processed) counted tagStack rest
                (t@(TagClose _) : rest, topOfStack : restOfStack) ->
                    if t == topOfStack then
                        run (t : processed) counted restOfStack rest
                    else
                        run (t : processed) counted tagStack rest
                (TagText content : rest, _) ->
                    let wordsInContent = T.split (== ' ') content
                        wordCount = length $ filter (not . T.null) wordsInContent
                        wordsToTake = min wordCount $ targetWordCount - counted
                        newWords = takeWordsIgnoringSpacing wordsToTake wordsInContent
                        newNode = TagText $ T.unwords newWords
                    in
                        if wordCount > wordsToTake then
                            run (newNode : processed) (counted + wordsToTake) tagStack
                                $ TagText (T.unwords $ drop (length newWords) wordsInContent)
                                    : rest
                        else
                            run (newNode : processed) (counted + wordsToTake) tagStack rest
                (TagComment _ : rest, _) ->
                    run processed counted tagStack rest
                (TagWarning _ : rest, _) ->
                    run processed counted tagStack rest
                (TagPosition _ _ : rest, _) ->
                    run processed counted tagStack rest
    takeWordsIgnoringSpacing count text =
        if count <= 0 then case text of
            "":_ ->
                [""]
            _ ->
                []
        else case text of
            [] ->
                []
            "" : rest ->
                "" : takeWordsIgnoringSpacing count rest
            w : rest ->
                w : takeWordsIgnoringSpacing (count - 1) rest




-- | Return the ID's of a Category's Child Categories, including the passed
-- CategoryId.
getChildCategoryIds  :: Key Category -> AppSQL [Key Category]
getChildCategoryIds categoryId = do
    childrenKeys <- selectKeysList [CategoryParentId ==. Just categoryId] []
    subKeys <- concat <$> mapM getChildCategoryIds childrenKeys
    return $ categoryId : subKeys

getParentCategories :: Key Category -> App [Entity Category]
getParentCategories categoryId =
    queryCategoryPredecessorCache categoryId
        <$> readCache getCategoryPredecessorCache

-- | Apply a CategorySale to the Variant's original price.
categorySalePrice :: Cents -> CategorySale -> Cents
categorySalePrice price categorySale =
    case categorySaleType categorySale of
        FlatSale amount ->
            Cents $ fromInteger $ max 0
                $ asInteger price - asInteger amount
        PercentSale percent ->
            Cents . max 0 . round $
                toRational (fromCents price)
                * (1 - min 1 (fromIntegral percent % 100))
  where
    asInteger :: Cents -> Integer
    asInteger =
        fromIntegral . fromCents


-- | Merge an Anonymous Cart into a Customer's Cart, removing the Anonymous
-- Cart.
mergeAnonymousCart :: T.Text -> CustomerId -> AppSQL ()
mergeAnonymousCart cartToken customerId = do
    maybeCustomerCart <- getBy . UniqueCustomerCart $ Just customerId
    maybeAnonymousCart <- getBy . UniqueAnonymousCart $ Just cartToken
    case (maybeCustomerCart, maybeAnonymousCart) of
        (Just (Entity customerCartId _), Just (Entity anonCartId _)) -> do
            anonymousCartItems <- selectList [CartItemCartId ==. anonCartId] []
            mapM_ (upsertCartItem customerCartId) anonymousCartItems
            deleteWhere [CartItemCartId ==. anonCartId] >> delete anonCartId
        (Nothing, Just (Entity cartId _)) ->
            update cartId
                [ CartSessionToken =. Nothing
                , CartExpirationTime =. Nothing
                , CartCustomerId =. Just customerId
                ]
        _ ->
            return ()
    where upsertCartItem :: CartId -> Entity CartItem -> AppSQL (Entity CartItem)
          upsertCartItem customerCartId (Entity _ anonymousCartItem) =
            upsert
                CartItem
                    { cartItemCartId = customerCartId
                    , cartItemProductVariantId = cartItemProductVariantId anonymousCartItem
                    , cartItemQuantity = cartItemQuantity anonymousCartItem
                    }
                [ CartItemQuantity +=. cartItemQuantity anonymousCartItem ]

-- | Merge the second Cart into the first, then delete it.
mergeCarts :: MonadIO m => Entity Cart -> Entity Cart -> SqlPersistT m ()
mergeCarts (Entity keepId _) (Entity mergeId _) = do
    mergeItems <- selectList [CartItemCartId ==. mergeId] []
    mapM_ upsertCartItem mergeItems
    deleteWhere [CartItemCartId ==. mergeId] >> delete mergeId
  where
    upsertCartItem :: MonadIO m => Entity CartItem -> SqlPersistT m (Entity CartItem)
    upsertCartItem (Entity _ mergeItem) =
        upsert
            CartItem
                { cartItemCartId = keepId
                , cartItemProductVariantId = cartItemProductVariantId mergeItem
                , cartItemQuantity = cartItemQuantity mergeItem
                }
            [ CartItemQuantity +=. cartItemQuantity mergeItem ]


-- | Get the first matching customer with the given email, searching by
-- ignoring the casing.
getCustomerByEmail :: T.Text -> AppSQL (Maybe (Entity Customer))
getCustomerByEmail email = fmap listToMaybe . E.select $ E.from $ \c -> do
    E.where_ $ E.lower_ (c E.^. CustomerEmail) E.==. E.val (T.toLower email)
    return c


-- | Insert an Address for a Customer, or activate an existing Address if
-- one with the same details exists.
insertOrActivateAddress :: Address -> AppSQL (Entity Address)
insertOrActivateAddress newAddress = do
    maybeAddress <- findAddress newAddress
    case maybeAddress of
        Nothing ->
            insertEntity newAddress
        Just existingAddress ->
            update (entityKey existingAddress)
                [ AddressIsActive =. True
                , AddressIsDefault =. addressIsDefault newAddress
                ]
                >> return existingAddress
    where findAddress :: Address -> AppSQL (Maybe (Entity Address))
          findAddress address =
            listToMaybe <$>
                selectList
                    [ AddressFirstName ==. addressFirstName address
                    , AddressLastName ==. addressLastName address
                    , AddressCompanyName ==. addressCompanyName address
                    , AddressAddressOne ==. addressAddressOne address
                    , AddressAddressTwo ==. addressAddressTwo address
                    , AddressCity ==. addressCity address
                    , AddressState ==. addressState address
                    , AddressZipCode ==. addressZipCode address
                    , AddressCountry ==. addressCountry address
                    , AddressType ==. addressType address
                    , AddressCustomerId ==. addressCustomerId address
                    ]
                    []

-- | Calculate VA's Sale Tax to an Order.
--
-- This is a temporary measure until Avalara integration is ready. This
-- should be applied to the entire Order amount, minus any store credit.
applyVATax :: Cents -> Cents
applyVATax preTaxTotal =
    Cents . round
        $ (53 % 1000) * toRational (fromCents preTaxTotal)

-- | Calculate the total tax from all of an Order's Products.
getOrderTax :: [OrderLineItem] -> Cents
getOrderTax = sum . map orderLineItemAmount . filter ((==) TaxLine . orderLineItemType)

-- | Calculate the total price from all of an Order's Products.
getOrderSubtotal :: [OrderProduct] -> Cents
getOrderSubtotal = sum . map
    (\prod ->
        orderProductPrice prod * Cents (orderProductQuantity prod)
    )

-- | Calculate the sum of all OrderLineItem charge's for an Order.
-- Returns an `Integer` because the credits may outweigh the charges.
getLineItemTotal :: [OrderLineItem] -> Integer
getLineItemTotal = sum . map
    (\item ->
        let amount = integerCents $ orderLineItemAmount item in
        if orderLineItemType item `elem` creditLineItemTypes then
            (-1) * amount
        else
            amount
    )

-- | Calculate the final price for an Order.
getOrderTotal :: [OrderLineItem] -> [OrderProduct] -> Cents
getOrderTotal lineItems products = Cents . fromIntegral $
    integerCents (getOrderSubtotal products)
        + getLineItemTotal lineItems

-- | Convert a Cents value into it's integer equivalent.
integerCents :: Cents -> Integer
integerCents = toInteger . fromCents


-- AVALARA

-- | Convert a LineItemType into the appropriate Tax Code. Credit
-- lines will return Nothing since they are not reported as LineItems. Tax
-- & StoreCredit lines will also return Nothing since they are not figured
-- into tax calculations.
lineItemToTaxCode :: LineItemType -> Maybe Avalara.TaxCode
lineItemToTaxCode = \case
    TaxLine ->
        Nothing
    ShippingLine ->
        Just Avalara.shippingTaxCode
    PriorityShippingLine ->
        Just Avalara.shippingAndHandlingTaxCode
    SurchargeLine ->
        Just Avalara.handlingOnlyTaxCode
    StoreCreditLine ->
        Nothing
    MemberDiscountLine ->
        Nothing
    CouponDiscountLine ->
        Nothing
    RefundLine ->
        Nothing

-- | Build an Avalara Address Region from a 'Region'. This transforms
-- regions into their ISO codes, or a 3-letter prefix/initials for Custom
-- regions.
avalaraRegion :: Region -> T.Text
avalaraRegion = \case
    USState code ->
        T.pack $ show code
    USArmedForces code ->
        T.pack $ show code
    CAProvince code ->
        T.pack $ show code
    CustomRegion name ->
        case T.words name of
            [] ->
                T.take 3 name
            [_] ->
                T.take 3 name
            words_ ->
                T.take 3 $ T.intercalate "" $ map (T.take 1) words_
