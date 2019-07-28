{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time (UTCTime(..), Day(..), DiffTime, secondsToDiffTime, getCurrentTime)
import Database.Persist.Sql (Entity(..), ToBackendKey, SqlBackend, toSqlKey)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit hiding (assert)

import Models
import Models.Fields
import Routes.CommonData

main :: IO ()
main =
    defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests" [commonData]

commonData :: TestTree
commonData =
    testGroup "CommonData Module"
        [ couponTests
        , priorityFeeTests
        , categorySaleTests
        , productSaleTests
        ]



-- COUPON
couponTests :: TestTree
couponTests = testGroup "Coupon Discount Calculations"
    [ testProperty "Free Shipping" freeShipping
    , testProperty "Free Shipping with no methods" freeShippingNoMethods
    , testProperty "Percentage Discount" percentageDiscount
    , testProperty "Flat Discount" flatDiscount
    ]
  where
    couponWithType :: CouponType -> Gen Coupon
    couponWithType couponType = do
        coupon <- genCoupon
        return $ coupon { couponDiscount = couponType }
    freeShipping :: Property
    freeShipping = property $ do
        coupon <- forAll $ couponWithType FreeShipping
        shippingCharge <- forAll genCartCharge
        calculateCouponDiscount coupon [shippingCharge] 0 === ccAmount shippingCharge
    freeShippingNoMethods :: Property
    freeShippingNoMethods = property $ do
        coupon <- forAll $ couponWithType FreeShipping
        calculateCouponDiscount coupon [] 0 === 0
    percentageDiscount :: Property
    percentageDiscount = property $ do
        coupon <- forAll genCoupon
        percent <- case couponDiscount coupon of
            PercentageDiscount wholePercent ->
                return wholePercent
            _ ->
                forAll genWholePercentage
        let coupon_ = coupon { couponDiscount = PercentageDiscount percent }
        subTotal <- fromCents <$> forAll genCents
        calculateCouponDiscount coupon_ [] subTotal
            === Cents (round (toRational subTotal * (fromIntegral percent % 100)))
    flatDiscount :: Property
    flatDiscount = property $ do
        coupon <- forAll genCoupon
        amount <- case couponDiscount coupon of
            FlatDiscount amt ->
                return amt
            _ ->
                (+ 1) <$> forAll genCents
        let coupon_ = coupon { couponDiscount = FlatDiscount amount }
        subTotal <- forAll genCents
        let result = calculateCouponDiscount coupon_ [] (fromCents subTotal)
        if amount > subTotal then result === subTotal else result === amount


-- PRIORITY S&H
priorityFeeTests :: TestTree
priorityFeeTests = testGroup "Priority S&H Calculations"
    [ testCase "No Shipping Methods" noMethods
    , testProperty "Priority S&H Not Available" noPriorityAvailable
    , testProperty "Fee Correctly Calculated" calculatedCorrectly
    , testCase "Only Flat Rate Calculation" onlyFlat
    , testCase "Only Percentage Rate Calculation" onlyPercent
    ]
  where
    noMethods :: Assertion
    noMethods =
        calculatePriorityFee [] 9001 @?= Nothing
    noPriorityAvailable :: Property
    noPriorityAvailable = property $ do
        method <- forAll genCartCharge
        (Cents subTotal) <- forAll genCents
        calculatePriorityFee [ShippingCharge method Nothing] subTotal === Nothing
    calculatedCorrectly :: Property
    calculatedCorrectly = property $ do
        method <- forAll genCartCharge
        pr@(PriorityShippingFee (Cents flat) percent) <- forAll genPriorityFee
        (Cents subTotal) <- forAll genCents
        let percentAmount = toRational subTotal * (fromIntegral percent % 100)
        calculatePriorityFee [ShippingCharge method $ Just pr] subTotal
            === Just (Cents $ round $ percentAmount + toRational flat)
    onlyFlat :: Assertion
    onlyFlat =
        calculatePriorityFee [makeShippingCharge (PriorityShippingFee 200 0)] 1000
            @?= Just 200
    onlyPercent :: Assertion
    onlyPercent =
        calculatePriorityFee [makeShippingCharge (PriorityShippingFee 0 10)] 1000
            @?= Just 100
    makeShippingCharge :: PriorityShippingFee -> ShippingCharge
    makeShippingCharge fee = ShippingCharge (CartCharge "" 900) (Just fee)


-- SALES
categorySaleTests :: TestTree
categorySaleTests = testGroup "Category Sale Calculations"
    [ testProperty "Flat Amount Is Subtracted From Price" testFlatLessThanPrice
    , testProperty "Flat Amount >= Price Makes Product Free" testFlatGreaterThanPrice
    , testProperty "Percentage Sale Calculations" testPercentageProperty
    , testCase "Percentage Sale Calculation" testPercentageUnit
    , testProperty "Overrides An Existing Sale Price If Cheaper" testOverridesSalePrice
    , testProperty "Doesn't Override An Existing Sale Price If More Expensive" testNoOverridesSalePrice
    ]
  where
    testFlatLessThanPrice :: Property
    testFlatLessThanPrice = property $ do
        variantEntity <- forAll $ genEntity genProductVariant
        let variantData = makeVariantData variantEntity Nothing
        saleAmount <- forAll $ genCentRange $ Range.linear 1 (fromCents (getVariantPrice variantData) - 1)
        sale <- forAll $ genCategorySale $ FlatSale saleAmount
        applyCategorySaleDiscount sale variantData
            === getVariantPrice variantData - saleAmount
    testFlatGreaterThanPrice :: Property
    testFlatGreaterThanPrice = property $ do
        variantEntity <- forAll $ genEntity genProductVariant
        let variantData = makeVariantData variantEntity Nothing
            price = fromCents $ getVariantPrice variantData
        saleAmount <- forAll $ genCentRange $ Range.linear price (price * 10)
        sale <- forAll $ genCategorySale $ FlatSale saleAmount
        applyCategorySaleDiscount sale variantData === 0
    testPercentageProperty :: Property
    testPercentageProperty = property $ do
        variantEntity <- forAll $ genEntity genProductVariant
        let variantData = makeVariantData variantEntity Nothing
        salePercent <- forAll genWholePercentage
        sale <- forAll $ genCategorySale $ PercentSale salePercent
        let discountPercent = 1 - (fromIntegral salePercent % 100)
        applyCategorySaleDiscount sale variantData ===
            Cents (round $ toRational (fromCents $ getVariantPrice variantData) * discountPercent)
    testPercentageUnit :: Assertion
    testPercentageUnit = do
        time <- getCurrentTime
        let variantData = makeVariantData (makeVariant 1000) Nothing
            sale = CategorySale "" (PercentSale 13) time time []
        applyCategorySaleDiscount sale variantData @?= 870
    testOverridesSalePrice :: Property
    testOverridesSalePrice = property $ do
        (variantEntity, price) <- forAll
            $ makeVariantWithPrice $ Range.linear 2000 5000
        let variantData = makeVariantData variantEntity (Just $ price - 1500)
        saleAmount <- forAll $ genCentRange $ Range.linear 1501 $ fromCents price
        sale <- forAll $ genCategorySale $ FlatSale saleAmount
        getVariantPrice (applyCategorySale sale variantData) === price - saleAmount
    testNoOverridesSalePrice :: Property
    testNoOverridesSalePrice = property $ do
        (variantEntity, price) <- forAll
            $ makeVariantWithPrice $ Range.linear 2000 5000
        let variantData = makeVariantData variantEntity (Just 200)
        sale <- forAll $ do
            let price_ = fromCents price
            amount <- genCentRange $ Range.linear 0 (price_ - 199)
            genCategorySale $ FlatSale amount
        getVariantPrice (applyCategorySale sale variantData) === 200
    makeVariant :: Cents -> Entity ProductVariant
    makeVariant price =
        Entity (toSqlKey 1)
            $ ProductVariant (toSqlKey 1) "" price 1 (Milligrams 1) True
    makeVariantWithPrice :: Range Natural -> Gen (Entity ProductVariant, Cents)
    makeVariantWithPrice priceRange = do
        entity <- genProductVariant
        key <- genEntityKey
        price <- genCentRange priceRange
        let entityWithPrice = entity { productVariantPrice = price }
        return (Entity key entityWithPrice, price)

productSaleTests :: TestTree
productSaleTests = testGroup "Product Sale Calculations"
    [ testProperty "Sale Price Set If Less Than Price" testSalePrice
    , testProperty "Sale Price Not Set If Greater Than or Equal to Price" testNoSalePrice
    ]
  where
    testSalePrice :: Property
    testSalePrice = property $ do
        variant <- forAll $ genEntity genProductVariant
        let normalPrice = productVariantPrice $ entityVal variant
        salePrice <- forAll $ genCentRange $ Range.linear 0 (fromCents normalPrice - 1)
        getVariantPrice (makeVariantData variant $ Just salePrice) === salePrice
    testNoSalePrice :: Property
    testNoSalePrice = property $ do
        variant <- forAll $ genEntity genProductVariant
        let normalPrice = fromCents $ productVariantPrice $ entityVal variant
        salePrice <- forAll $ genCentRange $ Range.linear (normalPrice + 1) (normalPrice * 10)
        getVariantPrice (makeVariantData variant $ Just salePrice)
            === Cents normalPrice




-- GENERATORS


-- Generate an active coupon with minimum order size of 0 to $10.00
genCoupon :: Gen Coupon
genCoupon =
    Coupon
        <$> genText
        <*> genText
        <*> genText
        <*> pure True
        <*> genCouponType
        <*> genCentRange (Range.linear 0 1000)
        <*> genUTCTime
        <*> pure 0
        <*> pure 0
        <*> genUTCTime

genCouponType :: Gen CouponType
genCouponType =
    Gen.choice
        [ FlatDiscount <$> genCents
        , PercentageDiscount <$> genWholePercentage
        , pure FreeShipping
        ]

-- Generate charges of $0.01 to $10.00
genCartCharge :: Gen CartCharge
genCartCharge =
    CartCharge
        <$> genText
        <*> genCentRange (Range.linear 1 1000)

genPriorityFee :: Gen PriorityShippingFee
genPriorityFee =
    PriorityShippingFee
        <$> genCentRange (Range.linear 1 1000)
        <*> genWholePercentage

genProductVariant :: Gen ProductVariant
genProductVariant =
    ProductVariant
        <$> fmap toSqlKey (Gen.integral $ Range.linear 1 100)
        <*> genText
        <*> genCentRange (Range.linear 1 999999)
        <*> Gen.integral (Range.linear 1 1000)
        <*> fmap Milligrams (Gen.integral (Range.linear 1 1000))
        <*> Gen.bool

genCategorySale :: SaleType -> Gen CategorySale
genCategorySale saleType =
    CategorySale
        <$> genText
        <*> pure saleType
        <*> genUTCTime
        <*> genUTCTime
        <*> Gen.list (Range.linear 1 10) genEntityKey


genEntity :: (ToBackendKey SqlBackend a) => Gen a -> Gen (Entity a)
genEntity genModel =
    Entity <$> genEntityKey <*> genModel


genEntityKey :: (ToBackendKey SqlBackend a) => Gen (Key a)
genEntityKey =
    toSqlKey <$> Gen.int64 (Range.linear 1 1000)


genCentRange :: Range Natural -> Gen Cents
genCentRange r =
    Cents <$> Gen.integral r

genCents :: Gen Cents
genCents = genCentRange $ Range.linear 0 999999

genWholePercentage :: Gen Percent
genWholePercentage = Gen.integral $ Range.linear 1 100

genText :: MonadGen m => m Text
genText = Gen.text (Range.linear 1 10) Gen.alpha

genUTCTime :: Gen UTCTime
genUTCTime =
    UTCTime
        <$> genDay
        <*> genTime
  where
    genDay :: Gen Day
    genDay = ModifiedJulianDay <$> Gen.integral (Range.linear 0 999999)
    genTime :: Gen DiffTime
    genTime = secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)
