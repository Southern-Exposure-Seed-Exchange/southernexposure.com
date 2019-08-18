module PageData exposing
    ( AddressDetails
    , AdvancedSearch
    , CartDetails
    , CartItem
    , CartItemId(..)
    , CategoryDetails
    , CheckoutDetails
    , LineItemType(..)
    , MyAccount
    , OrderDetails
    , OrderSummary
    , PageData
    , PredecessorCategory
    , ProductData
    , ProductDetails
    , SearchResults
    , addressDetailsDecoder
    , advancedSearchDecoder
    , blankCartDetails
    , cartDetailsDecoder
    , cartTotals
    , categoryConfig
    , categoryDetailsDecoder
    , checkoutDetailsDecoder
    , initial
    , isFreeCheckout
    , myAccountDecoder
    , orderDetailsDecoder
    , orderTotals
    , productDataDecoder
    , productDetailsDecoder
    , searchConfig
    , statusText
    )

import Address
import Api
import Category exposing (Category, CategoryId(..))
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), Milligrams(..), centsDecoder, centsMap, centsMap2)
import Paginate exposing (Paginated)
import Product exposing (Product, ProductVariant, ProductVariantId(..), variantPrice)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import RemoteData exposing (WebData)
import Search
import SeedAttribute exposing (SeedAttribute)
import StaticPage exposing (StaticPage)
import Time exposing (Posix)



-- MODEL


type alias PageData =
    { categoryDetails : Paginated ProductData { slug : String, sorting : Sorting.Option } CategoryDetails
    , productDetails : WebData ProductDetails
    , advancedSearch : WebData AdvancedSearch
    , searchResults : Paginated ProductData { data : Search.Data, sorting : Sorting.Option } String
    , pageDetails : WebData StaticPage
    , locations : WebData AddressLocations
    , myAccount : WebData MyAccount
    , addressDetails : WebData AddressDetails
    , cartDetails : WebData CartDetails
    , checkoutDetails : WebData CheckoutDetails
    , orderDetails : WebData OrderDetails
    }


initial : PageData
initial =
    let
        categoryPaginate =
            Paginate.initial categoryConfig
                { slug = "", sorting = Sorting.default }
                (.page Pagination.default)
                (.perPage Pagination.default)
                |> Tuple.first

        searchPaginate =
            Paginate.initial searchConfig
                { data = Search.initial, sorting = Sorting.default }
                (.page Pagination.default)
                (.perPage Pagination.default)
                |> Tuple.first
    in
    { categoryDetails = categoryPaginate
    , productDetails = RemoteData.NotAsked
    , advancedSearch = RemoteData.NotAsked
    , searchResults = searchPaginate
    , pageDetails = RemoteData.NotAsked
    , locations = RemoteData.NotAsked
    , myAccount = RemoteData.NotAsked
    , addressDetails = RemoteData.NotAsked
    , cartDetails = RemoteData.NotAsked
    , checkoutDetails = RemoteData.NotAsked
    , orderDetails = RemoteData.NotAsked
    }



-- Category Details


type alias CategoryDetails =
    { category : Category
    , subCategories : List Category
    , predecessors : List PredecessorCategory
    }


categoryDetailsDecoder : Decoder CategoryDetails
categoryDetailsDecoder =
    Decode.map3 CategoryDetails
        (Decode.field "category" Category.decoder)
        (Decode.field "subCategories" <| Decode.list Category.decoder)
        (Decode.field "predecessors" <|
            Decode.map List.reverse <|
                Decode.list predecessorCategoryDecoder
        )


categoryConfig : Paginate.Config ProductData { slug : String, sorting : Sorting.Option } CategoryDetails
categoryConfig =
    let
        request { slug, sorting } page perPage =
            Api.get (Api.CategoryDetails slug <| Pagination.Data page perPage sorting)
                |> Api.withJsonResponse fetchDecoder
                |> Api.sendRequest identity

        fetchDecoder =
            Decode.map3 Paginate.FetchResponse
                (Decode.field "products" <| Decode.list productDataDecoder)
                (Decode.field "total" Decode.int)
                (Decode.map Just categoryDetailsDecoder)
    in
    Paginate.makeConfig request



-- Product Details


type alias ProductDetails =
    { product : Product
    , variants : Dict Int ProductVariant
    , maybeSeedAttribute : Maybe SeedAttribute
    , categories : List Category
    , predecessors : List PredecessorCategory
    }


productDetailsDecoder : Decoder ProductDetails
productDetailsDecoder =
    Decode.map5 ProductDetails
        (Decode.field "product" Product.decoder)
        (Decode.field "variants" variantDictDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable SeedAttribute.decoder)
        (Decode.field "categories" <| Decode.list Category.decoder)
        (Decode.field "predecessors" <|
            Decode.map List.reverse <|
                Decode.list predecessorCategoryDecoder
        )



-- Search Results


type alias SearchResults =
    Paginated ProductData { data : Search.Data, sorting : Sorting.Option } String


searchConfig : Paginate.Config ProductData { data : Search.Data, sorting : Sorting.Option } String
searchConfig =
    let
        fetchDecoder =
            Decode.map3 Paginate.FetchResponse
                (Decode.field "products" <| Decode.list productDataDecoder)
                (Decode.field "total" Decode.int)
                (Decode.field "categoryName" <| Decode.nullable Decode.string)

        request { data, sorting } page perPage =
            Api.post (Api.ProductSearch <| Pagination.Data page perPage sorting)
                |> Api.withJsonBody (Search.encode data)
                |> Api.withJsonResponse fetchDecoder
                |> Api.sendRequest identity
    in
    Paginate.makeConfig request



-- My Account


type alias MyAccount =
    { orderSummaries : List OrderSummary
    , storeCredit : Cents
    }


myAccountDecoder : Decoder MyAccount
myAccountDecoder =
    Decode.map2 MyAccount
        (Decode.field "orderDetails" <| Decode.list orderSummaryDecoder)
        (Decode.field "storeCredit" centsDecoder)


type alias OrderSummary =
    { id : Int
    , shippingAddress : Address.Model
    , status : OrderStatus
    , total : Cents
    , created : Posix
    }


orderSummaryDecoder : Decoder OrderSummary
orderSummaryDecoder =
    Decode.map5 OrderSummary
        (Decode.field "id" Decode.int)
        (Decode.field "shippingAddress" Address.decoder)
        (Decode.field "status" orderStatusDecoder)
        (Decode.field "total" centsDecoder)
        (Decode.field "created" Iso8601.decoder)



-- Address Details


type alias AddressDetails =
    { shippingAddresses : List Address.Model
    , billingAddresses : List Address.Model
    }


addressDetailsDecoder : Decoder AddressDetails
addressDetailsDecoder =
    Decode.map2 AddressDetails
        (Decode.field "shippingAddresses" <| Decode.list Address.decoder)
        (Decode.field "billingAddresses" <| Decode.list Address.decoder)



-- Carts


type alias CartDetails =
    { items : List CartItem
    , charges : CartCharges
    }


blankCartDetails : CartDetails
blankCartDetails =
    CartDetails []
        (CartCharges (CartCharge "" (Cents 0))
            []
            Nothing
            Nothing
            Nothing
            Nothing
            (Cents 0)
        )


cartDetailsDecoder : Decoder CartDetails
cartDetailsDecoder =
    Decode.map2 CartDetails
        (Decode.field "items" <| Decode.list cartItemDecoder)
        (Decode.field "charges" cartChargesDecoder)


cartTotals : { a | items : List CartItem, charges : CartCharges } -> { subTotal : Cents, total : Cents }
cartTotals { items, charges } =
    let
        subTotal =
            List.foldl (\item acc -> itemTotal item |> addCents acc) (Cents 0) items

        memberDiscount =
            maybeAmount charges.memberDiscount

        couponDiscount =
            maybeAmount charges.couponDiscount

        maybeAmount =
            Maybe.map .amount >> Maybe.withDefault (Cents 0)

        shippingAmount =
            charges.shippingMethod
                |> Maybe.map (.charge >> .amount)
                |> Maybe.withDefault (Cents 0)

        priorityAmount =
            maybeAmount charges.priorityShipping

        surchargeAmount =
            charges.surcharges |> List.foldl (\i -> addCents i.amount) (Cents 0)

        taxAmount =
            List.foldl (\item acc -> item.tax |> addCents acc) (Cents 0) items

        itemTotal { variant, quantity } =
            variantPrice variant
                |> (centsMap <| (*) quantity)

        total =
            subTotal
                |> addCents surchargeAmount
                |> addCents shippingAmount
                |> addCents priorityAmount
                |> addCents taxAmount
                |> subtractCents memberDiscount
                |> subtractCents couponDiscount

        addCents =
            centsMap2 (+)

        subtractCents =
            centsMap2 (\toRemove amount -> amount - toRemove)
    in
    { subTotal = subTotal
    , total = total
    }



-- Checkout Details


type alias CheckoutDetails =
    { shippingAddresses : List Address.Model
    , billingAddresses : List Address.Model
    , items : List CartItem
    , charges : CartCharges
    , storeCredit : Cents
    , memberNumber : String
    }


checkoutDetailsDecoder : Decoder CheckoutDetails
checkoutDetailsDecoder =
    Decode.map6 CheckoutDetails
        (Decode.field "shippingAddresses" <| Decode.list Address.decoder)
        (Decode.field "billingAddresses" <| Decode.list Address.decoder)
        (Decode.field "items" <| Decode.list cartItemDecoder)
        (Decode.field "charges" cartChargesDecoder)
        (Decode.field "storeCredit" centsDecoder)
        (Decode.field "memberNumber" Decode.string)


isFreeCheckout : WebData CheckoutDetails -> Bool
isFreeCheckout =
    RemoteData.toMaybe
        >> Maybe.map (.charges >> .grandTotal >> (\t -> t == Cents 0))
        >> Maybe.withDefault False



-- Order Details


type alias OrderDetails =
    { order : Order
    , lineItems : List OrderLineItem
    , products : List OrderProduct
    , shippingAddress : Address.Model
    , billingAddress : Maybe Address.Model
    }


orderDetailsDecoder : Decoder OrderDetails
orderDetailsDecoder =
    Decode.map5 OrderDetails
        (Decode.field "order" orderDecoder)
        (Decode.field "lineItems" <| Decode.list lineItemDecoder)
        (Decode.field "products" <| Decode.list orderProductDecoder)
        (Decode.field "shippingAddress" Address.decoder)
        (Decode.field "billingAddress" <| Decode.nullable Address.decoder)


orderTotals : OrderDetails -> { subTotal : Cents, tax : Cents, total : Cents }
orderTotals { lineItems, products } =
    let
        ( subTotal, tax ) =
            List.foldl
                (\item ( runningTotal, runningTax ) ->
                    ( centsMap ((*) item.quantity) item.price
                        |> centsMap2 (+) runningTotal
                    , centsMap2 (+) item.tax runningTax
                    )
                )
                ( Cents 0, Cents 0 )
                products

        ( credits, debits ) =
            List.foldl
                (\charge ( cs, ds ) ->
                    case charge.itemType of
                        Shipping ->
                            ( cs, centsMap2 (+) charge.amount ds )

                        Surcharge ->
                            ( cs, centsMap2 (+) charge.amount ds )

                        PriorityShipping ->
                            ( cs, centsMap2 (+) charge.amount ds )

                        StoreCredit ->
                            ( centsMap2 (+) charge.amount cs, ds )

                        MemberDiscount ->
                            ( centsMap2 (+) charge.amount cs, ds )

                        CouponDiscount ->
                            ( centsMap2 (+) charge.amount cs, ds )
                )
                ( Cents 0, Cents 0 )
                lineItems

        total =
            subTotal
                |> centsMap2 (+) debits
                |> centsMap2 (+) tax
                |> centsMap2 (\c t -> t - c) credits
    in
    { subTotal = subTotal
    , tax = tax
    , total = total
    }


type alias Order =
    { status : OrderStatus
    , comment : String
    , taxDescription : String
    , createdAt : Posix
    }


orderDecoder : Decoder Order
orderDecoder =
    Decode.map4 Order
        (Decode.field "status" orderStatusDecoder)
        (Decode.field "comment" Decode.string)
        (Decode.field "taxDescription" Decode.string)
        (Decode.field "createdAt" Iso8601.decoder)


type OrderStatus
    = Processing
    | OrderReceived
    | PaymentReceived
    | PaymentFailed
    | Refunded
    | Shipped


orderStatusDecoder : Decoder OrderStatus
orderStatusDecoder =
    decodeStringWith <|
        \str ->
            case str of
                "Processing" ->
                    Decode.succeed Processing

                "OrderReceived" ->
                    Decode.succeed OrderReceived

                "PaymentReceived" ->
                    Decode.succeed PaymentReceived

                "PaymentFailed" ->
                    Decode.succeed PaymentFailed

                "Refunded" ->
                    Decode.succeed Refunded

                "Delivered" ->
                    Decode.succeed Shipped

                _ ->
                    Decode.fail <| "Invalid OrderStatus: " ++ str


statusText : OrderStatus -> String
statusText status =
    case status of
        Processing ->
            "Processing"

        OrderReceived ->
            "Received"

        PaymentReceived ->
            "Payment Complete"

        PaymentFailed ->
            "Payment Failed"

        Refunded ->
            "Refunded"

        Shipped ->
            "Shipped"


type alias OrderLineItem =
    { itemType : LineItemType
    , description : String
    , amount : Cents
    }


lineItemDecoder : Decoder OrderLineItem
lineItemDecoder =
    Decode.map3 OrderLineItem
        (Decode.field "type" lineItemTypeDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "amount" centsDecoder)


type LineItemType
    = Shipping
    | Surcharge
    | StoreCredit
    | MemberDiscount
    | PriorityShipping
    | CouponDiscount


lineItemTypeDecoder : Decoder LineItemType
lineItemTypeDecoder =
    decodeStringWith <|
        \str ->
            case str of
                "ShippingLine" ->
                    Decode.succeed Shipping

                "SurchargeLine" ->
                    Decode.succeed Surcharge

                "StoreCreditLine" ->
                    Decode.succeed StoreCredit

                "MemberDiscountLine" ->
                    Decode.succeed MemberDiscount

                "PriorityShippingLine" ->
                    Decode.succeed PriorityShipping

                "CouponDiscountLine" ->
                    Decode.succeed CouponDiscount

                _ ->
                    Decode.fail <| "Invalid LineItemType: " ++ str


decodeStringWith : (String -> Decoder a) -> Decoder a
decodeStringWith f =
    Decode.string |> Decode.andThen f


type alias OrderProduct =
    { name : String
    , weight : Milligrams
    , quantity : Int
    , price : Cents
    , tax : Cents
    }


orderProductDecoder : Decoder OrderProduct
orderProductDecoder =
    Decode.map5 OrderProduct
        (Decode.field "name" Decode.string)
        (Decode.field "weight" <| Decode.map Milligrams Decode.int)
        (Decode.field "quantity" Decode.int)
        (Decode.field "price" centsDecoder)
        (Decode.field "tax" centsDecoder)



-- Common Page Data


type alias PredecessorCategory =
    { id : CategoryId
    , slug : String
    , name : String
    }


predecessorCategoryDecoder : Decoder PredecessorCategory
predecessorCategoryDecoder =
    Decode.map3 PredecessorCategory
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "slug" Decode.string)
        (Decode.field "name" Decode.string)


type alias ProductData =
    ( Product, Dict Int ProductVariant, Maybe SeedAttribute )


productDataDecoder : Decoder ProductData
productDataDecoder =
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.field "product" Product.decoder)
        (Decode.field "variants" variantDictDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable SeedAttribute.decoder)


variantDictDecoder : Decoder (Dict Int ProductVariant)
variantDictDecoder =
    Decode.list Product.variantDecoder
        |> Decode.map
            (List.foldl
                (\v -> Dict.insert ((\(ProductVariantId i) -> i) v.id) v)
                Dict.empty
            )


type alias AdvancedSearch =
    List AdvancedSearchCategory


type alias AdvancedSearchCategory =
    { id : CategoryId
    , name : String
    }


advancedSearchDecoder : Decoder AdvancedSearch
advancedSearchDecoder =
    Decode.field "categories" <|
        Decode.list <|
            Decode.map2 AdvancedSearchCategory
                (Decode.field "id" <| Decode.map CategoryId Decode.int)
                (Decode.field "name" Decode.string)


type CartItemId
    = CartItemId Int


type alias CartItem =
    { id : CartItemId
    , product : Product
    , variant : ProductVariant
    , quantity : Int
    , tax : Cents
    }


cartItemDecoder : Decoder CartItem
cartItemDecoder =
    Decode.map5 CartItem
        (Decode.field "id" <| Decode.map CartItemId Decode.int)
        (Decode.field "product" Product.decoder)
        (Decode.field "variant" Product.variantDecoder)
        (Decode.field "quantity" Decode.int)
        (Decode.field "tax" centsDecoder)


type alias CartCharge =
    { description : String
    , amount : Cents
    }


cartChargeDecoder : Decoder CartCharge
cartChargeDecoder =
    Decode.map2 CartCharge
        (Decode.field "description" Decode.string)
        (Decode.field "amount" centsDecoder)


{-| TODO: Currently grand total is only used to see if checkout is free. Should
replace most calculations with simply using the grandTotal field.

TODO: Use the productTotal field that the server returns as well.

-}
type alias CartCharges =
    { tax : CartCharge
    , surcharges : List CartCharge
    , shippingMethod : Maybe ShippingCharge
    , priorityShipping : Maybe CartCharge
    , memberDiscount : Maybe CartCharge
    , couponDiscount : Maybe CartCharge
    , grandTotal : Cents
    }


cartChargesDecoder : Decoder CartCharges
cartChargesDecoder =
    Decode.map7 CartCharges
        (Decode.field "tax" cartChargeDecoder)
        (Decode.field "surcharges" <| Decode.list cartChargeDecoder)
        (Decode.field "shippingMethods" <|
            Decode.map List.head <|
                Decode.list shippingChargeDecoder
        )
        (Decode.field "priorityShipping" <| Decode.nullable cartChargeDecoder)
        (Decode.field "memberDiscount" <| Decode.nullable cartChargeDecoder)
        (Decode.field "couponDiscount" <| Decode.nullable cartChargeDecoder)
        (Decode.field "grandTotal" centsDecoder)


type alias ShippingCharge =
    { charge : CartCharge
    , priorityFee : Maybe PriorityFee
    }


shippingChargeDecoder : Decoder ShippingCharge
shippingChargeDecoder =
    Decode.map2 ShippingCharge
        (Decode.field "charge" cartChargeDecoder)
        (Decode.field "priorityFee" <| Decode.nullable priorityFeeDecoder)


type alias PriorityFee =
    { flat : Cents
    , percent : Int
    }


priorityFeeDecoder : Decoder PriorityFee
priorityFeeDecoder =
    Decode.map2 PriorityFee
        (Decode.field "flat" centsDecoder)
        (Decode.field "percent" Decode.int)
