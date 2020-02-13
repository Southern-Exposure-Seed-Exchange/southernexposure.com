module PageData exposing
    ( AddressDetails
    , AdminCategoryListData
    , AdminComment
    , AdminCouponListData
    , AdminEditCategoryData
    , AdminEditCouponData
    , AdminEditCustomerData
    , AdminEditPageData
    , AdminListCategory(..)
    , AdminListCoupon
    , AdminListPage
    , AdminListProduct
    , AdminNewCategoryData
    , AdminOrderDetails
    , AdminPageListData
    , AdminProductCategory
    , AdminProductListData
    , AdminSharedProductData
    , AdvancedSearch
    , CartDetails
    , CartItem
    , CartItemId(..)
    , CategoryDetails
    , CheckoutDetails
    , CouponType(..)
    , CustomerData
    , LineItemType(..)
    , MyAccount
    , OrderData
    , OrderDetails
    , OrderLineItem
    , OrderProduct
    , OrderSummary
    , PageData
    , PredecessorCategory
    , ProductData
    , ProductDetails
    , SearchResults
    , addressDetailsDecoder
    , adminCategoryListDataDecoder
    , adminCouponListDataDecoder
    , adminEditCategoryDataDecoder
    , adminEditCouponDataDecoder
    , adminEditCustomerDataDecoder
    , adminEditPageDataDecoder
    , adminListPageDecoder
    , adminNewCategoryDataDecoder
    , adminNewProductDataDecoder
    , adminOrderDetailsDecoder
    , adminPageListDataDecoder
    , adminProductListDataDecoder
    , advancedSearchDecoder
    , blankCartDetails
    , cartDetailsDecoder
    , cartTotals
    , categoryConfig
    , categoryDetailsDecoder
    , checkoutDetailsDecoder
    , couponTypeEncoder
    , customersConfig
    , initial
    , isFreeCheckout
    , lineItemDecoder
    , myAccountDecoder
    , orderDetailsDecoder
    , orderProductDecoder
    , orderTotals
    , ordersConfig
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
import Json.Encode as Encode exposing (Value)
import Locations exposing (AddressLocations)
import Models.Fields exposing (Cents(..), ImageData, LotSize, centsDecoder, centsEncoder, centsMap, centsMap2, imageDecoder, lotSizeDecoder)
import Paginate exposing (Paginated)
import Product exposing (Product, ProductId, ProductVariant, ProductVariantId(..), variantPrice)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import RemoteData exposing (WebData)
import Search
import SeedAttribute exposing (SeedAttribute)
import StaticPage exposing (StaticPage, StaticPageId)
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
    , adminCategoryList : WebData AdminCategoryListData
    , adminNewCategory : WebData AdminNewCategoryData
    , adminEditCategory : WebData AdminEditCategoryData
    , adminPageList : WebData AdminPageListData
    , adminEditPage : WebData AdminEditPageData
    , adminOrderList : Paginated OrderData String ()
    , adminOrderDetails : WebData AdminOrderDetails
    , adminCustomerList : Paginated CustomerData String ()
    , adminEditCustomer : WebData AdminEditCustomerData
    , adminProductList : WebData AdminProductListData
    , adminSharedProduct : WebData AdminSharedProductData
    , adminCouponList : WebData AdminCouponListData
    , adminEditCoupon : WebData AdminEditCouponData
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

        ordersPaginate =
            Paginate.initial ordersConfig "" 1 50
                |> Tuple.first

        customersPaginate =
            Paginate.initial customersConfig "" 1 50
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
    , adminCategoryList = RemoteData.NotAsked
    , adminNewCategory = RemoteData.NotAsked
    , adminEditCategory = RemoteData.NotAsked
    , adminPageList = RemoteData.NotAsked
    , adminEditPage = RemoteData.NotAsked
    , adminOrderList = ordersPaginate
    , adminOrderDetails = RemoteData.NotAsked
    , adminCustomerList = customersPaginate
    , adminEditCustomer = RemoteData.NotAsked
    , adminProductList = RemoteData.NotAsked
    , adminSharedProduct = RemoteData.NotAsked
    , adminCouponList = RemoteData.NotAsked
    , adminEditCoupon = RemoteData.NotAsked
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

        itemTotal { variant, quantity } =
            variantPrice variant
                |> (centsMap <| (*) quantity)

        total =
            subTotal
                |> addCents surchargeAmount
                |> addCents shippingAmount
                |> addCents priorityAmount
                |> addCents charges.tax.amount
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
    }


checkoutDetailsDecoder : Decoder CheckoutDetails
checkoutDetailsDecoder =
    Decode.map5 CheckoutDetails
        (Decode.field "shippingAddresses" <| Decode.list Address.decoder)
        (Decode.field "billingAddresses" <| Decode.list Address.decoder)
        (Decode.field "items" <| Decode.list cartItemDecoder)
        (Decode.field "charges" cartChargesDecoder)
        (Decode.field "storeCredit" centsDecoder)


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


orderTotals : OrderDetails -> { subTotal : Cents, total : Cents }
orderTotals { lineItems, products } =
    let
        subTotal =
            List.foldl
                (\item runningTotal ->
                    centsMap ((*) item.quantity) item.price
                        |> centsMap2 (+) runningTotal
                )
                (Cents 0)
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

                        Tax ->
                            ( cs, centsMap2 (+) charge.amount ds )

                        StoreCredit ->
                            ( centsMap2 (+) charge.amount cs, ds )

                        MemberDiscount ->
                            ( centsMap2 (+) charge.amount cs, ds )

                        CouponDiscount ->
                            ( centsMap2 (+) charge.amount cs, ds )

                        Refund ->
                            ( centsMap2 (+) charge.amount cs, ds )
                )
                ( Cents 0, Cents 0 )
                lineItems

        total =
            subTotal
                |> centsMap2 (+) debits
                |> centsMap2 (\c t -> t - c) credits
    in
    { subTotal = subTotal
    , total = total
    }


type alias Order =
    { id : Int
    , status : OrderStatus
    , comment : String
    , createdAt : Posix
    }


orderDecoder : Decoder Order
orderDecoder =
    Decode.map4 Order
        (Decode.field "id" Decode.int)
        (Decode.field "status" orderStatusDecoder)
        (Decode.field "comment" Decode.string)
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
    | Refund
    | Tax


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

                "RefundLine" ->
                    Decode.succeed Refund

                "TaxLine" ->
                    Decode.succeed Tax

                _ ->
                    Decode.fail <| "Invalid LineItemType: " ++ str


decodeStringWith : (String -> Decoder a) -> Decoder a
decodeStringWith f =
    Decode.string |> Decode.andThen f


type alias OrderProduct =
    { name : String
    , sku : String
    , lotSize : Maybe LotSize
    , quantity : Int
    , price : Cents
    }


orderProductDecoder : Decoder OrderProduct
orderProductDecoder =
    Decode.map5 OrderProduct
        (Decode.field "name" Decode.string)
        (Decode.field "sku" Decode.string)
        (Decode.field "lotSize" <| Decode.nullable lotSizeDecoder)
        (Decode.field "quantity" Decode.int)
        (Decode.field "price" centsDecoder)



-- Category Admin


type alias AdminCategoryListData =
    { roots : List AdminListCategory
    }


adminCategoryListDataDecoder : Decoder AdminCategoryListData
adminCategoryListDataDecoder =
    Decode.map AdminCategoryListData
        (Decode.field "roots" <| Decode.list adminListCategoryDecoder)


type AdminListCategory
    = AdminListCategory
        { id : CategoryId
        , name : String
        , children : List AdminListCategory
        }


adminListCategoryDecoder : Decoder AdminListCategory
adminListCategoryDecoder =
    Decode.map3 (\id name children -> AdminListCategory { id = id, name = name, children = children })
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "children" <| Decode.list <| Decode.lazy <| \_ -> adminListCategoryDecoder)


type alias AdminNewCategoryData =
    List { name : String, id : CategoryId }


adminNewCategoryDataDecoder : Decoder AdminNewCategoryData
adminNewCategoryDataDecoder =
    Decode.list <|
        Decode.map2 (\id name -> { id = id, name = name })
            (Decode.field "id" <| Decode.map CategoryId Decode.int)
            (Decode.field "name" Decode.string)


type alias AdminEditCategoryData =
    { id : CategoryId
    , name : String
    , slug : String
    , parent : Maybe CategoryId
    , description : String
    , image : ImageData
    , order : Int
    }


adminEditCategoryDataDecoder : Decoder AdminEditCategoryData
adminEditCategoryDataDecoder =
    Decode.map7 AdminEditCategoryData
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "parentId" <| Decode.maybe <| Decode.map CategoryId Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "image" imageDecoder)
        (Decode.field "order" Decode.int)



-- Page Admin


type alias AdminPageListData =
    { pages : List AdminListPage
    , homePageId : Maybe StaticPageId
    }


adminPageListDataDecoder : Decoder AdminPageListData
adminPageListDataDecoder =
    Decode.map2 AdminPageListData
        (Decode.field "pages" <| Decode.list adminListPageDecoder)
        (Decode.field "homePageId" <| Decode.nullable StaticPage.idDecoder)


type alias AdminListPage =
    { id : StaticPageId
    , name : String
    , slug : String
    }


adminListPageDecoder : Decoder AdminListPage
adminListPageDecoder =
    Decode.map3 AdminListPage
        (Decode.field "id" StaticPage.idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)


type alias AdminEditPageData =
    { id : StaticPageId
    , title : String
    , slug : String
    , content : String
    }


adminEditPageDataDecoder : Decoder AdminEditPageData
adminEditPageDataDecoder =
    Decode.map4 AdminEditPageData
        (Decode.field "id" StaticPage.idDecoder)
        (Decode.field "title" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "content" Decode.string)



-- Order Admin


ordersConfig : Paginate.Config OrderData String ()
ordersConfig =
    let
        request query page perPage =
            Api.get (Api.AdminOrderList page perPage query)
                |> Api.withJsonResponse fetchDecoder
                |> Api.sendRequest identity

        fetchDecoder =
            Decode.map3 Paginate.FetchResponse
                (Decode.field "orders" <| Decode.list orderDataDecoder)
                (Decode.field "total" Decode.int)
                (Decode.succeed <| Just ())
    in
    Paginate.makeConfig request


type alias OrderData =
    { id : Int
    , date : Posix
    , name : String
    , email : String
    , street : String
    , state : Locations.Region
    , status : OrderStatus
    , total : Cents
    }


orderDataDecoder : Decoder OrderData
orderDataDecoder =
    Decode.map8 OrderData
        (Decode.field "id" Decode.int)
        (Decode.field "date" Iso8601.decoder)
        (Decode.field "customerName" Decode.string)
        (Decode.field "customerEmail" Decode.string)
        (Decode.field "shippingStreet" Decode.string)
        (Decode.field "shippingRegion" Locations.regionDecoder)
        (Decode.field "orderStatus" orderStatusDecoder)
        (Decode.field "orderTotal" centsDecoder)


type alias AdminOrderDetails =
    { details : OrderDetails
    , adminComments : List AdminComment
    , stripeId : Maybe String
    , customerId : Int
    }


adminOrderDetailsDecoder : Decoder AdminOrderDetails
adminOrderDetailsDecoder =
    Decode.map4 AdminOrderDetails
        (Decode.field "details" orderDetailsDecoder)
        (Decode.field "adminComments" <| Decode.list adminCommentDecoder)
        (Decode.field "stripeId" <| Decode.nullable Decode.string)
        (Decode.field "customerId" Decode.int)


type alias AdminComment =
    { time : Posix
    , content : String
    }


adminCommentDecoder : Decoder AdminComment
adminCommentDecoder =
    Decode.map2 AdminComment
        (Decode.field "time" Iso8601.decoder)
        (Decode.field "content" Decode.string)



-- Customer Admin


customersConfig : Paginate.Config CustomerData String ()
customersConfig =
    let
        request query page perPage =
            Api.get (Api.AdminCustomerList page perPage query)
                |> Api.withJsonResponse fetchDecoder
                |> Api.sendRequest identity

        fetchDecoder =
            Decode.map3 Paginate.FetchResponse
                (Decode.field "customers" <| Decode.list customerDataDecoder)
                (Decode.field "total" Decode.int)
                (Decode.succeed <| Just ())
    in
    Paginate.makeConfig request


type alias CustomerData =
    { id : Int
    , email : String
    , name : String
    }


customerDataDecoder : Decoder CustomerData
customerDataDecoder =
    Decode.map3 CustomerData
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "name" Decode.string)


type alias AdminEditCustomerData =
    { id : Int
    , email : String
    , storeCredit : Cents
    , isAdmin : Bool
    , stripeId : Maybe String
    , orders : List CustomerOrderData
    }


adminEditCustomerDataDecoder : Decoder AdminEditCustomerData
adminEditCustomerDataDecoder =
    Decode.map6 AdminEditCustomerData
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "storeCredit" centsDecoder)
        (Decode.field "isAdmin" Decode.bool)
        (Decode.field "stripeId" <| Decode.nullable Decode.string)
        (Decode.field "orders" <| Decode.list customerOrderDataDecoder)


type alias CustomerOrderData =
    { id : Int
    , date : Posix
    , status : OrderStatus
    , shipping : Address.Model
    , total : Cents
    }


customerOrderDataDecoder : Decoder CustomerOrderData
customerOrderDataDecoder =
    Decode.map5 CustomerOrderData
        (Decode.field "id" Decode.int)
        (Decode.field "date" Iso8601.decoder)
        (Decode.field "status" orderStatusDecoder)
        (Decode.field "shipping" Address.decoder)
        (Decode.field "total" centsDecoder)



-- Product Admin


type alias AdminProductListData =
    { products : List AdminListProduct }


adminProductListDataDecoder : Decoder AdminProductListData
adminProductListDataDecoder =
    Decode.map AdminProductListData
        (Decode.field "products" <| Decode.list adminListProductDecoder)


type alias AdminListProduct =
    { id : ProductId
    , name : String
    , baseSku : String
    , categories : List String
    , isActive : Bool
    }


adminListProductDecoder : Decoder AdminListProduct
adminListProductDecoder =
    Decode.map5 AdminListProduct
        (Decode.field "id" Product.idDecoder)
        (Decode.field "name" Decode.string)
        (Decode.field "baseSKU" Decode.string)
        (Decode.field "categories" <| Decode.list Decode.string)
        (Decode.field "isActive" Decode.bool)


type alias AdminSharedProductData =
    { categories : List AdminProductCategory
    }


adminNewProductDataDecoder : Decoder AdminSharedProductData
adminNewProductDataDecoder =
    Decode.map AdminSharedProductData <|
        Decode.field "categories" <|
            Decode.list adminProductCategoryDecoder


type alias AdminProductCategory =
    { id : CategoryId
    , name : String
    }


adminProductCategoryDecoder : Decoder AdminProductCategory
adminProductCategoryDecoder =
    Decode.map2 AdminProductCategory
        (Decode.field "id" <| Decode.map CategoryId Decode.int)
        (Decode.field "name" Decode.string)



-- Coupon Admin


type alias AdminCouponListData =
    { coupons : List AdminListCoupon }


adminCouponListDataDecoder : Decoder AdminCouponListData
adminCouponListDataDecoder =
    Decode.map AdminCouponListData
        (Decode.field "coupons" <| Decode.list adminListCouponDecoder)


type alias AdminListCoupon =
    { id : Int
    , code : String
    , name : String
    , isActive : Bool
    , discount : CouponType
    , expires : Posix
    }


adminListCouponDecoder : Decoder AdminListCoupon
adminListCouponDecoder =
    Decode.map6 AdminListCoupon
        (Decode.field "id" Decode.int)
        (Decode.field "code" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "isActive" Decode.bool)
        (Decode.field "type" couponTypeDecoder)
        (Decode.field "expires" Iso8601.decoder)


type CouponType
    = FlatDiscount Cents
    | PercentageDiscount Int
    | FreeShipping


couponTypeDecoder : Decoder CouponType
couponTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "flat" ->
                        Decode.map FlatDiscount
                            (Decode.field "amount" centsDecoder)

                    "percentage" ->
                        Decode.map PercentageDiscount
                            (Decode.field "amount" Decode.int)

                    "shipping" ->
                        Decode.succeed FreeShipping

                    _ ->
                        Decode.fail <| "couponTypeDecoder: Unknown coupon type: " ++ type_
            )


couponTypeEncoder : CouponType -> Value
couponTypeEncoder type_ =
    let
        ( typeField, amountField ) =
            case type_ of
                FreeShipping ->
                    ( "shipping", [] )

                PercentageDiscount percent ->
                    ( "percentage", [ ( "amount", Encode.int percent ) ] )

                FlatDiscount amount ->
                    ( "flat", [ ( "amount", centsEncoder amount ) ] )
    in
    Encode.object <|
        ( "type", Encode.string typeField )
            :: amountField


type alias AdminEditCouponData =
    { id : Int
    , code : String
    , name : String
    , description : String
    , isActive : Bool
    , discount : CouponType
    , minimumOrder : Cents
    , expires : Posix
    , totalUses : Int
    , customerUses : Int
    }


adminEditCouponDataDecoder : Decoder AdminEditCouponData
adminEditCouponDataDecoder =
    Decode.map8 AdminEditCouponData
        (Decode.field "id" Decode.int)
        (Decode.field "code" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "isActive" Decode.bool)
        (Decode.field "discount" couponTypeDecoder)
        (Decode.field "minimumOrder" centsDecoder)
        (Decode.field "expires" Iso8601.decoder)
        |> Decode.andThen
            (\d ->
                Decode.map2 d
                    (Decode.field "totalUses" Decode.int)
                    (Decode.field "usesPerCustomer" Decode.int)
            )



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
    }


cartItemDecoder : Decoder CartItem
cartItemDecoder =
    Decode.map4 CartItem
        (Decode.field "id" <| Decode.map CartItemId Decode.int)
        (Decode.field "product" Product.decoder)
        (Decode.field "variant" Product.variantDecoder)
        (Decode.field "quantity" Decode.int)


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
    , couponDiscount : Maybe CartCharge
    , grandTotal : Cents
    }


cartChargesDecoder : Decoder CartCharges
cartChargesDecoder =
    Decode.map6 CartCharges
        (Decode.field "tax" cartChargeDecoder)
        (Decode.field "surcharges" <| Decode.list cartChargeDecoder)
        (Decode.field "shippingMethods" <|
            Decode.map List.head <|
                Decode.list shippingChargeDecoder
        )
        (Decode.field "priorityShipping" <| Decode.nullable cartChargeDecoder)
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
