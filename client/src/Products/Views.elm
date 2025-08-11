module Products.Views exposing (..)

import BootstrapGallery as Gallery
import Category exposing (Category)
import Components.Button as Button exposing (ButtonType(..), defaultButton)
import Components.Form as Form
import Components.Svg exposing (minusSvg, plusSvg, shoppingCartSvgSmall)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (alt, attribute, class, for, href, id, selected, src, title, type_, value)
import Html.Events exposing (on, onClick, onSubmit, targetValue)
import Html.Extra exposing (viewIfLazy)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Messages exposing (Msg(..))
import Models.Fields exposing (Cents(..), blankImage, centsToString, imageToSrcSet, imgSrcFallback, lotSizeToString)
import PageData exposing (ProductData)
import Pages.Cart.Type exposing (CartForms)
import Paginate exposing (Paginated)
import Product exposing (Product, ProductId(..), ProductVariant, ProductVariantId(..), productMainImage, variantPrice)
import Products.Pagination as Pagination
import Products.Sorting as Sorting
import RemoteData
import Routing exposing (Route(..))
import SeedAttribute exposing (SeedAttribute)
import Views.Aria as Aria
import Views.Format as Format
import Views.Microdata as Microdata
import Views.Pager as Pager
import Views.Utils exposing (htmlOrBlank, icon, numericInput, onIntInput, rawHtml, routeLinkAttributes)



--------------------------------------------------------------
-- Type
--------------------------------------------------------------


type ProductFormStyle
    = Card
    | Detail
    | Checkout


type alias CartFormData =
    { maybeSelectedVariant : Maybe ProductVariant
    , maybeSelectedVariantId : Maybe ProductVariantId
    , quantity : Int
    , isOutOfStock : Bool
    , isLimitedAvailablity : Bool
    , variantSelect : List (Html Msg)
    , selectedItemNumber : String
    , offersMeta : Html Never
    , requestFeedback : Html Never
    }


cartFormData : CartForms -> ( Product, Dict Int ProductVariant ) -> CartFormData
cartFormData addToCartForms ( product, variants ) =
    let
        ( maybeSelectedVariantId, quantity, requestStatus ) =
            Dict.get (fromProductId product.id) addToCartForms
                |> Maybe.withDefault { variant = Nothing, quantity = 1, requestStatus = RemoteData.NotAsked }
                |> (\v -> ( v.variant |> ifNothing maybeFirstVariantId, v.quantity, v.requestStatus ))

        maybeSelectedVariant =
            maybeSelectedVariantId
                |> Maybe.andThen (\id -> Dict.get (fromVariantId id) variants)

        maybeFirstVariantId =
            variantList
                |> List.filter
                    (\v ->
                        if isOutOfStock then
                            True

                        else
                            v.quantity > 0
                    )
                |> List.sortBy .skuSuffix
                |> List.head
                |> Maybe.map .id

        isOutOfStock =
            Product.isOutOfStock variantList

        showVariantSelect =
            Dict.size variants > 1

        variantSelect _ =
            customSelectView product maybeSelectedVariant variantList

        -- TODO: removed this when no longer needed
        variantSelect2 _ =
            Form.selectView
                { tagId = "inputVariant-" ++ String.fromInt (fromProductId product.id)
                , ariaLabel = "Select a Lot Size Variant for " ++ product.name
                , onSelectHandler = ChangeCartFormVariantId product.id
                , valueDecoder =
                    String.toInt
                        >> Maybe.map (ProductVariantId >> Decode.succeed)
                        >> Maybe.withDefault (Decode.fail "")
                , values = variantList
                , view = variantOption
                }

        variantOption variant =
            let
                isSelected =
                    Just variant == maybeSelectedVariant

                appendPrice s =
                    if isSelected then
                        [ s ]

                    else
                        [ s, Format.cents <| variantPrice variant ]

                appendOoS l =
                    if variant.quantity <= 0 then
                        l ++ [ "OoS" ]

                    else
                        l

                -- Disable the option unless the entire product is
                -- out-of-stock.
                disabledAttr =
                    if isOutOfStock then
                        []

                    else
                        [ A.disabled <| variant.quantity <= 0 ]
            in
            Maybe.map lotSizeToString variant.lotSize
                |> Maybe.withDefault (product.baseSKU ++ variant.skuSuffix)
                |> appendPrice
                |> appendOoS
                |> String.join " - "
                |> text
                |> List.singleton
                |> option
                    ([ value <| String.fromInt <| fromVariantId variant.id
                     , selected (Just variant == maybeSelectedVariant)
                     , A.classList [ ( "out-of-stock", variant.quantity <= 0 ) ]
                     ]
                        ++ disabledAttr
                    )

        variantList =
            Dict.values variants

        fromVariantId (ProductVariantId i) =
            i

        fromProductId (ProductId i) =
            i

        ifNothing valIfNothing maybe =
            case maybe of
                Just _ ->
                    maybe

                Nothing ->
                    valIfNothing

        offers =
            div [] <|
                List.map
                    renderOfferMeta
                    variantList

        renderOfferMeta variant =
            let
                availability =
                    if variant.quantity > 0 then
                        Microdata.InStock

                    else
                        Microdata.OutOfStock
            in
            div (Microdata.offers :: Microdata.offer)
                [ Microdata.availabilityMeta availability
                , Microdata.conditionMeta Microdata.NewCondition
                , Microdata.mpnMeta (product.baseSKU ++ variant.skuSuffix)
                , Microdata.skuMeta (product.baseSKU ++ variant.skuSuffix)
                , Microdata.priceMeta (centsToString variant.price)
                , Microdata.priceCurrencyMeta "USD"
                , Microdata.descriptionMeta <|
                    Maybe.withDefault "" <|
                        Maybe.map lotSizeToString variant.lotSize
                , Microdata.urlMeta <|
                    Routing.reverse <|
                        ProductDetails product.slug Nothing
                ]

        requestFeedback =
            case requestStatus of
                RemoteData.NotAsked ->
                    text ""

                RemoteData.Loading ->
                    div [ class "tw:py-[8px] text-warning font-weight-bold small" ]
                        [ icon "spinner fa-spin mr-1"
                        , text "Adding to Cart"
                        ]

                RemoteData.Success _ ->
                    div [ class "tw:py-[8px] tw:text-[rgb(77,170,154)] font-weight-bold small" ]
                        [ icon "check-circle mr-1"
                        , text "Added to Cart!"
                        ]

                RemoteData.Failure _ ->
                    div [ class "tw:py-[8px] text-danger font-weight-bold small" ]
                        [ icon "times mr-1"
                        , text "Error Adding To Cart!"
                        ]
    in
    { maybeSelectedVariant = maybeSelectedVariant
    , maybeSelectedVariantId = maybeSelectedVariantId
    , quantity = quantity
    , isOutOfStock = Product.isOutOfStock variantList
    , isLimitedAvailablity = Product.isLimitedAvailablity variantList
    , variantSelect =
        case showVariantSelect of
            True ->
                [ viewIfLazy showVariantSelect variantSelect ]

            False ->
                []
    , selectedItemNumber = getItemNumber product maybeSelectedVariant
    , offersMeta = offers
    , requestFeedback = requestFeedback
    }



--------------------------------------------------------------
-- Views
--------------------------------------------------------------


productAttrView : String -> Maybe SeedAttribute -> Html msg
productAttrView selectedItemNumber maybeSeedAttribute =
    div [ class "tw:flex tw:items-center" ]
        [ small [ class "text-muted d-block tw:grow" ]
            [ renderItemNumber selectedItemNumber ]
        , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
        ]


outOfStockView paddingClass maybeSelectedVariant =
    maybeSelectedVariant
        |> htmlOrBlank
            (\v ->
                if v.quantity <= 0 then
                    div [ class <| "tw:flex " ++ paddingClass ] [ outOfStockBadge ]

                else
                    div [] []
            )


variantSelectView paddingClass variantSelect =
    if List.isEmpty variantSelect then
        div [] []

    else
        div [ class paddingClass ]
            variantSelect


priceView : ProductFormStyle -> Maybe ProductVariant -> Html msg
priceView style maybeSelectedVariant =
    let
        class_ =
            case style of
                Card ->
                    "tw:text-[28px] tw:font-bold"

                _ ->
                    "tw:text-[40px] tw:leading-[44px] tw:font-bold "
    in
    maybeSelectedVariant
        |> htmlOrBlank (\v -> p [ class class_ ] [ renderPrice v ])


detailFormView : CartFormData -> Product -> Maybe SeedAttribute -> List Category -> Html Msg
detailFormView { maybeSelectedVariant, maybeSelectedVariantId, quantity, isOutOfStock, variantSelect, selectedItemNumber, offersMeta, requestFeedback } product maybeSeedAttribute categories =
    let
        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <|
                case maybeSelectedVariantId of
                    Just variantId ->
                        [ onSubmit <| SubmitAddToCart product.id variantId ]

                    Nothing ->
                        []

        availabilityBadge =
            if isOutOfStock then
                addToCartInputDisabled

            else
                text ""

        categoryBlocks =
            List.filter (not << String.isEmpty << .description) categories
                |> List.map
                    (\category ->
                        div [ class "" ]
                            [ h3 [ class "tw:text-[16px]! tw:pb-[8px]" ]
                                [ a ((routeLinkAttributes <| CategoryDetails category.slug Pagination.default) ++ [ class "se-link" ])
                                    [ span [ Microdata.category ] [ text category.name ] ]
                                ]
                            , div [ class "static-page" ]
                                [ rawHtml category.description
                                ]
                            ]
                    )
    in
    form formAttributes
        [ div [ class "tw:pb-[16px]" ]
            [ productAttrView selectedItemNumber maybeSeedAttribute
            ]
        , div [ class "tw:pt-[12px] tw:pb-[24px]" ] [ priceView Detail maybeSelectedVariant ]
        , outOfStockView "tw:pb-[24px]" maybeSelectedVariant
        , variantSelectView "tw:pb-[24px]" variantSelect
        , Html.map never requestFeedback
        , div [ class "tw:pb-[28px] tw:w-full tw:flex tw:flex-col" ]
            [ viewIfLazy (not isOutOfStock) (addToCartInput Detail quantity product maybeSelectedVariant)
            , availabilityBadge
            ]
        , Html.map never offersMeta
        , div [ Microdata.description, class "tw:text-[16px] static-page" ] [ rawHtml product.longDescription ]
        , div [] categoryBlocks
        , Microdata.mpnMeta product.baseSKU
        , Microdata.skuMeta product.baseSKU
        , Microdata.brandMeta "Southern Exposure Seed Exchange"
        , Microdata.urlMeta <|
            Routing.reverse <|
                ProductDetails product.slug Nothing
        ]


detailView : CartForms -> PageData.ProductDetails -> List (Html Msg)
detailView addToCartForms { product, variants, maybeSeedAttribute, categories } =
    let
        cartData =
            cartFormData addToCartForms ( product, variants )
    in
    [ div (Microdata.product ++ [ class "tw:flex tw:gap-[40px]" ])
        [ div [ class "tw:shrink-0" ]
            [ productImageGalleryView product
            ]
        , div []
            [ h6 [ class "tw:text-[32px]! tw:pb-[16px]" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "" ])
                    [ Product.singleVariantName product variants ]
                ]
            , detailFormView cartData product maybeSeedAttribute categories
            ]
        ]
    ]


cardFormView : CartFormData -> Product -> Maybe SeedAttribute -> Html Msg
cardFormView { maybeSelectedVariant, maybeSelectedVariantId, quantity, isOutOfStock, variantSelect, selectedItemNumber, offersMeta, requestFeedback } product maybeSeedAttribute =
    let
        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <|
                case maybeSelectedVariantId of
                    Just variantId ->
                        [ onSubmit <| SubmitAddToCart product.id variantId ]

                    Nothing ->
                        []

        availabilityBadge =
            if isOutOfStock then
                addToCartInputDisabled

            else
                text ""
    in
    form formAttributes
        [ div [ class "tw:pt-[12px] " ] [ productAttrView selectedItemNumber maybeSeedAttribute ]
        , outOfStockView "tw:pt-[12px]" maybeSelectedVariant
        , variantSelectView "tw:pt-[12px]" variantSelect
        , div [ Microdata.description, class "tw:pt-[16px] tw:line-clamp-4 tw:text-[14px] static-page" ] [ rawHtml product.longDescription ]
        , Microdata.mpnMeta product.baseSKU
        , Microdata.skuMeta product.baseSKU
        , Microdata.brandMeta "Southern Exposure Seed Exchange"
        , div [ class "tw:grow" ]
            []
        , div [ class "tw:pt-[24px]" ]
            [ Html.map never requestFeedback
            , div [ class "tw:flex tw:items-center" ]
                [ priceView Card maybeSelectedVariant
                , div [ class "tw:grow" ] []
                , viewIfLazy (not isOutOfStock) (addToCartInput Card quantity product maybeSelectedVariant)
                , availabilityBadge
                , Html.map never offersMeta
                ]
            ]
        ]


cardView : CartFormData -> ProductData -> Html Msg
cardView cartData ( product, variants, maybeSeedAttribute ) =
    div (Microdata.product ++ [ class "tw:bg-[rgba(77,170,154,0.06)] tw:rounded-[16px] tw:overflow-hidden tw:p-[16px] tw:flex tw:flex-col" ])
        [ productImageLinkView "tw:w-full tw:h-[200px]" product Nothing
        , div [ class "tw:pt-[16px]" ]
            [ h6 [ class "mb-0 d-flex justify-content-between" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "tw:line-clamp-2 tw:h-[39px]" ])
                    [ Product.singleVariantName product variants ]
                ]
            ]
        , cardFormView cartData product maybeSeedAttribute
        ]



-- | Product image view, clicking it will redirect to the product detail


productImageLinkView : String -> Product -> Maybe ProductVariantId-> Html msg
productImageLinkView sizeClass product maybeVariantId =
    let
        productImage =
            productMainImage product
    in
    Keyed.node "a"
        (Aria.label ("View Details for " ++ product.name)
            :: routeLinkAttributes (ProductDetails product.slug maybeVariantId)
        )
        [ ( "product-img-" ++ (String.fromInt <| (\(ProductId i) -> i) product.id)
          , img
                [ src <| imgSrcFallback productImage
                , imageToSrcSet productImage

                -- , listImageSizes
                , Microdata.image
                , alt <| "Product Image for " ++ product.name
                , class <| sizeClass ++ " tw:object-cover tw:rounded-[16px] clickable-image"
                ]
                []
          )
        ]



-- | Product image view, clicking it will open the gallery view


productImageGalleryView : Product -> Html Msg
productImageGalleryView product =
    let
        productImage =
            productMainImage product
    in
    a
        [ href <| productImage.original
        , A.target "_self"
        , Gallery.openOnClick ProductDetailsLightbox productImage
        , Aria.label <| "View Product Image for " ++ product.name
        ]
        [ img
            [ src <| imgSrcFallback productImage
            , imageToSrcSet productImage
            , Microdata.image
            , alt <| "Product Image for " ++ product.name
            , class "tw:w-[360px] tw:h-[360px] tw:object-cover tw:rounded-[16px] clickable-image"
            ]
            []
        ]


listView : (Pagination.Data -> Route) -> Pagination.Data -> CartForms -> Paginated ProductData a c -> List (Html Msg)
listView routeConstructor pagination addToCartForms products =
    let
        sortHtml =
            if productsCount > 1 then
                div [ class "tw:flex tw:justify-between tw:pb-[20px] tw:px-[20px] tw:items-center" ]
                    [ sortingInput
                    , pager.perPageLinks ()
                    ]

            else
                text ""

        productsCount =
            Paginate.getTotalItems products

        sortingInput : Html Msg
        sortingInput =
            div [ class "form-inline products-sorting" ]
                [ select
                    [ id "product-sort-select"
                    , class "form-control form-control-sm"
                    , onProductsSortSelect (NavigateTo << routeConstructor)
                    , Aria.label "Select Products Sorting Method"
                    ]
                  <|
                    List.map
                        (\data ->
                            option
                                [ value <| Sorting.toQueryValue data
                                , selected (data == pagination.sorting)
                                ]
                                [ text <| Sorting.toDescription data ]
                        )
                        Sorting.all
                ]

        onProductsSortSelect : (Pagination.Data -> msg) -> Html.Attribute msg
        onProductsSortSelect msg =
            targetValue
                |> Decode.map
                    (\str ->
                        msg <|
                            { pagination | sorting = Sorting.fromQueryValue str }
                    )
                |> on "change"

        pager =
            Pager.elements
                { itemDescription = "Products"
                , pagerAriaLabel = "Category Product Pages"
                , pagerCssClass = ""
                , pageSizes = [ 10, 25, 50, 75, 100 ]
                , routeConstructor =
                    \{ page, perPage } ->
                        routeConstructor { pagination | page = page, perPage = perPage }
                }
                products

        productRows =
            List.map
                (\(( p, v, _ ) as productData) ->
                    let
                        cartData =
                            cartFormData addToCartForms ( p, v )
                    in
                    cardView cartData productData
                )
                (Paginate.getCurrent products)
                |> List.foldr (\r rs -> r :: rs) []
    in
    if productsCount /= 0 then
        [ sortHtml

        -- , pager.viewTop ()
        , div [ class "tw:grid tw:grid-cols-1  tw:gap-[24px] tw:lg:grid-cols-3 " ]
            productRows
        , pager.viewBottom ()
        , SeedAttribute.legend
        ]

    else
        []


listImageSizes : Html.Attribute msg
listImageSizes =
    attribute "sizes" <|
        String.join ", "
            [ "(max-width: 575px) 100vw"
            , "(max-width: 767px) 175px"
            , "100px"
            ]


renderPrice : ProductVariant -> Html msg
renderPrice variant =
    if variantPrice variant == Cents 0 then
        text "Free!"

    else
        case variant.salePrice of
            Just salePrice ->
                div [ class "sale-price" ]
                    [ Html.del [] [ text <| Format.cents variant.price ]
                    , div [ class "text-danger" ] [ text <| Format.cents salePrice ]
                    ]

            Nothing ->
                text <| Format.cents variant.price


getItemNumber : Product -> Maybe ProductVariant -> String
getItemNumber product maybeSelectedVariant =
    case maybeSelectedVariant of
        Nothing ->
            product.baseSKU

        Just v ->
            product.baseSKU ++ v.skuSuffix


renderItemNumber : String -> Html msg
renderItemNumber selectedItemNumber =
    p
        [ class "tw:text-[rgba(30,12,3,0.7)] tw:text-[14px] tw:leading-[20px]"
        ]
        [ text <| "Item #" ++ selectedItemNumber ]


outOfStockBadge : Html msg
outOfStockBadge =
    div [ class "tw:text-[rgba(214,34,70,1)] tw:pt-[4px] tw:pb-[4px] tw:px-[12px] tw:rounded-[8px] tw:bg-[rgba(214,34,70,0.1)] tw:border tw:border-[rgba(214,34,70,0.4)]" ]
        [ span [ class "tw:block tw:font-semibold tw:text-[14px] tw:leading-[20px]" ] [ text "Out of stock" ]
        ]


addToCartInputDisabled : Html msg
addToCartInputDisabled =
    Button.view
        { defaultButton
            | label = "Add to cart"
            , type_ = Button.Disabled
            , icon = Just <| shoppingCartSvgSmall
            , size = Button.Large
        }


limitedAvailabilityBadge : Html msg
limitedAvailabilityBadge =
    span [ class "badge badge-warning" ] [ text "Limited Availability" ]


addToCartInput : ProductFormStyle -> Int -> Product -> Maybe ProductVariant -> () -> Html Msg
addToCartInput style quantity product maybeSelectedVariant _ =
    let
        ( class_, buttonView ) =
            case style of
                Card ->
                    ( "tw:gap-[4px]"
                    , div [ class "tw:flex" ]
                        [ Button.view { defaultButton | type_ = Button.FormSubmit, icon = Just <| shoppingCartSvgSmall }
                        ]
                    )

                _ ->
                    ( "tw:gap-[20px]"
                    , div [ class "tw:w-full" ]
                        [ Button.view
                            { defaultButton
                                | label = "Add to cart"
                                , type_ = Button.FormSubmit
                                , icon = Just <| shoppingCartSvgSmall
                                , padding = Button.Expand
                                , size = Button.Large
                            }
                        ]
                    )

        view =
            div [ class <| "tw:flex " ++ class_ ]
                [ customNumberView style
                    quantity
                    (ChangeCartFormQuantity product.id)
                    (IncreaseCartFormQuantity product.id)
                    (DecreaseCartFormQuantity product.id)
                , buttonView
                ]
    in
    case maybeSelectedVariant of
        Just selectedVariant ->
            if selectedVariant.quantity <= 0 then
                addToCartInputDisabled

            else
                view

        Nothing ->
            view


customNumberView : ProductFormStyle -> Int -> (Int -> msg) -> msg -> msg -> Html msg
customNumberView style currentVal onChangeHandler increaseHandler decreaseHandler =
    let
        { class_, fillClass, buttonSizeClass, formClass } =
            case style of
                Card ->
                    { class_ = "tw:bg-[rgba(29,127,110,1)] tw:text-white"
                    , fillClass = "tw:fill-white"
                    , buttonSizeClass = "tw:h-[40px] tw:w-[30px] tw:hover:bg-[rgb(17,75,65)]"
                    , formClass = "tw:w-[30px] text-center tw:border-white"
                    }

                Detail ->
                    { class_ = "tw:bg-white tw:border tw:border-[rgba(29,127,110,1)]"
                    , fillClass = "tw:fill-black"
                    , buttonSizeClass = "tw:w-[48px] tw:h-[48px] tw:hover:bg-[rgb(219,219,219)]"
                    , formClass = "tw:w-[46px] tw:text-center tw:border-[rgba(29,127,110,1)]"
                    }

                Checkout ->
                    { class_ = "tw:bg-white tw:border tw:border-[rgba(29,127,110,1)]"
                    , fillClass = "tw:fill-black"
                    , buttonSizeClass = "tw:w-[48px] tw:h-[40px] tw:hover:bg-[rgb(219,219,219)]"
                    , formClass = "tw:w-[46px] tw:text-center tw:border-[rgba(29,127,110,1)]"
                    }
    in
    div [ class <| class_ ++ " tw:flex tw:shrink-0 tw:rounded-[8px] tw:overflow-hidden" ]
        [ button
            [ type_ "button"
            , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
            , onClick decreaseHandler
            ]
            [ minusSvg fillClass
            ]
        , div [ class "tw:flex tw:items-center tw:justify-center tw:px-[4px]" ]
            [ Form.numberView
                (formClass ++ " tw:block no-arrow tw:py-[2px] tw:pl-[6px] tw:hover:border tw:focus:border tw:rounded-[4px]")
                currentVal
                onChangeHandler
            ]
        , button
            [ type_ "button"
            , class <| buttonSizeClass ++ " tw:cursor-pointer tw:flex tw:items-center tw:justify-center"
            , onClick increaseHandler
            ]
            [ plusSvg fillClass
            ]
        ]


customSelectView : Product -> Maybe ProductVariant -> List ProductVariant -> Html Msg
customSelectView product maybeSelectedVariant variantList =
    let
        getLotSizeLabel variant =
            Maybe.map lotSizeToString variant.lotSize
                |> Maybe.withDefault (product.baseSKU ++ variant.skuSuffix)

        singleItemView : ProductVariant -> Html Msg
        singleItemView variant =
            let
                selectedColor =
                    "tw:text-[rgba(77,170,154,1)] tw:border-[rgba(77,170,154,0.4)] tw:bg-[rgba(77,170,154,0.1)]"

                baseColor =
                    "tw:text-[rgba(30,12,3,0.6)] tw:border-[rgba(30,12,3,0.1)] tw:bg-[rgba(30,12,3,0.04)]"

                colorClass =
                    case maybeSelectedVariant of
                        Just selected ->
                            if selected.id == variant.id then
                                selectedColor

                            else
                                baseColor

                        Nothing ->
                            baseColor
            in
            button
                [ type_ "button"
                , class <| colorClass ++ " tw:py-[4px] tw:px-[12px] tw:border tw:rounded-[8px]! tw:cursor-pointer"
                , onClick <| ChangeCartFormVariantId product.id variant.id
                ]
                [ span [ class " tw:block tw:text-[14px] tw:leading-[20px] tw:font-semibold" ]
                    [ text <| getLotSizeLabel variant ]
                ]
    in
    div [ class "tw:flex tw:gap-[8px] tw:flex-wrap" ] <|
        List.map
            singleItemView
            variantList
