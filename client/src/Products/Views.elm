module Products.Views exposing (detailView, listView)

import BootstrapGallery as Gallery
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
import Model exposing (CartForms)
import Models.Fields exposing (Cents(..), blankImage, centsToString, imageToSrcSet, imgSrcFallback, lotSizeToString)
import PageData exposing (ProductData)
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

        selectedItemNumber =
            case maybeSelectedVariant of
                Nothing ->
                    product.baseSKU

                Just v ->
                    product.baseSKU ++ v.skuSuffix

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
                    div [ class "text-warning font-weight-bold small" ]
                        [ icon "spinner fa-spin mr-1"
                        , text "Adding to Cart"
                        ]

                RemoteData.Success _ ->
                    div [ class "tw:text-green-400 font-weight-bold small" ]
                        [ icon "check-circle mr-1"
                        , text "Added to Cart!"
                        ]

                RemoteData.Failure _ ->
                    div [ class "text-danger font-weight-bold small" ]
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
    , selectedItemNumber = selectedItemNumber
    , offersMeta = offers
    , requestFeedback = requestFeedback
    }



--------------------------------------------------------------
-- Views
--------------------------------------------------------------


detailView : CartForms -> PageData.ProductDetails -> List (Html Msg)
detailView addToCartForms { product, variants, maybeSeedAttribute, categories } =
    let
        productImage =
            productMainImage product

        categoryBlocks =
            List.filter (not << String.isEmpty << .description) categories
                |> List.map
                    (\category ->
                        div [ class "product-category static-page" ]
                            [ h3 [ class "mt-3" ]
                                [ a (routeLinkAttributes <| CategoryDetails category.slug Pagination.default)
                                    [ span [ Microdata.category ] [ text category.name ] ]
                                ]
                            , rawHtml category.description
                            ]
                    )
    in
    List.singleton <|
        div Microdata.product
            [ h1 [ class "product-details-title d-flex justify-content-between" ]
                [ Product.singleVariantName product variants
                , div [ class "d-none d-md-inline-flex" ]
                    [ htmlOrBlank SeedAttribute.icons maybeSeedAttribute ]
                ]
            , div [ class "d-md-none" ]
                [ htmlOrBlank SeedAttribute.icons maybeSeedAttribute ]
            , hr [] []
            , div [ class "product-details" ]
                [ div [ class "clearfix" ]
                    [ div [ class "product-image mr-md-3 mb-2" ]
                        [ div
                            [ class "card" ]
                            [ div [ class "card-body text-center p-1" ]
                                [ a
                                    [ href <| productImage.original
                                    , A.target "_self"
                                    , Gallery.openOnClick ProductDetailsLightbox productImage
                                    , Aria.label <| "View Product Image for " ++ product.name
                                    ]
                                    [ img
                                        [ src <| imgSrcFallback productImage
                                        , imageToSrcSet productImage
                                        , class "img-fluid mb-2"
                                        , alt <| "Product Image for " ++ product.name
                                        , Microdata.image
                                        , attribute "sizes" <|
                                            String.join ", "
                                                [ "(max-width: 767px) 100vw"
                                                , "(max-width: 991px) 125px"
                                                , "(max-width: 1199px) 230px"
                                                , "315px"
                                                ]
                                        ]
                                        []
                                    ]
                                , cardFormView (cartFormData addToCartForms ( product, variants )) product maybeSeedAttribute
                                ]
                            ]
                        ]
                    , Microdata.mpnMeta product.baseSKU
                    , Microdata.skuMeta product.baseSKU
                    , Microdata.brandMeta "Southern Exposure Seed Exchange"
                    , Microdata.urlMeta <|
                        Routing.reverse <|
                            ProductDetails product.slug Nothing
                    , div [ Microdata.description, class "static-page" ] [ rawHtml product.longDescription ]
                    , div [] categoryBlocks
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

        selectedPrice =
            maybeSelectedVariant
                |> htmlOrBlank (\v -> p [ class "tw:text-[28px] tw:font-bold" ] [ renderPrice v ])

        availabilityBadge =
            if isOutOfStock then
                addToCartInputDisabled

            else
                text ""
    in
    form formAttributes
        [ div [ class "tw:flex tw:pt-[12px] tw:items-center" ]
            [ small [ class "text-muted d-block tw:grow" ]
                [ text <| "Item #" ++ selectedItemNumber ]
            , htmlOrBlank SeedAttribute.icons maybeSeedAttribute
            ]
        , if List.isEmpty variantSelect then
            if isOutOfStock then
                div [ class "tw:flex tw:pt-[12px]" ] [ outOfStockBadge ]

            else
                div [] []

          else
            div [ class "tw:pt-[12px]" ]
                variantSelect
        , div [ Microdata.description, class "tw:pt-[16px] tw:line-clamp-4 tw:text-[14px] static-page" ] [ rawHtml product.longDescription ]
        , Microdata.mpnMeta product.baseSKU
        , Microdata.skuMeta product.baseSKU
        , Microdata.brandMeta "Southern Exposure Seed Exchange"
        , div [ class "tw:grow" ]
            []
        , div [ class "tw:pt-[24px]" ]
            [ Html.map never requestFeedback
            , div [ class "tw:flex tw:items-center" ]
                [ selectedPrice
                , div [ class "tw:grow" ] []
                , viewIfLazy (not isOutOfStock) (addToCartInput quantity product)
                , availabilityBadge
                , Html.map never offersMeta
                ]
            ]
        ]


cardView : CartFormData -> ProductData -> Html Msg
cardView cartData ( product, variants, maybeSeedAttribute ) =
    let
        productImage =
            productMainImage product
    in
    div (Microdata.product ++ [ class "tw:bg-[rgba(77,170,154,0.06)] tw:rounded-[16px] tw:overflow-hidden tw:p-[16px] tw:flex tw:flex-col" ])
        [ Keyed.node "a"
            (Aria.label ("View Details for " ++ product.name)
                :: routeLinkAttributes (ProductDetails product.slug Nothing)
            )
            [ ( "product-img-" ++ (String.fromInt <| (\(ProductId i) -> i) product.id)
              , img
                    [ src <| imgSrcFallback productImage
                    , imageToSrcSet productImage

                    -- , listImageSizes
                    , Microdata.image
                    , alt <| "Product Image for " ++ product.name
                    , class "tw:w-full tw:h-[200px] tw:object-cover tw:rounded-[16px] clickable-image"
                    ]
                    []
              )
            ]
        , div [ class "tw:pt-[16px]" ]
            [ h6 [ class "mb-0 d-flex justify-content-between" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "tw:line-clamp-2 tw:h-[39px]" ])
                    [ Product.singleVariantName product variants ]
                ]
            ]
        , cardFormView cartData product maybeSeedAttribute
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


outOfStockBadge : Html msg
outOfStockBadge =
    div [ class "tw:text-[rgba(214,34,70,1)] tw:pt-[4px] tw:pb-[4px] tw:px-[12px] tw:rounded-[8px] tw:bg-[rgba(214,34,70,0.1)] tw:border tw:border-[rgba(214,34,70,0.4)]" ]
        [ span [ class "tw:block tw:font-semibold tw:text-[14px] tw:leading-[20px]" ] [ text "Out of stock" ]
        ]


addToCartInputDisabled : Html msg
addToCartInputDisabled =
    Button.view { defaultButton | label = "Add to cart", type_ = Button.Disabled, icon = Just <| shoppingCartSvgSmall }


limitedAvailabilityBadge : Html msg
limitedAvailabilityBadge =
    span [ class "badge badge-warning" ] [ text "Limited Availability" ]


addToCartInput quantity product _ =
    div [ class "tw:flex" ]
        [ customNumberView quantity
            (ChangeCartFormQuantity product.id)
            (IncreaseCartFormQuantity product.id)
            (DecreaseCartFormQuantity product.id)
        , div [ class "tw:pl-[2px] tw:flex" ]
            [ Button.view { defaultButton | type_ = Button.FormSubmit, icon = Just <| shoppingCartSvgSmall }
            ]
        ]


customNumberView : Int -> (Int -> msg) -> msg -> msg -> Html msg
customNumberView currentVal onChangeHandler increaseHandler decreaseHandler =
    div [ class "tw:flex tw:shrink-0 tw:text-white tw:rounded-[8px] tw:bg-[rgba(29,127,110,1)] tw:overflow-hidden" ]
        [ button
            [ type_ "button"
            , class "tw:cursor-pointer tw:h-[40px] tw:w-[30px] tw:flex tw:items-center tw:justify-center tw:hover:bg-[rgb(17,75,65)]"
            , onClick decreaseHandler
            ]
            [ minusSvg
            ]
        , div [ class "tw:flex tw:items-center tw:justify-center tw:px-[4px]" ]
            [ Form.numberView "tw:block tw:w-[30px] no-arrow tw:py-[2px] tw:pl-[6px] tw:hover:border tw:focus:border tw:rounded-[4px]" currentVal onChangeHandler
            ]
        , button
            [ type_ "button"
            , class "tw:cursor-pointer tw:h-[40px] tw:w-[30px] tw:flex tw:items-center tw:justify-center tw:hover:bg-[rgb(17,75,65)]"
            , onClick increaseHandler
            ]
            [ plusSvg
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
