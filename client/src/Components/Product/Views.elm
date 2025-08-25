module Components.Product.Views exposing (..)

import BootstrapGallery as Gallery
import Components.Aria as Aria
import Components.Button as Button exposing (ButtonType(..), defaultButton)
import Components.Form as Form
import Components.Microdata as Microdata
import Components.Pager as Pager
import Components.Pagination as Pagination
import Components.Product.Type exposing (Model, Msg(..), initProductModel)
import Components.SeedAttribute as SeedAttribute
import Components.Sorting as Sorting
import Components.Svg exposing (minusSvg, plusSvg, shoppingCartSvgSmall)
import Components.Tooltip as Tooltip
import Data.Category exposing (Category)
import Data.Fields exposing (Cents(..), centsToString, imageToSrcSet, imgSrcFallback, lotSizeToString)
import Data.PageData as PageData exposing (ProductData)
import Data.Product as Product exposing (InventoryPolicy(..), Product, ProductId(..), ProductVariant, ProductVariantId(..), productMainImage, variantPrice)
import Data.Routing.Routing as Routing exposing (Route(..))
import Data.SeedAttribute as SeedAttribute exposing (SeedAttribute)
import Data.Shared exposing (Shared)
import Data.ViewKey exposing (ViewKey)
import Dict exposing (Dict, get)
import Html exposing (..)
import Html.Attributes as A exposing (alt, attribute, class, href, id, selected, src, type_, value)
import Html.Events exposing (on, onClick, onSubmit, targetValue)
import Html.Extra exposing (viewIfLazy)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Paginate exposing (Paginated)
import RemoteData
import Utils.Format as Format
import Utils.View exposing (htmlOrBlank, icon, numericInput, onIntInput, rawHtml, routeLinkAttributes)



--------------------------------------------------------------
-- Type
--------------------------------------------------------------


type ProductFormStyle
    = Card
    | Detail
    | Checkout


type alias CartFormData pmsg =
    { maybeSelectedVariant : Maybe ProductVariant
    , maybeSelectedVariantId : Maybe ProductVariantId
    , quantity : Int
    , variantSelect : List (Html pmsg)
    , selectedItemNumber : String
    , offersMeta : Html Never
    , requestFeedback : Html Never
    }


{-| Given the product dict, get necessary data to be passed into view function
-}
cartFormData : Shared pmsg -> Dict Int Model -> ( Product, Dict Int ProductVariant ) -> CartFormData pmsg
cartFormData shared productDict ( product, variants ) =
    let
        ( maybeSelectedVariantId, quantity, requestStatus ) =
            Dict.get (fromProductId product.id) productDict
                |> Maybe.withDefault initProductModel
                |> (\v -> ( v.variant |> ifNothing maybeFirstVariantId, v.quantity, v.requestStatus ))

        maybeSelectedVariant =
            maybeSelectedVariantId
                |> Maybe.andThen (\id -> Dict.get (fromVariantId id) variants)

        maybeFirstVariantId =
            variantList
                |> (\vs ->
                        let
                            purchasableVs =
                                List.filter
                                    (\v ->
                                        Product.isPurchaseable v
                                    )
                                    vs
                        in
                        if List.length purchasableVs > 0 then
                            purchasableVs

                        else
                            vs
                   )
                |> List.sortBy .skuSuffix
                |> List.head
                |> Maybe.map .id

        showVariantSelect =
            Dict.size variants > 1

        variantSelect _ =
            customSelectView shared product maybeSelectedVariant variantList

        -- TODO: removed this when no longer needed
        variantSelect2 _ =
            Form.selectView
                { tagId = "inputVariant-" ++ String.fromInt (fromProductId product.id)
                , ariaLabel = "Select a Lot Size Variant for " ++ product.name
                , onSelectHandler = shared.productMsg product.id << ChangeCartFormVariantId
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
            in
            Maybe.map lotSizeToString variant.lotSize
                |> Maybe.withDefault (product.baseSKU ++ variant.skuSuffix)
                |> appendPrice
                |> String.join " - "
                |> text
                |> List.singleton
                |> option
                    [ value <| String.fromInt <| fromVariantId variant.id
                    , selected (Just variant == maybeSelectedVariant)
                    , A.classList [ ( "out-of-stock", variant.quantity <= 0 ) ]
                    ]

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

                RemoteData.Success (Ok _) ->
                    div [ class "tw:py-[8px] tw:text-[rgb(77,170,154)] font-weight-bold small" ]
                        [ icon "check-circle mr-1"
                        , text "Added to Cart!"
                        ]

                RemoteData.Success (Err errors) ->
                    div [ class "tw:py-[8px] text-danger font-weight-bold small" ]
                        [ icon "times mr-1"
                        , text "Error Adding To Cart: "
                        , br [] []
                        , case get "variant" errors of
                            Just variantValidation ->
                                text <| " " ++ String.join "\n" variantValidation

                            Nothing ->
                                text ""
                        , case get "quantity" errors of
                            Just quantityValidation ->
                                text <| " " ++ String.join "\n" quantityValidation

                            Nothing ->
                                text ""
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


productAttrView : Shared pmsg -> ViewKey -> String -> Maybe SeedAttribute -> Html pmsg
productAttrView shared parentKey selectedItemNumber maybeSeedAttribute =
    div [ class "tw:flex tw:items-center" ]
        [ small [ class "text-muted d-block tw:grow" ]
            [ renderItemNumber selectedItemNumber ]
        , htmlOrBlank (SeedAttribute.icons shared parentKey shared.tooltipMsg) maybeSeedAttribute
        ]


outOfStockView : String -> Maybe ProductVariant -> Html msg
outOfStockView paddingClass maybeSelectedVariant =
    maybeSelectedVariant
        |> htmlOrBlank
            (\v ->
                if not (Product.isPurchaseable v) then
                    div [ class <| "tw:flex " ++ paddingClass ] [ outOfStockBadge ]

                else
                    div [] []
            )


limitedAvailabilityView : Shared pmsg -> ViewKey -> String -> Maybe ProductVariant -> Html pmsg
limitedAvailabilityView shared parentKey paddingClass maybeSelectedVariant =
    maybeSelectedVariant
        |> htmlOrBlank
            (\v ->
                if Product.isLimitedAvailability v then
                    div [ class <| "tw:flex " ++ paddingClass ]
                        [ Tooltip.view
                            { key = parentKey ++ "limitedAvailabilityBadge"
                            , triggerEl = limitedAvailabilityBadge
                            , text = "This product has limited availability. Expect delays in delivery."
                            , mkParentMsg = shared.tooltipMsg
                            , widthClass = Just "tw:w-[264px]"
                            , align = Tooltip.Right
                            }
                            shared.tooltips
                        ]

                else
                    div [] []
            )


variantSelectView : String -> List (Html msg) -> Html msg
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


detailFormView : Shared pmsg -> CartFormData pmsg -> Product -> Maybe SeedAttribute -> List Category -> Html pmsg
detailFormView shared { maybeSelectedVariant, maybeSelectedVariantId, quantity, variantSelect, selectedItemNumber, offersMeta, requestFeedback } product maybeSeedAttribute categories =
    let
        key =
            "product-detail"

        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <|
                case maybeSelectedVariantId of
                    Just variantId ->
                        [ onSubmit <| shared.productMsg product.id <| SubmitAddToCart variantId ]

                    Nothing ->
                        []

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
            [ productAttrView shared key selectedItemNumber maybeSeedAttribute
            ]
        , div [ class "tw:pt-[12px] tw:pb-[24px]" ] [ priceView Detail maybeSelectedVariant ]
        , outOfStockView "tw:pb-[24px]" maybeSelectedVariant
        , limitedAvailabilityView shared key "tw:pb-[24px]" maybeSelectedVariant
        , variantSelectView "tw:pb-[24px]" variantSelect
        , Html.map never requestFeedback
        , div [ class "tw:pb-[28px] tw:w-full tw:flex tw:flex-col" ]
            [ addToCartInput shared Detail quantity product maybeSelectedVariant
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


detailView : Shared pmsg -> Dict Int Model -> PageData.ProductDetails -> List (Html pmsg)
detailView shared productDict { product, variants, maybeSeedAttribute, categories } =
    let
        cartData =
            cartFormData shared productDict ( product, variants )
    in
    [ div (Microdata.product ++ [ class "tw:flex tw:gap-[40px] tw:flex-col tw:lg:flex-row tw:items-center tw:lg:items-start" ])
        [ div [ class "tw:shrink-0 tw:flex" ]
            [ productImageGalleryView shared product
            ]
        , div []
            [ h6 [ class "tw:text-[32px]! tw:pb-[16px]" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "" ])
                    [ Product.singleVariantName product variants ]
                ]
            , detailFormView shared cartData product maybeSeedAttribute categories
            ]
        ]
    ]


cardFormView : Shared pmsg -> ViewKey -> CartFormData pmsg -> Product -> Maybe SeedAttribute -> Html pmsg
cardFormView shared key { maybeSelectedVariant, maybeSelectedVariantId, quantity, variantSelect, selectedItemNumber, offersMeta, requestFeedback } product maybeSeedAttribute =
    let
        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <|
                case maybeSelectedVariantId of
                    Just variantId ->
                        [ onSubmit <| shared.productMsg product.id <| SubmitAddToCart variantId ]

                    Nothing ->
                        []
    in
    form formAttributes
        [ div [ class "tw:pt-[12px] " ] [ productAttrView shared key selectedItemNumber maybeSeedAttribute ]
        , outOfStockView "tw:pt-[12px]" maybeSelectedVariant
        , limitedAvailabilityView shared key "tw:pt-[12px]" maybeSelectedVariant
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
                , addToCartInput shared Card quantity product maybeSelectedVariant
                , Html.map never offersMeta
                ]
            ]
        ]


cardView : Shared pmsg -> ViewKey -> CartFormData pmsg -> ProductData -> Html pmsg
cardView shared key cartData ( product, variants, maybeSeedAttribute ) =
    div (Microdata.product ++ [ class "tw:bg-[rgba(77,170,154,0.06)] tw:rounded-[16px] tw:p-[16px] tw:flex tw:flex-col" ])
        [ productImageLinkView "tw:w-full tw:h-[200px]" product Nothing
        , div [ class "tw:pt-[16px]" ]
            [ h6 [ class "mb-0 d-flex justify-content-between" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "tw:line-clamp-2 tw:h-[39px]" ])
                    [ Product.singleVariantName product variants ]
                ]
            ]
        , cardFormView shared key cartData product maybeSeedAttribute
        ]



-- | Product image view, clicking it will redirect to the product detail


productImageLinkView : String -> Product -> Maybe ProductVariantId -> Html msg
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
                , class <| sizeClass ++ " tw:object-cover tw:rounded-[8px] tw:lg:rounded-[16px] clickable-image"
                ]
                []
          )
        ]



-- | Product image view, clicking it will open the gallery view


productImageGalleryView : Shared pmsg -> Product -> Html pmsg
productImageGalleryView shared product =
    let
        productImage =
            productMainImage product
    in
    a
        [ href <| productImage.original
        , A.target "_self"
        , Gallery.openOnClick shared.lightboxMsg productImage
        , Aria.label <| "View Product Image for " ++ product.name
        ]
        [ img
            [ src <| imgSrcFallback productImage
            , imageToSrcSet productImage
            , Microdata.image
            , alt <| "Product Image for " ++ product.name
            , class "tw:w-[360px] tw:aspect-square tw:object-cover tw:rounded-[16px] clickable-image"
            ]
            []
        ]


listView : Shared pmsg -> (Pagination.Data -> Route) -> Pagination.Data -> Dict Int Model -> Paginated ProductData a c -> List (Html pmsg)
listView shared routeConstructor pagination productDict products =
    let
        sortHtml =
            if productsCount > 1 then
                div [ class "tw:flex tw:justify-between tw:pb-[20px] tw:px-0 tw:lg:px-[20px] tw:items-center" ]
                    [ sortingInput
                    , pager.perPageLinks ()
                    ]

            else
                text ""

        productsCount =
            Paginate.getTotalItems products

        sortingInput : Html pmsg
        sortingInput =
            div [ class "form-inline products-sorting" ]
                [ select
                    [ id "product-sort-select"
                    , class "form-control form-control-sm"
                    , onProductsSortSelect (shared.navigateToMsg << routeConstructor)
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
            List.indexedMap
                (\i (( p, v, _ ) as productData) ->
                    let
                        cartData =
                            cartFormData shared productDict ( p, v )
                    in
                    cardView shared ("card" ++ String.fromInt i) cartData productData
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

        -- , SeedAttribute.legend
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


limitedAvailabilityBadge : Html pmsg
limitedAvailabilityBadge =
    div [ class "tw:text-[rgba(250,173,20,1)] tw:pt-[4px] tw:pb-[4px] tw:px-[12px] tw:rounded-[8px] tw:bg-[rgba(250,173,20,0.1)] tw:border tw:border-[rgba(250,173,20,0.4)]" ]
        [ span [ class "tw:block tw:font-semibold tw:text-[14px] tw:leading-[20px]" ] [ text "Limited Stock — expect delays" ]
        ]


addToCartInput : Shared pmsg -> ProductFormStyle -> Int -> Product -> Maybe ProductVariant -> Html pmsg
addToCartInput shared style quantity product maybeSelectedVariant =
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
            div []
                [ div [ class <| "tw:flex " ++ class_ ]
                    [ customNumberView style
                        quantity
                        (shared.productMsg product.id << ChangeCartFormQuantity)
                        (shared.productMsg product.id IncreaseCartFormQuantity)
                        (shared.productMsg product.id DecreaseCartFormQuantity)
                    , buttonView
                    ]

                -- , AddToCart.view () AddToCart.init
                ]
    in
    case maybeSelectedVariant of
        Just selectedVariant ->
            if not (Product.isPurchaseable selectedVariant) then
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


customSelectView : Shared pmsg -> Product -> Maybe ProductVariant -> List ProductVariant -> Html pmsg
customSelectView shared product maybeSelectedVariant variantList =
    let
        getLotSizeLabel variant =
            Maybe.map lotSizeToString variant.lotSize
                |> Maybe.withDefault (product.baseSKU ++ variant.skuSuffix)

        singleItemView : ProductVariant -> Html pmsg
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
                , onClick <| shared.productMsg product.id <| ChangeCartFormVariantId variant.id
                ]
                [ span [ class " tw:block tw:text-[14px] tw:leading-[20px] tw:font-semibold" ]
                    [ text <| getLotSizeLabel variant ]
                ]
    in
    div [ class "tw:flex tw:gap-[8px] tw:flex-wrap" ] <|
        List.map
            singleItemView
            variantList
