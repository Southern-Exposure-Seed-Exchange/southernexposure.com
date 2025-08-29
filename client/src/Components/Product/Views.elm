module Components.Product.Views exposing (..)

import BootstrapGallery as Gallery
import Components.AddToCart.AddToCart as AddToCart
import Components.AddToCart.Type as AddToCart
import Components.Aria as Aria
import Components.Button as Button exposing (ButtonType(..), defaultButton)
import Components.ImageSlider.ImageSlider as ImageSlider
import Components.ImageSlider.Type as ImageSlider
import Components.Microdata as Microdata
import Components.Pager as Pager
import Components.Pagination as Pagination
import Components.Product.Type exposing (Model, Msg(..), initProductModel)
import Components.SeedAttribute as SeedAttribute
import Components.Sorting as Sorting
import Components.Svg exposing (shoppingCartSvgSmall)
import Components.Tooltip as Tooltip
import Data.Fields exposing (Cents(..), centsToString, imageToSrcSet, imgSrcFallback, lotSizeToString)
import Data.PageData as PageData
import Data.Product as Product exposing (InventoryPolicy(..), Product, ProductId(..), ProductVariant, ProductVariantId(..), productMainImage, unProductId, variantPrice)
import Data.Routing.Routing as Routing exposing (Route(..))
import Data.SeedAttribute as SeedAttribute exposing (SeedAttribute)
import Data.Shared exposing (Shared)
import Data.ViewKey exposing (ViewKey)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (alt, attribute, class, href, id, selected, src, type_, value)
import Html.Events exposing (on, onClick, targetValue)
import Html.Extra exposing (viewIfLazy)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Paginate exposing (Paginated)
import Utils.Format as Format
import Utils.View exposing (htmlOrBlank, rawHtml, routeLinkAttributes)



--------------------------------------------------------------
-- Utils
--------------------------------------------------------------


getMaybeSelectedVariant : PageData.ProductData -> Model -> Maybe ProductVariant
getMaybeSelectedVariant ( _, variants, _ ) model =
    model.variant
        |> Maybe.andThen (\(ProductVariantId i) -> Dict.get i variants)


getVariantSelect : Shared pmsg -> PageData.ProductData -> Model -> List (Html pmsg)
getVariantSelect shared (( product, variants, _ ) as productData) model =
    let
        showVariantSelect =
            Dict.size variants > 1

        variantSelect _ =
            customSelectView shared product (getMaybeSelectedVariant productData model) (Dict.values variants)
    in
    case showVariantSelect of
        True ->
            [ viewIfLazy showVariantSelect variantSelect ]

        False ->
            []


getOffersMeta : PageData.ProductData -> Html Never
getOffersMeta ( product, variants, _ ) =
    let
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
    in
    div [] <|
        List.map
            renderOfferMeta
            (Dict.values variants)



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


priceView : AddToCart.ProductFormStyle -> Maybe ProductVariant -> Html msg
priceView style maybeSelectedVariant =
    let
        class_ =
            case style of
                AddToCart.Card ->
                    "tw:text-[28px] tw:font-bold"

                _ ->
                    "tw:text-[40px] tw:leading-[44px] tw:font-bold "
    in
    maybeSelectedVariant
        |> htmlOrBlank (\v -> p [ class class_ ] [ renderPrice v ])


detailFormView : Shared pmsg -> PageData.ProductDetails -> Model -> Html pmsg
detailFormView shared { product, variants, maybeSeedAttribute, categories } model =
    let
        key =
            "product-detail"

        productData =
            ( product, variants, maybeSeedAttribute )

        selectedItemNumber =
            getItemNumber product maybeSelectedVariant

        maybeSelectedVariant =
            getMaybeSelectedVariant productData model

        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <| []

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
    div formAttributes
        [ div [ class "tw:pb-[16px]" ]
            [ productAttrView shared key selectedItemNumber maybeSeedAttribute
            ]
        , div [ class "tw:pt-[12px] tw:pb-[24px]" ] [ priceView AddToCart.Detail maybeSelectedVariant ]
        , outOfStockView "tw:pb-[24px]" maybeSelectedVariant
        , limitedAvailabilityView shared key "tw:pb-[24px]" maybeSelectedVariant
        , variantSelectView "tw:pb-[24px]" (getVariantSelect shared productData model)
        , Html.map (shared.productMsg product.id << AddToCartMsg) (AddToCart.statusView model.addToCart)
        , div [ class "tw:pb-[28px] tw:w-full tw:flex tw:flex-col" ]
            [ addToCartInput shared model AddToCart.Detail product maybeSelectedVariant
            ]
        , Html.map never <| getOffersMeta productData
        , div [ Microdata.description, class "tw:text-[16px] static-page tw:text-[#4A2604]" ] [ rawHtml product.longDescription ]
        , div [] categoryBlocks
        , Microdata.mpnMeta product.baseSKU
        , Microdata.skuMeta product.baseSKU
        , Microdata.brandMeta "Southern Exposure Seed Exchange"
        , Microdata.urlMeta <|
            Routing.reverse <|
                ProductDetails product.slug Nothing
        ]


detailView : Shared pmsg -> Dict Int Model -> PageData.ProductDetails -> List (Html pmsg)
detailView shared productDict ({ product, variants } as productDetail) =
    let
        model =
            Dict.get (unProductId product.id) productDict
                |> Maybe.withDefault initProductModel
    in
    [ div (Microdata.product ++ [ class "tw:flex tw:gap-[40px] tw:flex-col tw:lg:flex-row tw:items-center tw:lg:items-start" ])
        [ ImageSlider.view
            (Gallery.openOnClick shared.lightboxMsg)
            (shared.productMsg product.id << ImageSliderMsg)
            model.imageSlider
        , div []
            [ h6 [ class "tw:text-[32px]! tw:pb-[16px]" ]
                [ Product.singleVariantName product variants
                ]
            , detailFormView shared productDetail model
            ]
        ]
    ]


cardFormView : Shared pmsg -> ViewKey -> PageData.ProductData -> Model -> Html pmsg
cardFormView shared key (( product, _, maybeSeedAttribute ) as productData) model =
    let
        formAttributes =
            (::) (class "add-to-cart-form tw:flex tw:flex-col tw:grow") <| []

        selectedItemNumber =
            getItemNumber product maybeSelectedVariant

        maybeSelectedVariant =
            getMaybeSelectedVariant productData model
    in
    div formAttributes
        [ div [ class "tw:pt-[12px] " ] [ productAttrView shared key selectedItemNumber maybeSeedAttribute ]
        , outOfStockView "tw:pt-[12px]" maybeSelectedVariant
        , limitedAvailabilityView shared key "tw:pt-[12px]" maybeSelectedVariant
        , variantSelectView "tw:pt-[12px]" (getVariantSelect shared productData model)
        , div [ Microdata.description, class "tw:pt-[16px] tw:line-clamp-4 tw:text-[14px] static-page tw:text-[#4A2604]" ] [ rawHtml product.longDescription ]
        , Microdata.mpnMeta product.baseSKU
        , Microdata.skuMeta product.baseSKU
        , Microdata.brandMeta "Southern Exposure Seed Exchange"
        , div [ class "tw:grow" ]
            []
        , div [ class "tw:pt-[24px]" ]
            [ Html.map (shared.productMsg product.id << AddToCartMsg) (AddToCart.statusView model.addToCart)
            , div [ class "tw:flex tw:items-center" ]
                [ priceView AddToCart.Card maybeSelectedVariant
                , div [ class "tw:grow" ] []
                , addToCartInput shared model AddToCart.Card product maybeSelectedVariant
                , Html.map never <| getOffersMeta productData
                ]
            ]
        ]


cardView : Shared pmsg -> ViewKey -> PageData.ProductData -> Model -> Html pmsg
cardView shared key (( product, variants, _ ) as productData) model =
    div (Microdata.product ++ [ class "tw:bg-[rgba(77,170,154,0.06)] tw:transition-colors tw:hover:bg-[rgba(77,170,154,0.1)] tw:rounded-[16px] tw:p-[16px] tw:flex tw:flex-col" ])
        [ productImageLinkView "tw:w-full tw:h-[200px]" product Nothing
        , div [ class "tw:pt-[16px]" ]
            [ h6 [ class "mb-0 d-flex justify-content-between" ]
                [ a (Microdata.url :: routeLinkAttributes (ProductDetails product.slug Nothing) ++ [ class "tw:line-clamp-2 tw:text-[18px] tw:leading-[28px]" ])
                    [ Product.singleVariantName product variants ]
                ]
            ]
        , cardFormView shared key productData model
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
            , class "tw:w-full tw:lg:w-[360px] tw:aspect-square tw:object-cover tw:rounded-[16px] clickable-image"
            ]
            []
        ]


listView : Shared pmsg -> (Pagination.Data -> Route) -> Pagination.Data -> Dict Int Model -> Paginated PageData.ProductData a c -> List (Html pmsg)
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
                (\i (( p, _, _ ) as productData) ->
                    let
                        productModel =
                            Dict.get (unProductId p.id) productDict
                                |> Maybe.withDefault initProductModel
                    in
                    cardView shared ("card" ++ String.fromInt i) productData productModel
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


addToCartInput : Shared pmsg -> { a | addToCart : AddToCart.Model } -> AddToCart.ProductFormStyle -> Product -> Maybe ProductVariant -> Html pmsg
addToCartInput shared model style product maybeSelectedVariant =
    let
        view =
            div []
                [ case maybeSelectedVariant of
                    Nothing ->
                        text ""

                    Just variant ->
                        Html.map (shared.productMsg product.id << AddToCartMsg) (AddToCart.view style variant.id model.addToCart)
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
