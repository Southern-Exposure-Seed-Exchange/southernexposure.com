module Components.Shared exposing (..)

import Components.Button as Button exposing (defaultButton)
import Components.IconButton as IconButton
import Components.Svg exposing (..)
import Data.Fields exposing (Cents(..), centsMap, centsMap2)
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..), reverse)
import Data.Search exposing (UniqueSearch(..))
import Data.User exposing (AuthStatus(..))
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, id, src)
import List exposing (member)
import Utils.Format as Format
import Utils.Images as Images
import Utils.View exposing (routeLinkAttributes)


logoAndName : Route -> Html msg
logoAndName linkRoute =
    let
        logoImage =
            div [ class "tw:w-[48px] tw:lg:w-[84px]" ]
                [ img
                    [ id "site-logo"
                    , class ""

                    -- , class "float-left mr-2 mx-lg-3"
                    , src <| Images.static "logos/sese.png"
                    , alt "SESE's Logo - Two Hands Supporting a Growing Flower"
                    ]
                    []
                ]

        homeRoute =
            Routing.homePage

        titleLink =
            a (class "d-block tw:text-[#1E0C03]! tw:text-[18px] tw:lg:text-[29px]" :: routeLinkAttributes linkRoute)
                [ text "Southern Exposure"
                , br [ class "d-block" ] []
                , text " Seed Exchange"
                ]
    in
    div [ class "tw:flex tw:gap-[16px] tw:items-center" ]
        [ a (class "my-auto" :: routeLinkAttributes linkRoute) [ logoImage ]
        , div [ id "site-title", class "media-body tw:pt-[4px] tw:lg:pt-0" ]
            [ h1 [ class "media-heading m-0 " ] [ titleLink ]
            ]
        ]


searchIcon : Html msg
searchIcon =
    a
        [ class "tw:relative tw:p-[6px]! tw:cursor-pointer tw:group", href "/all-products" ]
        [ searchSvgBig
        ]


cartIcon : Int -> Html msg
cartIcon cartItemCount =
    a
        [ class "tw:relative tw:p-[6px]! tw:cursor-pointer tw:group", href (reverse Cart) ]
        [ if cartItemCount > 0 then
            div [ class "tw:absolute tw:top-[-6px] tw:right-[-6px]" ]
                [ div [ class "tw:w-[24px] tw:h-[24px] tw:bg-[#D62246] tw:rounded-full tw:flex tw:items-center tw:justify-center" ]
                    [ span [ class "tw:pt-[1px] tw:block tw:text-white tw:text-[12px]" ] [ text <| String.fromInt cartItemCount ]
                    ]
                ]

          else
            div [] []
        , IconButton.view shoppingCartSvg
        ]


receiptProductMobileView { nameView, sku, quantity, price } =
    div [ class "tw:grow" ]
        [ div []
            [ div [ class "font-weight-bold" ] [ nameView ]
            , small [ class "text-muted" ]
                [ text <| "Item #" ++ sku ]
            ]
        , div [ class "tw:pt-[6px] tw:flex tw:items-center" ]
            [ span [ class "tw:text-[18px] tw:leading-[28px] tw:font-semibold tw:grow" ]
                [ text <| Format.cents <| centsMap ((*) quantity) <| price ]
            , span [ class "tw:shrink-0 tw:opacity-80" ]
                [ text <|
                    (Format.cents <| price)
                        ++ " x "
                        ++ String.fromInt quantity
                ]
            ]
        ]


receiptTotalMobileView :
    { a
        | subTotal : Cents
        , total : Cents
        , tax : Maybe { b | amount : Cents, description : String }
        , maybeAppliedCredit : Maybe Cents -- negative amount
        , memberDiscount : Maybe { c | description : String, amount : Cents } -- negative amount
        , couponDiscount : Maybe { c | description : String, amount : Cents } -- negative amount
        , shippingMethod : Maybe { d | description : String, amount : Cents }
        , priorityShipping : Maybe { f | description : String, amount : Cents }
        , surcharges : List { g | description : String, amount : Cents }
    }
    -> Maybe msg
    -> Html msg
receiptTotalMobileView { subTotal, total, tax, maybeAppliedCredit, memberDiscount, couponDiscount, shippingMethod, priorityShipping, surcharges } maybeRemoveCouponMsg =
    let
        subTotalRowMobile =
            if subTotal /= total then
                footerRowMobile "Sub-Total" subTotal "font-weight-bold"

            else
                text ""

        taxRow =
            case tax of
                Just t ->
                    if t.amount == Cents 0 then
                        text ""

                    else
                        chargeRow t

                Nothing ->
                    text ""

        chargeRow { description, amount } =
            footerRowMobile description amount ""

        maybeChargeRow =
            Maybe.map chargeRow >> Maybe.withDefault (text "")

        storeCreditRow =
            case maybeAppliedCredit of
                Nothing ->
                    text ""

                Just (Cents 0) ->
                    text ""

                Just credit ->
                    footerRowMobile "Store Credit" credit "tw:font-semibold tw:text-[rgba(77,170,154,1)]"

        totalRow =
            footerRowMobile "Total" total "font-weight-bold tw:text-[18px] tw:leading-[24px]"

        couponDiscountRow =
            case couponDiscount of
                Nothing ->
                    text ""

                Just { description, amount } ->
                    div [ class "checkout-coupon-line tw:flex tw:items-center tw:gap-[8px]" ]
                        [ span [ class "tw:line-clamp-1" ] [ text <| description ++ "" ]
                        , case maybeRemoveCouponMsg of
                            Just msg ->
                                Button.view { defaultButton | label = "X", style = Button.Outline, type_ = Button.TriggerMsg msg }

                            Nothing ->
                                text ""
                        , div [ class "tw:grow" ] []
                        , div [ class "tw:whitespace-nowrap text-right tw:font-semibold tw:text-[rgba(77,170,154,1)]" ]
                            [ text <| Format.cents amount ]
                        ]

        footerRowMobile content amount rowClass =
            div [ class <| "tw:flex tw:flex-gap-[16px] tw:py-[10px] " ++ rowClass ]
                [ div [ class "" ] [ text <| content ++ ":" ]
                , div [ class "tw:grow" ] []
                , div [ class "" ] [ text <| Format.cents amount ]
                ]
    in
    div [ class "tw:py-[18px] tw:px-[16px] tw:rounded-[16px] tw:bg-[rgba(30,12,3,0.02)]" ] <|
        [ subTotalRowMobile
        , maybeChargeRow shippingMethod
        , maybeChargeRow priorityShipping
        ]
            ++ List.map chargeRow surcharges
            ++ [ taxRow
               , storeCreditRow
               , maybeChargeRow memberDiscount
               , couponDiscountRow
               , totalRow
               ]
