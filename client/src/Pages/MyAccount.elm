module Pages.MyAccount exposing (getDetails, view)

import Components.Alert as Alert exposing (defaultAlert)
import Components.Button as Button exposing (ButtonType(..), defaultButton)
import Components.Svg exposing (discountSvg)
import Data.Api as Api
import Data.Fields exposing (Cents(..))
import Data.Locations as Locations exposing (AddressLocations)
import Data.Msg exposing (Msg(..))
import Data.PageData as PageData exposing (MyAccount)
import Data.Routing.Routing as Routing exposing (Route(..), reverse)
import Html exposing (..)
import Html.Attributes exposing (class)
import Time
import Utils.Format as Format
import Utils.View exposing (pageTitleView)


getDetails : Maybe Int -> Cmd Msg
getDetails maybeLimit =
    Api.get (Api.CustomerMyAccount maybeLimit)
        |> Api.withJsonResponse PageData.myAccountDecoder
        |> Api.sendRequest GetMyAccountDetails


view : Time.Zone -> AddressLocations -> MyAccount -> List (Html Msg)
view zone locations { storeCredit, orderSummaries } =
    let
        (Cents credit) =
            storeCredit

        storeCreditText =
            if credit > 0 then
                div [ class "tw:pb-[20px]" ]
                    [ Alert.view
                        { defaultAlert
                            | content =
                                span []
                                    [ span [] [ text "You have " ]
                                    , span [ class "tw:font-semibold" ] [ text <| Format.cents storeCredit ]
                                    , span [] [ text " of Store Credit available. You can use this during Checkout." ]
                                    ]
                            , icon = Just (span [ class "tw:pt-[1px]" ] [ discountSvg "tw:fill-[#4DAA9A]" ])
                        }
                    ]

            else
                text ""

        summaryTable =
            if List.isEmpty orderSummaries then
                div [ class "tw:p-[8px]" ] [ text "You have no recent orders." ]

            else
                div []
                    [ orderTable zone locations orderSummaries
                    ]
    in
    [ pageTitleView "Recent Orders"
    , storeCreditText
    , summaryTable
    ]


orderTable : Time.Zone -> AddressLocations -> List PageData.OrderSummary -> Html Msg
orderTable zone locations orderSummaries =
    let
        orderRowDesktop { id, shippingAddress, status, total, created } =
            tr []
                [ td [ class "" ] [ text <| Format.date zone created ]
                , td [ class "" ] [ text <| String.fromInt id ]
                , td [] [ addressInfo shippingAddress ]
                , td [ class "" ] [ text <| status ]
                , td [ class "" ] [ text <| Format.cents total ]
                , td [ class "" ]
                    [ Button.view { defaultButton | label = "View", type_ = Button.Link <| reverse <| OrderDetails id Nothing }
                    ]
                ]

        orderRowMobile { id, shippingAddress, status, total, created } =
            div [ class "tw:p-[20px] tw:rounded-[16px] tw:bg-[rgba(30,12,3,0.02)]" ]
                [ p [ class "" ] [ text <| "Order from " ++ Format.date zone created ]
                , p [ class "tw:pt-[4px] tw:opacity-60" ] [ text <| "#" ++ String.fromInt id ]
                , div [ class "tw:pt-[12px]" ]
                    [ span [ class "tw:font-semibold" ] [ text "Order status: " ]
                    , span [] [ text status ]
                    ]
                , div [ class "tw:pt-[16px]" ] [ addressInfo shippingAddress ]
                , div [ class "tw:pt-[16px] tw:font-semibold" ] [ text <| Format.cents total ]
                , div [ class "tw:pt-[12px]" ]
                    [ Button.view { defaultButton | label = "View", type_ = Button.Link <| reverse <| OrderDetails id Nothing }
                    ]
                ]

        showAllButton =
            Button.view { defaultButton | label = "Show All Orders", type_ = TriggerMsg ShowAllOrders, style = Button.Outline }

        addressInfo { firstName, lastName, street, city, state, zipCode } =
            address [ class "mb-0" ]
                [ b [] [ text <| firstName ++ " " ++ lastName ]
                , br [] []
                , text street
                , br [] []
                , text city
                , text ", "
                , state
                    |> Maybe.andThen (Locations.regionName locations)
                    |> Maybe.map text
                    |> Maybe.withDefault (text "")
                , text " "
                , text zipCode
                ]

        desktopView =
            table [ class "se-table tw:hidden tw:lg:block" ]
                [ thead []
                    [ tr []
                        [ th [ class "" ] [ text "Date" ]
                        , th [ class "" ] [ text "Order #" ]
                        , th [] [ text "Shipping Address" ]
                        , th [ class "" ] [ text "Order Status" ]
                        , th [ class "" ] [ text "Total" ]
                        , th [] []
                        ]
                    ]
                , tbody [] <| List.map orderRowDesktop orderSummaries
                ]

        mobileView =
            div [ class "tw:flex tw:lg:hidden tw:flex-col tw:gap-[12px]" ] <|
                List.map orderRowMobile orderSummaries
    in
    div []
        [ desktopView
        , mobileView

        -- TODO: handle mobile view here
        , div [ class "tw:pt-[28px] tw:flex tw:justify-center" ]
            [ showAllButton
            ]
        ]
