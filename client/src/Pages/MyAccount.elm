module Pages.MyAccount exposing (getDetails, view)

import Api
import Components.Alert as Alert exposing (defaultAlert)
import Components.Button as Button exposing (ButtonType(..), defaultButton)
import Components.Svg exposing (discountSvg)
import Html exposing (..)
import Html.Attributes exposing (class)
import Locations exposing (AddressLocations)
import Messages exposing (Msg(..))
import Models.Fields exposing (Cents(..))
import PageData exposing (MyAccount)
import Routing exposing (Route(..), reverse)
import Time
import Views.Format as Format
import Views.Utils exposing (pageTitleView)


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
                            , icon = Just (span [ class "tw:pt-[1px]" ] [ discountSvg ])
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
        orderRow { id, shippingAddress, status, total, created } =
            tr []
                [ td [ class "" ] [ text <| Format.date zone created ]
                , td [ class "" ] [ text <| String.fromInt id ]
                , td [] [ addressInfo shippingAddress ]
                , td [ class "" ] [ text <| status ]
                , td [ class "" ] [ text <| Format.cents total ]
                , td [ class "" ]
                    [ Button.view { defaultButton | label = "View", type_ = Button.Link <| reverse <| OrderDetails id Nothing, style = Button.Outline }
                    ]
                ]

        showAllButton =
            Button.view { defaultButton | label = "Show All Orders", type_ = TriggerMsg ShowAllOrders }

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
    in
    div []
        [ table [ class "se-table" ]
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
            , tbody [] <| List.map orderRow orderSummaries
            ]

        -- TODO: handle mobile view here
        , div [ class "tw:pt-[28px] tw:flex tw:justify-center" ]
            [ showAllButton
            ]
        ]
