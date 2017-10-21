module Auth.MyAccount exposing (getDetails, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Api
import Messages exposing (Msg(NavigateTo, GetMyAccountDetails, ShowAllOrders))
import Views.Format as Format
import Views.Utils exposing (routeLinkAttributes)
import Routing exposing (Route(EditLogin, EditContact, OrderDetails))
import PageData exposing (MyAccount)
import Locations exposing (AddressLocations)


getDetails : String -> Maybe Int -> Cmd Msg
getDetails token maybeLimit =
    Api.get (Api.CustomerMyAccount maybeLimit)
        |> Api.withToken token
        |> Api.withJsonResponse PageData.myAccountDecoder
        |> Api.sendRequest GetMyAccountDetails


view : AddressLocations -> MyAccount -> List (Html Msg)
view locations { orderSummaries } =
    let
        accountLinks =
            [ li []
                [ a (routeLinkAttributes EditLogin)
                    [ text "Edit Login Details" ]
                ]
            , li []
                [ a (routeLinkAttributes EditContact)
                    [ text "Edit Contact Information" ]
                ]
            ]

        summaryTable =
            if List.isEmpty orderSummaries then
                text ""
            else
                div []
                    [ h3 [] [ text "Recent Orders" ]
                    , orderTable locations orderSummaries
                    ]
    in
        [ h1 [] [ text "My Account" ]
        , hr [] []
        , ul [] accountLinks
        , summaryTable
        ]


orderTable : AddressLocations -> List PageData.OrderSummary -> Html Msg
orderTable locations orderSummaries =
    let
        orderRow { id, shippingAddress, status, total, created } =
            tr []
                [ td [ class "text-center" ] [ text <| Format.date created ]
                , td [ class "text-center" ] [ text <| toString id ]
                , td [] [ addressInfo shippingAddress ]
                , td [ class "text-right" ] [ text <| Format.cents total ]
                , td [ class "text-center" ]
                    [ button
                        [ class "btn btn-light btn-sm"
                        , onClick <| NavigateTo (OrderDetails id)
                        ]
                        [ text "View" ]
                    ]
                ]

        showAllButton =
            div [ class "form-group text-right" ]
                [ button [ class "btn btn-light", onClick ShowAllOrders ]
                    [ text "Show All Orders" ]
                ]

        addressInfo { firstName, lastName, street, city, state, zipCode } =
            address [ class "mb-0" ]
                [ b [] [ text <| firstName ++ " " ++ lastName ]
                , br [] []
                , text street
                , br [] []
                , text city
                , text ", "
                , Locations.regionName locations state |> Maybe.map text |> Maybe.withDefault (text "")
                , text " "
                , text zipCode
                ]
    in
        div []
            [ table [ class "table table-sm table-striped" ]
                [ thead []
                    [ tr []
                        [ th [ class "text-center" ] [ text "Date" ]
                        , th [ class "text-center" ] [ text "Order #" ]
                        , th [] [ text "Shipping Address" ]
                        , th [ class "text-right" ] [ text "Total" ]
                        , th [] []
                        ]
                    ]
                , tbody [] <| List.map orderRow orderSummaries
                ]
            , showAllButton
            ]
