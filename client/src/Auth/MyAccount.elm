module Auth.MyAccount exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import Views.Format as Format
import Views.Utils exposing (routeLinkAttributes)
import Routing exposing (Route(EditLogin, EditContact))
import PageData exposing (MyAccount)
import Locations exposing (AddressLocations)


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


orderTable : AddressLocations -> List PageData.OrderSummary -> Html msg
orderTable locations orderSummaries =
    let
        orderRow { id, shippingAddress, status, total, created } =
            tr []
                [ td [ class "text-center" ] [ text <| Format.date created ]
                , td [ class "text-center" ] [ text <| toString id ]
                , td [] [ addressInfo shippingAddress ]
                , td [ class "text-right" ] [ text <| Format.cents total ]
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
        table [ class "table table-sm table-striped" ]
            [ thead []
                [ tr []
                    [ th [ class "text-center" ] [ text "Date" ]
                    , th [ class "text-center" ] [ text "Order #" ]
                    , th [] [ text "Shipping Address" ]
                    , th [ class "text-right" ] [ text "Total" ]
                    ]
                ]
            , tbody [] <| List.map orderRow orderSummaries
            ]
