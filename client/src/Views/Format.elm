module Views.Format
    exposing
        ( date
        , cents
        )

import Decimal
import Time.DateTime as DateTime exposing (DateTime)
import Models.Fields exposing (Cents(..))


date : DateTime -> String
date dateTime =
    [ DateTime.month dateTime
    , DateTime.day dateTime
    , DateTime.year dateTime % 100
    ]
        |> List.map toString
        |> String.join "/"


cents : Cents -> String
cents (Cents i) =
    Decimal.fromInt i
        |> Decimal.mul (Decimal.unsafeFromString "0.01")
        |> Decimal.round -2
        |> Decimal.toString
        |> (++) "$"
