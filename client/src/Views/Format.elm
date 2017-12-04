module Views.Format
    exposing
        ( date
        , cents
        , centsNumber
        )

import Decimal
import Time.DateTime as DateTime exposing (DateTime)
import Models.Fields exposing (Cents(..))


{-| Format a DateTime into a MM/DD/YY string.
-}
date : DateTime -> String
date dateTime =
    [ DateTime.month dateTime
    , DateTime.day dateTime
    , DateTime.year dateTime % 100
    ]
        |> List.map toString
        |> String.join "/"


{-| Format a Cents into a decimal-representation of Dollars prefixed with a
Dollar-sign.
-}
cents : Cents -> String
cents ((Cents i) as c) =
    "$" ++ centsNumber c


{-| Format a Cents into a decimal-representation of Dollars.
-}
centsNumber : Cents -> String
centsNumber (Cents c) =
    Decimal.fromInt c
        |> Decimal.mul (Decimal.unsafeFromString "0.01")
        |> Decimal.round -2
        |> Decimal.toString
