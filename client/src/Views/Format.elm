module Views.Format
    exposing
        ( cents
        , centsNumber
        , date
        )

import DateFormat
import Decimal
import Models.Fields exposing (Cents(..))
import Time exposing (Posix, Zone)


{-| Format a Posix Time into a MM/DD/YY string.
-}
date : Zone -> Posix -> String
date =
    DateFormat.format
        [ DateFormat.monthFixed
        , DateFormat.text "/"
        , DateFormat.dayOfMonthFixed
        , DateFormat.text "/"
        , DateFormat.yearNumberLastTwo
        ]


{-| Format a Cents into a decimal-representation of Dollars prefixed with a
Dollar-sign.
-}
cents : Cents -> String
cents ((Cents i) as c) =
    if i < 0 then
        "−$" ++ centsNumber (Cents <| abs i)
    else
        "$" ++ centsNumber c


{-| Format a Cents into a decimal-representation of Dollars.
-}
centsNumber : Cents -> String
centsNumber (Cents c) =
    Decimal.fromInt c
        |> Decimal.mul (Decimal.fromIntWithExponent 1 -2)
        |> Decimal.round -2
        |> Decimal.toString
