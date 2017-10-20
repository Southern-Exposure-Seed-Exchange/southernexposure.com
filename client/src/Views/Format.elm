module Views.Format
    exposing
        ( date
        )

import Time.DateTime as DateTime exposing (DateTime)


date : DateTime -> String
date dateTime =
    [ DateTime.month dateTime
    , DateTime.day dateTime
    , DateTime.year dateTime % 100
    ]
        |> List.map toString
        |> String.join "/"
