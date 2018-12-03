module SiteUI
    exposing
        ( NavigationData
        , navigationDecoder
        )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Category exposing (Category, CategoryId(..))


type alias NavigationData =
    { roots : List Category
    , children : Dict Int (List Category)
    }


navigationDecoder : Decoder NavigationData
navigationDecoder =
    Decode.map2 NavigationData
        (Decode.field "rootCategories" <| Decode.list Category.decoder)
        (Decode.field "childrenCategories" <|
            Decode.map stringToIntKeys <|
                Decode.dict <|
                    Decode.list Category.decoder
        )


stringToIntKeys : Dict String v -> Dict Int v
stringToIntKeys =
    Dict.foldl
        (\key value newDict ->
            case String.toInt key of
                Nothing ->
                    newDict

                Just newKey ->
                    Dict.insert newKey value newDict
        )
        Dict.empty
