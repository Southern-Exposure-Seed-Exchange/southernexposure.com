module SiteUI exposing
    ( NavigationData
    , navigationDecoder
    , CategoryListData
    , CategoryData
    , categoryListDecoder
    )

import Category exposing (Category, CategoryId(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


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

type alias CategoryListData =
    List CategoryData


type alias CategoryData =
    { id : CategoryId
    , name : String
    }


categoryListDecoder : Decoder CategoryListData
categoryListDecoder =
    Decode.field "categories" <|
        Decode.list <|
            Decode.map2 CategoryData
                (Decode.field "id" <| Decode.map CategoryId Decode.int)
                (Decode.field "name" Decode.string)
