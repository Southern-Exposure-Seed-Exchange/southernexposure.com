module Views.Microdata exposing
    ( breadcrumbList, itemListElement
    , item, name, position, itemscope
    , meta, positionMeta
    , itemprop, ItemType(..), itemtype
    )

{-| Html attributes for applying Microdata Structured Data to elements.

@docs breadcrumbList, itemListElement

@docs item, name, position, itemscope

@docs meta, positionMeta

@docs itemprop, ItemType, itemtype

-}

import Html exposing (..)
import Html.Attributes exposing (..)


breadcrumbList : List (Attribute msg)
breadcrumbList =
    [ itemscope
    , itemtype BreadcrumbList
    ]


itemListElement : List (Attribute msg)
itemListElement =
    [ itemscope
    , itemtype ListItem
    , itemprop "itemListElement"
    ]


item : Attribute msg
item =
    itemprop "item"


name : Attribute msg
name =
    itemprop "name"


position : Attribute msg
position =
    itemprop "position"


positionMeta : Int -> Html msg
positionMeta i =
    meta [ position ] <| String.fromInt i


meta : List (Attribute msg) -> String -> Html msg
meta props content =
    node "meta" (attribute "content" content :: props) []


itemprop : String -> Attribute msg
itemprop =
    attribute "itemprop"


itemscope =
    attribute "itemscope" ""


itemtype : ItemType -> Attribute msg
itemtype type_ =
    let
        name_ =
            case type_ of
                BreadcrumbList ->
                    "BreadcrumbList"

                ListItem ->
                    "ListItem"
    in
    attribute "itemtype" <| "https://schema.org/" ++ name_


type ItemType
    = BreadcrumbList
    | ListItem
