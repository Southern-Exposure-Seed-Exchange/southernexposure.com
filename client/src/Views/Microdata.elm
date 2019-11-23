module Views.Microdata exposing
    ( breadcrumbList, itemListElement, organization, postalAddress
    , item, name, description, position, logo, url, sameAs, itemscope
    , email, telephone, faxNumber
    , address, streetAddress, addressLocality, addressRegion, postalCode
    , link, urlLink, sameAsLink, logoLink
    , meta, positionMeta
    , itemprop, ItemType(..), itemtype
    )

{-| Html attributes for applying Microdata Structured Data to elements.

@docs breadcrumbList, itemListElement, organization, postalAddress

@docs item, name, description, position, logo, url, sameAs, itemscope

@docs email, telephone, faxNumber

@docs address, streetAddress, addressLocality, addressRegion, postalCode

@docs link, urlLink, sameAsLink, logoLink

@docs meta, positionMeta

@docs itemprop, ItemType, itemtype

-}

import Html exposing (..)
import Html.Attributes exposing (..)



-- ITEM TYPES


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


organization : List (Attribute msg)
organization =
    [ itemscope
    , itemtype Organization
    ]


postalAddress : List (Attribute msg)
postalAddress =
    [ itemscope
    , itemtype PostalAddress
    ]



-- PROPERTIES


item : Attribute msg
item =
    itemprop "item"


name : Attribute msg
name =
    itemprop "name"


position : Attribute msg
position =
    itemprop "position"


legalName : Attribute msg
legalName =
    itemprop "legalName"


logo : Attribute msg
logo =
    itemprop "logo"


telephone : Attribute msg
telephone =
    itemprop "telephone"


address : Attribute msg
address =
    itemprop "address"


streetAddress : Attribute msg
streetAddress =
    itemprop "streetAddress"


addressLocality : Attribute msg
addressLocality =
    itemprop "addressLocality"


addressRegion : Attribute msg
addressRegion =
    itemprop "addressRegion"


postalCode : Attribute msg
postalCode =
    itemprop "postalCode"


email : Attribute msg
email =
    itemprop "email"


faxNumber : Attribute msg
faxNumber =
    itemprop "faxNumber"


description : Attribute msg
description =
    itemprop "description"


url : Attribute msg
url =
    itemprop "url"


sameAs : Attribute msg
sameAs =
    itemprop "sameAs"



-- META


positionMeta : Int -> Html msg
positionMeta i =
    meta [ position ] <| String.fromInt i


meta : List (Attribute msg) -> String -> Html msg
meta props content =
    node "meta" (attribute "content" content :: props) []



-- LINK


urlLink : String -> Html msg
urlLink =
    link [ url ]


sameAsLink : String -> Html msg
sameAsLink =
    link [ sameAs ]


logoLink : String -> Html msg
logoLink =
    link [ logo ]


link : List (Attribute msg) -> String -> Html msg
link props dest =
    node "link" (href dest :: props) []



-- HELPERS


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

                Organization ->
                    "Organization"

                PostalAddress ->
                    "PostalAddress"
    in
    attribute "itemtype" <| "https://schema.org/" ++ name_


type ItemType
    = BreadcrumbList
    | ListItem
    | Organization
    | PostalAddress
