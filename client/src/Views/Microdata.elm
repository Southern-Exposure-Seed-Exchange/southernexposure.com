module Views.Microdata exposing
    ( breadcrumbList, itemListElement, organization, postalAddress, website, searchAction
    , item, name, description, position, url
    , legalName, slogan, logo, email, telephone, faxNumber, sameAs
    , address, streetAddress, addressLocality, addressRegion, postalCode
    , potentialAction, target, queryInput
    , link, urlLink, sameAsLink, logoLink
    , meta, positionMeta, urlMeta, targetMeta, legalNameMeta, sloganMeta
    , itemprop, itemscope, ItemType(..), itemtype
    )

{-| Html attributes for applying Microdata Structured Data to elements.

@docs breadcrumbList, itemListElement, organization, postalAddress, website, searchAction

@docs item, name, description, position, url

@docs legalName, slogan, logo, email, telephone, faxNumber, sameAs

@docs address, streetAddress, addressLocality, addressRegion, postalCode

@docs potentialAction, target, queryInput

@docs link, urlLink, sameAsLink, logoLink

@docs meta, positionMeta, urlMeta, targetMeta, legalNameMeta, sloganMeta

@docs itemprop, itemscope, ItemType, itemtype

-}

import Html exposing (Attribute, Html, node)
import Html.Attributes exposing (attribute, href)



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


website : List (Attribute msg)
website =
    [ itemscope
    , itemtype WebSite
    ]


searchAction : List (Attribute msg)
searchAction =
    [ itemscope
    , itemtype SearchAction
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


slogan : Attribute msg
slogan =
    itemprop "slogan"


target : Attribute msg
target =
    itemprop "target"


potentialAction : Attribute msg
potentialAction =
    itemprop "potentialAction"


queryInput : Attribute msg
queryInput =
    itemprop "query-input"



-- META


positionMeta : Int -> Html msg
positionMeta i =
    meta [ position ] <| String.fromInt i


urlMeta : String -> Html msg
urlMeta =
    meta [ url ]


targetMeta : String -> Html msg
targetMeta =
    meta [ target ]


legalNameMeta : String -> Html msg
legalNameMeta =
    meta [ legalName ]


sloganMeta : String -> Html msg
sloganMeta =
    meta [ slogan ]


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


itemscope : Attribute msg
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

                WebSite ->
                    "WebSite"

                SearchAction ->
                    "SearchAction"
    in
    attribute "itemtype" <| "https://schema.org/" ++ name_


type ItemType
    = BreadcrumbList
    | ListItem
    | Organization
    | PostalAddress
    | WebSite
    | SearchAction
