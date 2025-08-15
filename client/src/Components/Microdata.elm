module Components.Microdata exposing
    ( breadcrumbList, itemListElement, organization, postalAddress, website, searchAction, product, offer
    , item, name, description, image, position, url, category, offers
    , legalName, slogan, logo, email, telephone, faxNumber, sameAs
    , address, streetAddress, addressLocality, addressRegion, postalCode
    , potentialAction, target, queryInput
    , availability, condition, mpn, sku, price, priceCurrency, brand
    , link, urlLink, sameAsLink, logoLink
    , meta, descriptionMeta, positionMeta, urlMeta, targetMeta, legalNameMeta, sloganMeta, availabilityMeta, conditionMeta, mpnMeta, skuMeta, priceMeta, priceCurrencyMeta, brandMeta
    , itemprop, itemscope, ItemType(..), itemtype
    , ItemAvailability(..), itemAvailability
    , ItemCondition(..), itemCondition
    )

{-| Html attributes for applying Microdata Structured Data to elements.

@docs breadcrumbList, itemListElement, organization, postalAddress, website, searchAction, product, offer

@docs item, name, description, image, position, url, category, offers

@docs legalName, slogan, logo, email, telephone, faxNumber, sameAs

@docs address, streetAddress, addressLocality, addressRegion, postalCode

@docs potentialAction, target, queryInput

@docs availability, condition, mpn, sku, price, priceCurrency, brand

@docs link, urlLink, sameAsLink, logoLink

@docs meta, descriptionMeta, positionMeta, urlMeta, targetMeta, legalNameMeta, sloganMeta, availabilityMeta, conditionMeta, mpnMeta, skuMeta, priceMeta, priceCurrencyMeta, brandMeta

@docs itemprop, itemscope, ItemType, itemtype

@docs ItemAvailability, itemAvailability

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


product : List (Attribute msg)
product =
    [ itemscope
    , itemtype Product
    ]


offer : List (Attribute msg)
offer =
    [ itemscope
    , itemtype Offer
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


image : Attribute msg
image =
    itemprop "image"


category : Attribute msg
category =
    itemprop "category"


offers : Attribute msg
offers =
    itemprop "offers"


availability : Attribute msg
availability =
    itemprop "availability"


condition : Attribute msg
condition =
    itemprop "itemCondition"


mpn : Attribute msg
mpn =
    itemprop "mpn"


sku : Attribute msg
sku =
    itemprop "sku"


price : Attribute msg
price =
    itemprop "price"


priceCurrency : Attribute msg
priceCurrency =
    itemprop "priceCurrency"


brand : Attribute msg
brand =
    itemprop "brand"



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


availabilityMeta : ItemAvailability -> Html msg
availabilityMeta val =
    meta [ availability ] <| itemAvailability val


conditionMeta : ItemCondition -> Html msg
conditionMeta val =
    meta [ condition ] <| itemCondition val


mpnMeta : String -> Html msg
mpnMeta =
    meta [ mpn ]


skuMeta : String -> Html msg
skuMeta =
    meta [ sku ]


priceMeta : String -> Html msg
priceMeta =
    meta [ price ]


priceCurrencyMeta : String -> Html msg
priceCurrencyMeta =
    meta [ priceCurrency ]


descriptionMeta : String -> Html msg
descriptionMeta =
    meta [ description ]


brandMeta : String -> Html msg
brandMeta =
    meta [ brand ]


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

                Product ->
                    "Product"

                Offer ->
                    "Offer"
    in
    attribute "itemtype" <| schemaUrl ++ name_


type ItemType
    = BreadcrumbList
    | ListItem
    | Organization
    | PostalAddress
    | WebSite
    | SearchAction
    | Product
    | Offer


itemAvailability : ItemAvailability -> String
itemAvailability val =
    (\s -> schemaUrl ++ s) <|
        case val of
            InStock ->
                "InStock"

            OutOfStock ->
                "OutOfStock"


type ItemAvailability
    = InStock
    | OutOfStock


type ItemCondition
    = NewCondition
    | UsedCondition


itemCondition : ItemCondition -> String
itemCondition val =
    (\s -> schemaUrl ++ s) <|
        case val of
            NewCondition ->
                "NewCondition"

            UsedCondition ->
                "UsedCondition"


schemaUrl : String
schemaUrl =
    "https://schema.org/"
