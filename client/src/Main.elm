module Main exposing (main)

import Html exposing (Html, text, div, h1, h4, hr, node, br, a, img, span, button, ul, li, small)
import Html.Attributes exposing (attribute, id, class, href, src, type_, target)
import Html.Attributes.Extra exposing (innerHtml)
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { pageData : WebData ProductDetailsData
    }


init : ( Model, Cmd Msg )
init =
    ( { pageData = RemoteData.Loading }
    , getProductDetailsData "green-pod-red-seed-asparagus-yardlong-bean-7-g"
    )


type Cents
    = Cents Int


type Milligrams
    = Milligrams Int


type ProductId
    = ProductId Int


type alias Product =
    { id : ProductId
    , name : String
    , slug : String
    , baseSKU : String
    , shortDescription : String
    , longDescription : String
    , imageURL : String
    }


type ProductVariantId
    = ProductVariantId Int


type alias ProductVariant =
    { id : ProductVariantId
    , product : ProductId
    , skuSuffix : String
    , price : Cents
    , quantity : Int
    , weight : Milligrams
    , isActive : Bool
    }


type SeedAttributeId
    = SeedAttributeId Int


type alias SeedAttribute =
    { id : SeedAttributeId
    , product : ProductId
    , isOrganic : Bool
    , isHeirloom : Bool
    , isEcological : Bool
    , isRegional : Bool
    }


type alias ProductDetailsData =
    { product : Product
    , variants : List ProductVariant
    , maybeSeedAttribute : Maybe SeedAttribute
    }



-- COMMANDS


getProductDetailsData : String -> Cmd Msg
getProductDetailsData slug =
    Http.get ("/api/" ++ slug) productDetailsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map GetProductDetailsData


productDetailsDecoder : Decode.Decoder ProductDetailsData
productDetailsDecoder =
    Decode.map3 ProductDetailsData
        (Decode.field "product" productDecoder)
        (Decode.field "variants" <| Decode.list productVariantDecoder)
        (Decode.field "seedAttribute" <| Decode.nullable seedAttributeDecoder)


productDecoder : Decode.Decoder Product
productDecoder =
    Decode.map7 Product
        (Decode.field "id" <| Decode.map ProductId Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "slug" Decode.string)
        (Decode.field "baseSku" Decode.string)
        (Decode.field "shortDescription" Decode.string)
        (Decode.field "longDescription" Decode.string)
        (Decode.field "imageUrl" Decode.string)


productVariantDecoder : Decode.Decoder ProductVariant
productVariantDecoder =
    Decode.map7 ProductVariant
        (Decode.field "id" <| Decode.map ProductVariantId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "skuSuffix" Decode.string)
        (Decode.field "price" <| Decode.map Cents Decode.int)
        (Decode.field "quantity" Decode.int)
        (Decode.field "weight" <| Decode.map Milligrams Decode.int)
        (Decode.field "isActive" Decode.bool)


seedAttributeDecoder : Decode.Decoder SeedAttribute
seedAttributeDecoder =
    Decode.map6 SeedAttribute
        (Decode.field "id" <| Decode.map SeedAttributeId Decode.int)
        (Decode.field "productId" <| Decode.map ProductId Decode.int)
        (Decode.field "isOrganic" Decode.bool)
        (Decode.field "isHeirloom" Decode.bool)
        (Decode.field "isEcological" Decode.bool)
        (Decode.field "isRegional" Decode.bool)



-- UPDATE


type Msg
    = GetProductDetailsData (RemoteData.WebData ProductDetailsData)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GetProductDetailsData response ->
            case response of
                RemoteData.Success data ->
                    ( { model | pageData = response }, Cmd.none )

                resp ->
                    let
                        _ =
                            Debug.log "Non Success Returned" resp
                    in
                        ( { model | pageData = response }, Cmd.none )



-- VIEW


view : Model -> Html msg
view { pageData } =
    let
        siteHeader =
            div [ class "container" ]
                [ div [ id "site-header", class "row clearfix" ]
                    [ div [ class "col-sm-7 col-lg-6" ]
                        [ div [ class "media" ]
                            [ a [ href "/" ]
                                [ img
                                    [ id "site-logo"
                                    , class "float-left mx-3"
                                    , src "/static/img/logos/sese.png"
                                    ]
                                    []
                                ]
                            , div [ id "site-title", class "media-body my-auto" ]
                                [ h1 [ class "media-heading m-0" ]
                                    [ a [ href "/" ]
                                        [ text "Southern Exposure"
                                        , br [] []
                                        , text "Seed Exchange"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "col-sm-5 col-lg-6 d-none d-sm-block text-right" ]
                        [ text "LINKS / SEARCH" ]
                    ]
                ]

        navigation =
            div [ id "navigation", class "container" ]
                [ node "nav"
                    [ class "navbar navbar-expand-md navbar-light bg-success" ]
                    [ button
                        [ class "navbar-toggler"
                        , type_ "button"
                        , attribute "data-toggle" "collapse"
                        , attribute "data-target" "#category-navbar"
                        , attribute "aria-controls" "navbarSupportedContent"
                        , attribute "aria-expanded" "false"
                        , attribute "aria-label" "Toggle navigation"
                        ]
                        [ span [ class "navbar-toggler-icon" ] [] ]
                    , div [ id "category-navbar", class "collapse navbar-collapse" ]
                        [ ul [ class "navbar-nav mx-auto d-flex text-left" ]
                            [ li [ class "nav-item" ]
                                [ a [ class "nav-link", href "#" ] [ text "Vegetables" ]
                                ]
                            , li [ class "nav-item active" ]
                                [ a [ class "nav-link", href "#" ] [ text "Flowers" ]
                                ]
                            ]
                        ]
                    ]
                ]

        middleContent =
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col order-2" ] pageContent
                    , sidebar
                    ]
                ]

        footer =
            div [ id "footer", class "container" ]
                [ node "footer"
                    []
                    [ div [ class "row" ]
                        [ div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Information" ] ]
                        , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Important Links" ] ]
                        , div [ class "col-sm-4" ] [ h4 [ class "mt-3" ] [ text "Contact Us" ] ]
                        , div [ class "col-sm-12 text-center" ]
                            [ text "Copyright © 2017 Southern Exposure Seed Exchange"
                            ]
                        ]
                    ]
                ]

        sidebar =
            div [ id "sidebar", class "col-sm-3 col-lg-3 col-xl-3 order-1" ]
                [ div [ class "card mb-3" ]
                    [ div [ class "card-body text-center" ]
                        [ a [ target "_blank", href "http://www.facebook.com/pages/Southern-Exposure-Seed-Exchange/353814746253?ref=ts" ]
                            [ img [ class "img-fluid", src <| staticImage "logos/facebook-big-icon.png" ] [] ]
                        , hr [] []
                        , div [ class "text-center font-weight-bold" ] [ text "Our Partners" ]
                        , a [ target "_blank", href "http://www.smartgardener.com/" ]
                            [ img [ class "mb-3 img-fluid", src <| staticImage "logos/smart-gardener.jpg" ] [] ]
                        , br [] []
                        , a [ target "_blank", href "http://www.localharvest.org/" ]
                            [ img [ class "img-fluid", src <| staticImage "logos/local-harvest.jpg" ] [] ]
                        ]
                    ]
                ]

        seedAttributeIcons { isOrganic, isHeirloom, isRegional, isEcological } =
            List.filter Tuple.first
                [ ( isOrganic, "icons/organic-certified.png" )
                , ( isHeirloom, "icons/heirloom.png" )
                , ( isRegional, "icons/southeast.png" )
                , ( isEcological, "icons/ecologically-grown.png" )
                ]
                |> List.map
                    (Tuple.second
                        >> (\url ->
                                img [ class "my-auto", src <| staticImage url ] []
                           )
                    )
                |> span [ class "d-inline-block ml-3" ]

        pageContent =
            case pageData of
                RemoteData.Loading ->
                    [ text "Loading..." ]

                RemoteData.Success { product, variants, maybeSeedAttribute } ->
                    [ h1 []
                        [ text product.name
                        , htmlOrBlank seedAttributeIcons maybeSeedAttribute
                        ]
                    , hr [] []
                    , div []
                        [ div [ class "clearfix" ]
                            [ div [ class "float-left col-sm-4 col-md-5 col-lg-4" ]
                                [ div
                                    [ class "card" ]
                                    [ div [ class "card-body text-center p-1" ]
                                        [ img
                                            [ src << mediaImage <| "products/" ++ product.imageURL
                                            , class "img-fluid"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            , div [ class "float-right col-sm-4 col-md-3 col-lg-3" ]
                                [ div [ class "card" ]
                                    [ div [ class "card-body text-center p-2" ]
                                        [ h4 [] [ text "$16.95" ]
                                        , text "ADD TO CART BUTTON"
                                        , small [ class "text-muted d-block" ]
                                            [ text <| "Item #" ++ product.baseSKU
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ class "col" ]
                                [ div [ innerHtml product.longDescription ] []
                                ]
                            ]
                        ]
                    ]

                e ->
                    [ text <| toString e ]
    in
        div []
            [ siteHeader
            , navigation
            , middleContent
            , footer
            ]


staticImage : String -> String
staticImage path =
    "/static/img/" ++ path


mediaImage : String -> String
mediaImage path =
    "/media/" ++ path


htmlOrBlank : (a -> Html msg) -> Maybe a -> Html msg
htmlOrBlank renderFunction =
    Maybe.map renderFunction
        >> Maybe.withDefault (text "")
