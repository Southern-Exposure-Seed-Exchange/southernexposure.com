module Main exposing (main)

import Html exposing (Html, text, div, h1, hr, node)
import Html.Attributes exposing (class, id)
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
    , getProductDetailsData "growing-great-garlic"
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
                [ div [ class "row clearfix" ]
                    [ div [ class "col-sm-7 col-lg-6" ]
                        [ text "LOGO / STORE NAME" ]
                    , div [ class "col-sm-5 col-lg-6 d-none d-sm-block" ]
                        [ text "LINKS / SEARCH" ]
                    ]
                ]

        navigation =
            div [ class "container" ]
                [ text "NAVIGATION" ]

        middleContent =
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-sm-9 order-2" ] pageContent
                    , sidebar
                    ]
                ]

        footer =
            div [ class "container" ]
                [ node "footer" [] [ text "FOOTER" ]
                ]

        sidebar =
            div [ id "sidebar", class "col-sm-3 col-lg-2 order-1" ] [ text "SIDEBAR" ]

        pageContent =
            case pageData of
                RemoteData.Loading ->
                    [ text "Loading..." ]

                RemoteData.Success { product, variants, maybeSeedAttribute } ->
                    [ h1 []
                        [ text product.name
                        ]
                    , hr [] []
                    , div []
                        [ div [ class "clearfix" ]
                            [ div [ class "float-left col-sm-4 col-md-5 col-lg-5" ]
                                [ text "PRODUCT IMAGE" ]
                            , div [ class "float-right col-sm-4 col-md-3 col-lg-2" ]
                                [ text "ADD TO CART" ]
                            , div [ innerHtml product.longDescription ] []
                            , div [] [ text <| "Item #" ++ product.baseSKU ]
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
