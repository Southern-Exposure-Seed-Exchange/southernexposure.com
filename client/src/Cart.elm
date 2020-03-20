module Cart exposing
    ( Form
    , Msg
    , fromCartDetails
    , initial
    , update
    , view
    )

import Api
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (alt, class, colspan, disabled, src, step, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Html.Keyed as Keyed
import Json.Encode as Encode
import Models.Fields exposing (Cents(..), centsMap, imageToSrcSet, imgSrcFallback, lotSizeToString)
import PageData exposing (CartDetails, CartItemId(..))
import Product exposing (variantPrice)
import RemoteData
import Routing exposing (Route(..))
import User exposing (AuthStatus)
import Views.Aria as Aria
import Views.Format as Format
import Views.Utils exposing (htmlOrBlank, icon, numericInput, onIntInput, rawHtml, routeLinkAttributes)



-- MODEL


type alias Form =
    { quantities : Dict Int Int
    }


initial : Form
initial =
    Form Dict.empty


fromCartDetails : CartDetails -> Form
fromCartDetails { items } =
    List.foldl
        (\item acc -> Dict.insert ((\(CartItemId i) -> i) item.id) item.quantity acc)
        Dict.empty
        items
        |> Form



-- UPDATE


type Msg
    = Quantity PageData.CartItemId Int
    | Remove PageData.CartItemId
    | Submit
    | UpdateResponse (RemoteData.WebData PageData.CartDetails)


update :
    Msg
    -> AuthStatus
    -> Maybe String
    -> Form
    -> CartDetails
    -> ( Form, Maybe CartDetails, Cmd Msg )
update msg authStatus maybeCartToken model details =
    case msg of
        Quantity itemId quantity ->
            ( { model
                | quantities =
                    Dict.update (fromCartItemId itemId)
                        (always <| Just quantity)
                        model.quantities
              }
            , Nothing
            , Cmd.none
            )

        Remove itemId ->
            ( model, Nothing, removeItem authStatus maybeCartToken itemId )

        Submit ->
            ( model, Nothing, updateCart authStatus maybeCartToken model details )

        UpdateResponse response ->
            case response of
                RemoteData.Success cartDetails ->
                    ( initial, Just cartDetails, Cmd.none )

                _ ->
                    ( model, Nothing, Cmd.none )


updateCart : AuthStatus -> Maybe String -> Form -> CartDetails -> Cmd Msg
updateCart authStatus maybeCartToken { quantities } { items } =
    let
        changed =
            changedQuantities quantities items

        encodedQuantities =
            Encode.object <|
                [ ( "quantities", Encode.object changed ) ]
                    ++ encodedCartToken maybeCartToken
    in
    if List.isEmpty changed then
        Cmd.none

    else
        case authStatus of
            User.Anonymous ->
                anonymousUpdateRequest encodedQuantities

            User.Authorized _ ->
                customerUpdateRequest encodedQuantities


removeItem : AuthStatus -> Maybe String -> CartItemId -> Cmd Msg
removeItem authStatus maybeCartToken itemId =
    let
        encodedDelete =
            Encode.object <|
                [ ( "quantities"
                  , Encode.object
                        [ ( String.fromInt <| fromCartItemId itemId, Encode.int 0 ) ]
                  )
                ]
                    ++ encodedCartToken maybeCartToken
    in
    case authStatus of
        User.Anonymous ->
            anonymousUpdateRequest encodedDelete

        User.Authorized _ ->
            customerUpdateRequest encodedDelete


anonymousUpdateRequest : Encode.Value -> Cmd Msg
anonymousUpdateRequest body =
    Api.post Api.CartUpdateAnonymous
        |> Api.withJsonBody body
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest UpdateResponse


customerUpdateRequest : Encode.Value -> Cmd Msg
customerUpdateRequest body =
    Api.post Api.CartUpdateCustomer
        |> Api.withJsonBody body
        |> Api.withJsonResponse PageData.cartDetailsDecoder
        |> Api.sendRequest UpdateResponse



-- VIEW


view : Form -> CartDetails -> List (Html Msg)
view ({ quantities } as form_) ({ items, charges } as cartDetails) =
    let
        itemCount =
            List.foldl (.quantity >> (+)) 0 items

        -- TODO: Add commas to format
        cartTable =
            table [ class "d-none d-md-table table table-striped table-sm cart-table" ]
                [ tableHeader
                , Keyed.node "tbody" [] <|
                    List.map
                        (\i ->
                            ( String.fromInt <| fromCartItemId i.id
                            , productRow i
                            )
                        )
                        items
                , tableFooter
                ]

        buttons =
            div [ class "form-group text-right cart-buttons" ]
                [ button
                    [ class "btn btn-success d-none d-sm-inline-block"
                    , type_ "submit"
                    , disabled formIsUnchanged
                    ]
                    [ icon "refresh", text " Update" ]
                , a (class "btn btn-primary ml-md-2" :: routeLinkAttributes Checkout)
                    [ icon "shopping-cart", text " Checkout" ]
                ]

        tableHeader =
            thead []
                [ tr []
                    [ th [ class "text-center" ] [ text "Quantity" ]
                    , th [] [ text "" ]
                    , th [] [ text "Name" ]
                    , th [ class "text-right" ] [ text "Item Price" ]
                    , th [ class "text-right" ] [ text "Item Total" ]
                    , th [ class "text-center" ] [ text "Remove" ]
                    ]
                ]

        productRow { id, product, variant, quantity } =
            tr []
                [ td [ class "text-center align-middle" ]
                    [ input
                        [ class "cart-quantity form-control mx-auto"
                        , type_ "number"
                        , value <| String.fromInt <| Maybe.withDefault 1 <| Dict.get (fromCartItemId id) quantities
                        , onIntInput <| Quantity id
                        , A.min "1"
                        , A.step "1"
                        , numericInput
                        , Aria.label "quantity"
                        ]
                        []
                    ]
                , td [ class "align-middle" ]
                    [ a
                        (Aria.label ("View Product Details for " ++ product.name)
                            :: routeLinkAttributes (ProductDetails product.slug)
                        )
                        [ img
                            [ src <| imgSrcFallback product.image
                            , imageToSrcSet product.image
                            , productImageSizes
                            , class "cart-product-image"
                            , alt <| "Product Image for " ++ product.name
                            ]
                            []
                        ]
                    ]
                , td [ class "align-middle" ]
                    [ div [ class "font-weight-bold" ]
                        [ a (routeLinkAttributes <| ProductDetails product.slug)
                            [ Product.nameWithLotSize product variant ]
                        ]
                    , small
                        [ class "text-muted" ]
                        [ text <| "Item #" ++ product.baseSKU ++ variant.skuSuffix ]
                    ]
                , td [ class "text-right align-middle" ]
                    [ text <| Format.cents <| variantPrice variant ]
                , td [ class "text-right align-middle" ]
                    [ text <| Format.cents <| centsMap ((*) quantity) <| variantPrice variant ]
                , td [ class "text-center align-middle" ]
                    [ button
                        [ class "btn btn-link text-danger"
                        , onClick <| Remove id
                        , Aria.label <| "Remove " ++ product.name ++ " From Cart"
                        ]
                        [ icon "times" ]
                    ]
                ]

        tableFooter =
            tfoot [] <|
                [ footerRow "font-weight-bold" "Sub-Total" totals.subTotal ]
                    ++ List.map chargeRow charges.surcharges
                    ++ [ htmlOrBlank chargeRow <|
                            Maybe.map .charge charges.shippingMethod
                       , taxRow
                       , totalRow
                       ]

        totals =
            PageData.cartTotals cartDetails

        taxRow =
            if charges.tax.amount == Cents 0 then
                text ""

            else
                chargeRow charges.tax

        footerRow rowClass content amount =
            tr [ class rowClass ]
                [ td [ colspan 4, class "text-right" ] [ text <| content ++ ":" ]
                , td [ class "text-right" ] [ text <| Format.cents amount ]
                , td [] []
                ]

        chargeRow charge =
            footerRow "" charge.description charge.amount

        totalRow =
            if totals.total /= totals.subTotal then
                footerRow "font-weight-bold" "Total" totals.total

            else
                text ""

        formIsUnchanged =
            isFormUnchanged quantities items
    in
    if not (List.isEmpty items) then
        [ h1 [] [ text "Shopping Cart" ]
        , hr [] []
        , -- TODO: Remove when restoring checkout
          div [ class "alert alert-primary" ]
            [ p [] [ text "Dear Valued Customers," ]
            , p [] [ text "As the coronavirus pandemic has developed, Southern Exposure Seed Exchange has been inundated with orders for seeds. Our shipping is a few days behind. We are working as hard as we can to get seeds out in a timely way to our customers who want them." ]
            , p [] [ text "So as not to disappoint you with delayed shipping, we are currently not accepting new orders. As soon as we are caught up on our shipping backlog, we will be happy to take your order and process it promptly. We estimate this may occur by Monday afternoon, March 23. Please check back to this website over the next few days and see how we are doing." ]
            , p [] [ text "Of course we are grateful that so many customers value our seeds in these troubled times. We thank you for your patience as we deal with this temporary problem. If you prefer not to wait, you might consider ordering from one of the  fine seed companies listed on our homepage." ]
            ]
        , hr [] []
        , p [ class "text-center font-weight-bold" ]
            [ text <| "Total Items: " ++ String.fromInt itemCount ++ " Amount: " ++ Format.cents totals.total
            ]
        , form [ class "mb-4", onSubmit Submit ]
            [ cartTable
            , mobileCartTable form_ cartDetails totals

            -- TODO: Bring back checkout functinality
            -- , buttons
            ]
        ]

    else
        [ h1 [] [ text "Shopping Cart" ]
        , hr [] []
        , p [] [ text "You haven't added anything to your Shopping Cart yet!" ]
        ]


mobileCartTable : Form -> CartDetails -> { subTotal : Cents, total : Cents } -> Html Msg
mobileCartTable { quantities } { items, charges } totals =
    let
        cartRow { id, product, variant } =
            div [ class "cart-mobile-product row py-4" ]
                [ div [ class "col-4 mb-3" ]
                    [ a (routeLinkAttributes <| ProductDetails product.slug)
                        [ img
                            [ class "img-fluid"
                            , src <| imgSrcFallback product.image
                            , imageToSrcSet product.image
                            , productImageSizes
                            ]
                            []
                        ]
                    ]
                , div [ class "col-8 pl-0 mb-2" ]
                    [ a (routeLinkAttributes <| ProductDetails product.slug)
                        [ h5 [ class "text-body mb-0 product-name-lotsize" ]
                            [ rawHtml product.name ]
                        ]
                    , div [ class "small text-muted" ]
                        [ text <|
                            "Item #"
                                ++ product.baseSKU
                                ++ variant.skuSuffix
                                ++ (Maybe.map (\ls -> " - " ++ lotSizeToString ls) variant.lotSize
                                        |> Maybe.withDefault ""
                                   )
                        ]
                    , div [ class "text-danger" ]
                        [ text <| Format.cents <| variantPrice variant ]
                    , productInputs "d-none d-sm-flex d-md-none mt-2" id
                    ]
                , div [ class "col-12 d-sm-none" ]
                    [ productInputs "" id
                    ]
                ]

        productInputs class_ id =
            div [ class <| "row " ++ class_ ]
                [ div [ class "col-3 pr-0" ]
                    [ input
                        [ class "cart-quantity form-control"
                        , type_ "number"
                        , value <| String.fromInt <| Maybe.withDefault 1 <| Dict.get (fromCartItemId id) quantities
                        , onIntInput <| Quantity id
                        , A.min "1"
                        , A.step "1"
                        , numericInput
                        , A.size 5
                        ]
                        []
                    ]
                , div [ class "col-5" ]
                    [ button
                        [ class "btn btn-block btn-secondary"
                        , type_ "submit"
                        , disabled formIsUnchanged
                        ]
                        [ text "Update" ]
                    ]
                , div [ class "col-4 pl-0" ]
                    [ button
                        [ class "btn btn-block btn-danger"
                        , onClick <| Remove id
                        ]
                        [ text "Remove" ]
                    ]
                ]

        totalRows =
            List.concat
                [ if totals.subTotal /= totals.total then
                    total "Sub-Total" totals.subTotal

                  else
                    []
                , Maybe.map (.charge >> chargeTotal) charges.shippingMethod
                    |> Maybe.withDefault []
                , List.concatMap chargeTotal charges.surcharges
                , if charges.tax.amount /= Cents 0 then
                    chargeTotal charges.tax

                  else
                    []
                , total "Total" totals.total
                ]

        total name amount =
            [ div [ class "col-8 font-weight-bold" ]
                [ text <| name ++ ":" ]
            , div [ class "col-4 text-right" ]
                [ text <| Format.cents amount ]
            ]

        chargeTotal { description, amount } =
            total description amount

        formIsUnchanged =
            isFormUnchanged quantities items
    in
    div [ class "d-md-none" ]
        [ Keyed.node "div" [ class "cart-mobile-products mb-4" ] <|
            List.map (\i -> ( String.fromInt <| fromCartItemId i.id, cartRow i )) items
        , div [ class "row mb-4 " ] totalRows
        ]


productImageSizes : Html.Attribute msg
productImageSizes =
    A.attribute "sizes" <|
        String.join ", "
            [ "(max-width: 390px) 100px"
            , "(max-width: 767px) 165px"
            , "100px"
            ]



-- UTILS


fromCartItemId : CartItemId -> Int
fromCartItemId (CartItemId i) =
    i


changedQuantities : Dict Int Int -> List PageData.CartItem -> List ( String, Encode.Value )
changedQuantities quantities =
    List.foldl
        (\{ id, quantity } acc ->
            case Dict.get (fromCartItemId id) quantities of
                Nothing ->
                    acc

                Just formQuantity ->
                    if formQuantity /= quantity then
                        ( String.fromInt <| fromCartItemId id, Encode.int formQuantity )
                            :: acc

                    else
                        acc
        )
        []


isFormUnchanged : Dict Int Int -> List PageData.CartItem -> Bool
isFormUnchanged quantities items =
    changedQuantities quantities items
        |> List.isEmpty


encodedCartToken : Maybe String -> List ( String, Encode.Value )
encodedCartToken =
    Maybe.map (\token -> [ ( "sessionToken", Encode.string token ) ])
        >> Maybe.withDefault []
