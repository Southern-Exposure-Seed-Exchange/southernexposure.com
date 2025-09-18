module Components.Admin.ProductSalesAdmin exposing
    ( EditForm
    , EditMsg
    , NewForm
    , NewMsg
    , edit
    , initialEditForm
    , initialNewForm
    , list
    , new
    , updateEditForm
    , updateNewForm
    )

import Components.Admin.Admin as Admin exposing (updateEditField)
import Components.HorizontalForm as Form
import Data.Api as Api
import Data.Fields exposing (Cents, centsEncoder, centsToString, lotSizeToString)
import Data.PageData exposing (AdminEditProductSaleData, AdminProductSaleListData, AdminProductSaleNewData, SaleProductData)
import Data.Product as Product exposing (ProductVariantId(..))
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Data.Validation as Validation exposing (formValidation)
import Dict
import Html exposing (Html, a, div, form, option, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Time exposing (Posix, Zone)
import Utils.Format as Format
import Utils.Update exposing (noCommand)
import Utils.Utils exposing (posixToDateString)
import Utils.View exposing (routeLinkAttributes)



-- LIST


list : Zone -> AdminProductSaleListData -> List (Html msg)
list zone { sales, variants } =
    let
        renderSale { id, variant, price, start, end } =
            let
                (ProductVariantId rawVariantId) =
                    variant

                ( sku, name, active ) =
                    case Dict.get rawVariantId variants of
                        Nothing ->
                            ( "<no data>", text "<no data>", False )

                        Just variantData ->
                            ( variantData.sku
                            , Product.nameWithLotSize variantData variantData
                            , variantData.isActive
                            )
            in
            tr []
                [ td [] [ text sku ]
                , td [] [ name ]
                , td [ class "text-center" ] [ Admin.activeIcon active ]
                , td [ class "text-right" ] [ text <| Format.cents price ]
                , td [ class "text-center" ] [ text <| Format.date zone start ]
                , td [ class "text-center" ] [ text <| Format.date zone end ]
                , td [] [ a (routeLinkAttributes <| Admin <| ProductSaleEdit id) [ text "Edit" ] ]
                ]
    in
    [ div [ class "form-group mb-4" ]
        [ a (class "btn btn-primary" :: (routeLinkAttributes <| Admin ProductSaleNew))
            [ text "New Product Sale" ]
        ]
    , table [ class "table table-striped table-sm" ]
        [ thead []
            [ tr []
                [ th [] [ text "SKU" ]
                , th [] [ text "Name" ]
                , th [ class "text-center" ] [ text "Product Active" ]
                , th [ class "text-right" ] [ text "Sale Price" ]
                , th [ class "text-center" ] [ text "Start Date" ]
                , th [ class "text-center" ] [ text "End Date" ]
                , th [] []
                ]
            ]
        , tbody [] <| List.map renderSale sales
        ]
    ]



-- NEW


type alias NewForm =
    { variant : ProductVariantId
    , price : String
    , start : String
    , end : String
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialNewForm : NewForm
initialNewForm =
    { variant = ProductVariantId 0
    , price = ""
    , start = ""
    , end = ""
    , errors = Api.initialErrors
    , isSaving = False
    }


type alias ValidNewForm =
    { variant : ProductVariantId
    , price : Cents
    , start : Posix
    , end : Posix
    }


validateNewForm : NewForm -> Result Api.FormErrors ValidNewForm
validateNewForm model =
    let
        validateVariantId ((ProductVariantId i) as pv) =
            if i == 0 then
                Err "Please select a Product."

            else
                Ok pv
    in
    formValidation ValidNewForm
        |> Validation.apply "variant" (validateVariantId model.variant)
        |> Validation.apply "price" (Validation.cents model.price)
        |> Validation.apply "start" (Validation.date model.start)
        |> Validation.apply "end" (Validation.date model.end)


encodeNewForm : ValidNewForm -> Value
encodeNewForm model =
    Encode.object
        [ ( "variant", (\(ProductVariantId i) -> Encode.int i) model.variant )
        , ( "price", centsEncoder model.price )
        , ( "start", Iso8601.encode model.start )
        , ( "end", Iso8601.encode model.end )
        ]


type NewMsg
    = NewSelectVariant ProductVariantId
    | NewInputPrice String
    | NewInputStart String
    | NewInputEnd String
    | NewSubmit
    | NewSubmitResponse (WebData (Result Api.FormErrors Int))


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
    case msg of
        NewSelectVariant v ->
            { model | variant = v } |> noCommand

        NewInputPrice v ->
            { model | price = v } |> noCommand

        NewInputStart v ->
            { model | start = v } |> noCommand

        NewInputEnd v ->
            { model | end = v } |> noCommand

        NewSubmit ->
            case validateNewForm model of
                Err errors ->
                    ( { model | errors = errors }
                    , Ports.scrollToErrorMessage
                    )

                Ok validatedModel ->
                    ( { model | errors = Api.initialErrors, isSaving = True }
                    , Api.post Api.AdminProductSaleNew
                        |> Api.withJsonBody (encodeNewForm validatedModel)
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest NewSubmitResponse
                    )

        NewSubmitResponse resp ->
            case resp of
                RemoteData.Success (Ok saleId) ->
                    ( initialNewForm
                    , Routing.newUrl key <| Admin <| ProductSaleEdit saleId
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | isSaving = False, errors = errors }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure err ->
                    ( { model | isSaving = False, errors = Api.apiFailureToError err }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    ( { model | isSaving = False }, Cmd.none )


new : NewForm -> AdminProductSaleNewData -> List (Html NewMsg)
new model variants =
    [ form [ class <| Admin.formSavingClass model, onSubmit NewSubmit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , variantSelect model.variant NewSelectVariant variants
        , normalPriceRow model.variant variants
        , Form.inputRow model.errors model.price NewInputPrice True "Sale Price" "price" "text" "off"
        , Form.dateRow model.errors model.start NewInputStart True "Start Date" "start"
        , Form.dateRow model.errors model.end NewInputEnd True "End Date" "end"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Add Sale" ]
        ]
    ]


variantSelect : ProductVariantId -> (ProductVariantId -> msg) -> List SaleProductData -> Html msg
variantSelect variant msg variants =
    let
        idParser val =
            case String.toInt val of
                Just i ->
                    Ok <| ProductVariantId i

                Nothing ->
                    Err <| "Could not parse Product Variant ID: " ++ val

        blankOption =
            option [ value "0", selected True ] [ text "Select a Product" ]

        options =
            List.sortBy .sku variants
                |> List.map
                    (\v ->
                        option
                            [ value <| String.fromInt v.id
                            , selected <| ProductVariantId v.id == variant
                            ]
                            [ text <| makeVariantName v ]
                    )
                |> (\opts ->
                        if variant == ProductVariantId 0 then
                            blankOption :: opts

                        else
                            opts
                   )

        makeVariantName { sku, name, lotSize } =
            String.join " - " <|
                List.filterMap identity
                    [ Just sku
                    , Just name
                    , Maybe.map lotSizeToString lotSize
                    ]
    in
    Form.selectRow idParser msg "Product" True options



-- EDIT


type alias EditForm =
    { price : Maybe String
    , variant : Maybe ProductVariantId
    , start : Maybe String
    , end : Maybe String
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialEditForm : EditForm
initialEditForm =
    { price = Nothing
    , variant = Nothing
    , start = Nothing
    , end = Nothing
    , errors = Api.initialErrors
    , isSaving = False
    }


type alias ValidEditForm =
    { id : Int
    , price : Maybe Cents
    , variant : Maybe ProductVariantId
    , start : Maybe Posix
    , end : Maybe Posix
    }


validateEditForm : Int -> EditForm -> Result Api.FormErrors ValidEditForm
validateEditForm saleId model =
    formValidation
        (\price start end ->
            { id = saleId
            , price = price
            , start = start
            , end = end
            , variant = model.variant
            }
        )
        |> Validation.applyMaybe "price" Validation.cents model.price
        |> Validation.applyMaybe "start" Validation.date model.start
        |> Validation.applyMaybe "end" Validation.date model.end


encodeEditForm : ValidEditForm -> Value
encodeEditForm model =
    let
        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "price", encodeMaybe centsEncoder model.price )
        , ( "variant", encodeMaybe (\(ProductVariantId i) -> Encode.int i) model.variant )
        , ( "start", encodeMaybe Iso8601.encode model.start )
        , ( "end", encodeMaybe Iso8601.encode model.end )
        ]


type EditMsg
    = EditSelectVariant ProductVariantId
    | EditInputPrice String
    | EditInputStart String
    | EditInputEnd String
    | EditSubmit
    | EditSubmitResponse (WebData (Result Api.FormErrors ()))


updateEditForm : Routing.Key -> WebData AdminEditProductSaleData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    let
        sale =
            RemoteData.map .sale original
    in
    case msg of
        EditSelectVariant val ->
            noCommand <| { model | variant = Just val }

        EditInputPrice val ->
            noCommand <|
                updateEditField val sale (.price >> centsToString) <|
                    \v -> { model | price = v }

        EditInputStart val ->
            noCommand <|
                updateEditField val sale (.start >> posixToDateString) <|
                    \v -> { model | start = v }

        EditInputEnd val ->
            noCommand <|
                updateEditField val sale (.end >> posixToDateString) <|
                    \v -> { model | end = v }

        EditSubmit ->
            case RemoteData.map .id sale |> RemoteData.toMaybe of
                Nothing ->
                    noCommand model

                Just saleId ->
                    case validateEditForm saleId model of
                        Err e ->
                            ( { model | errors = e }
                            , Ports.scrollToErrorMessage
                            )

                        Ok validForm ->
                            ( { model | errors = Api.initialErrors, isSaving = True }
                            , Api.patch Api.AdminEditProductSale
                                |> Api.withJsonBody (encodeEditForm validForm)
                                |> Api.withErrorHandler (Decode.succeed ())
                                |> Api.sendRequest EditSubmitResponse
                            )

        EditSubmitResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin ProductSaleList
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand { model | isSaving = False }


edit : EditForm -> AdminEditProductSaleData -> List (Html EditMsg)
edit model { sale, variants } =
    let
        valueWithFallback s1 s2 =
            s1 model |> Maybe.withDefault (s2 sale)

        inputRow s1 s2 =
            Form.inputRow model.errors (valueWithFallback s1 s2)
    in
    [ form [ class (Admin.formSavingClass model), onSubmit EditSubmit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , variantSelect (valueWithFallback .variant .variant) EditSelectVariant variants
        , normalPriceRow (valueWithFallback .variant .variant) variants
        , inputRow .price (.price >> centsToString) EditInputPrice True "Sale Price" "price" "text" "off"
        , Form.dateRow model.errors
            (valueWithFallback .start (.start >> posixToDateString))
            EditInputStart
            True
            "Start Date"
            "start"
        , Form.dateRow model.errors
            (valueWithFallback .end (.end >> posixToDateString))
            EditInputEnd
            True
            "End Date"
            "end"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Update Sale" ]
        ]
    ]



-- UTILS


{-| Render a form row showing the normal price of the selected variant.
-}
normalPriceRow : ProductVariantId -> List SaleProductData -> Html msg
normalPriceRow (ProductVariantId selectedId) variants =
    List.filter (\v -> v.id == selectedId) variants
        |> List.head
        |> Maybe.map (.price >> Format.cents)
        |> Maybe.withDefault "Select a Product"
        |> text
        |> List.singleton
        |> Form.withLabel "Normal Price" True
