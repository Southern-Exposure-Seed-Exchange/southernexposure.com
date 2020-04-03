module Views.ProductSalesAdmin exposing (NewForm, NewMsg, initialNewForm, list, new, updateNewForm)

import Api
import Dict
import Html exposing (Html, a, div, form, option, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Models.Fields exposing (Cents, centsEncoder, lotSizeToString)
import PageData exposing (AdminProductSaleListData, AdminProductSaleNewData, SaleProductData)
import Ports
import Product exposing (ProductVariantId(..))
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Posix, Zone)
import Update.Utils exposing (noCommand)
import Validation exposing (formValidation)
import Views.Admin as Admin
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Utils exposing (routeLinkAttributes)



-- LIST


list : Zone -> AdminProductSaleListData -> List (Html msg)
list zone { sales, variants } =
    let
        renderSale { variant, price, start, end } =
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

                -- TODO: Make Link
                , td [] [ text "Edit" ]
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
                , td [] []
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


updateNewForm : NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm msg model =
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
                RemoteData.Success (Ok _) ->
                    ( initialNewForm
                      -- TODO: Redirect to Edit Page
                    , Cmd.none
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
    let
        selectedVariant =
            List.filter (\v -> v.id == (\(ProductVariantId i) -> i) model.variant) variants |> List.head

        priceRow =
            selectedVariant
                |> Maybe.map (.price >> Format.cents)
                |> Maybe.withDefault "Select a Product"
                |> text
                |> List.singleton
                |> Form.withLabel "Normal Price" True
    in
    [ form [ class <| Admin.formSavingClass model, onSubmit NewSubmit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , variantSelect model.variant NewSelectVariant variants
        , priceRow
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
