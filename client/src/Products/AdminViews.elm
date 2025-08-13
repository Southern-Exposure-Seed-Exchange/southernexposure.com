module Products.AdminViews exposing
    ( EditForm
    , EditMsg
    , Form
    , ListForm
    , ListMsg
    , NewForm
    , NewMsg
    , editForm
    , formDecoder
    , initialEditForm
    , initialListForm
    , initialNewForm
    , list
    , new
    , updateEditForm
    , updateListForm
    , updateNewForm
    )

import Api
import Array exposing (Array)
import Category exposing (CategoryId(..))
import Dict
import File exposing (File)
import Html exposing (Html, a, br, button, div, fieldset, form, h3, hr, img, input, label, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes as A exposing (checked, class, download, for, href, id, name, required, selected, src, step, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, onSubmit, targetValue)
import Html.Extra exposing (viewIf)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Locations exposing (AddressLocations, Region(..), regionDecoder, regionEncoder)
import Models.Fields exposing (Cents(..), LotSize(..), centsDecoder, centsEncoder, centsToString, lotSizeDecoder, lotSizeEncoder, milligramsToGrams, milligramsToString)
import Models.Utils exposing (slugify)
import PageData
import Ports
import Product exposing (ProductId(..))
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Update.Utils exposing (noCommand, removeIndex, updateArray)
import Validation
import Views.Admin as Admin
import Views.HorizontalForm as Form
import Views.Images exposing (media)
import Views.Utils exposing (htmlOrBlank, routeLinkAttributes, selectImageFile)



-- LIST


type alias ListForm =
    { query : String
    , onlyActive : Bool
    , showDeleted : Bool
    }


initialListForm : ListForm
initialListForm =
    { query = ""
    , onlyActive = True
    , showDeleted = False
    }


type ListMsg
    = InputQuery String
    | InputOnlyActive Bool
    | InputShowDeleted Bool
    | RestoreProduct ProductId
    | RestoreResponse (WebData ())


updateListForm : Routing.Key -> ListMsg -> ListForm -> (ListForm, Cmd ListMsg)
updateListForm key msg model =
    case msg of
        InputQuery val ->
            noCommand { model | query = val }

        InputOnlyActive val ->
            noCommand { model | onlyActive = val }

        InputShowDeleted val ->
            noCommand { model | showDeleted = val }

        RestoreProduct productId ->
            (model, Api.post (Api.AdminRestoreDeletedProduct productId)
                |> Api.withJsonResponse (Decode.succeed ())
                |> Api.sendRequest RestoreResponse
            )

        RestoreResponse _ ->
            ( model, Routing.newUrl key <| Admin ProductList )

list : ListForm -> PageData.AdminProductListData -> List (Html ListMsg)
list listForm { products } =
    let
        renderProduct { id, name, baseSku, categories, isActive, isDeleted } =
            tr []
                [ td [] [ text <| (\(ProductId i) -> String.fromInt i) id ]
                , td [] [ text baseSku ]
                , td [] [ text name ]
                , td [] [ text <| String.join ", " categories ]
                , td [ class "text-center" ] [ Admin.activeIcon isActive ]
                , td []
                    [
                        if not isDeleted then
                            a (routeLinkAttributes <| Admin <| ProductEdit id)
                                [ text "Edit" ]
                        else
                            button [ onClick (RestoreProduct id) ] [ text "Restore" ]
                    ]
                ]

        onlyActiveInput =
            div [ class "flex-shrink-0 form-check form-check-inline" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , id "onlyActive"
                    , checked listForm.onlyActive
                    , onCheck InputOnlyActive
                    ]
                    []
                , label [ class "form-check-label", for "onlyActive" ]
                    [ text "Only Active Products" ]
                ]

        showDeletedInput =
            div [ class "flex-shrink-0 form-check form-check-inline" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , id "showDeleted"
                    , checked listForm.showDeleted
                    , onCheck InputShowDeleted
                    ]
                    []
                , label [ class "form-check-label", for "showDeleted" ]
                    [ text "Show Deleted Products" ]
                ]

        ( searchInput, filterProducts ) =
            Admin.searchInput InputQuery matchProduct listForm

        matchProduct p t =
            (iContains t p.name
                || iContains t p.baseSku
                || iContains t ((\(ProductId i) -> String.fromInt i) p.id)
                || List.any (iContains t) p.categories
            )
                && (p.isActive || not listForm.onlyActive)
                && (not p.isDeleted || listForm.showDeleted)

        iContains s1 s2 =
            String.contains s1 (String.toLower s2)
    in
    [ a (class "mb-2 btn btn-primary" :: (routeLinkAttributes <| Admin ProductNew))
        [ text "New Product" ]
    , div [ class "d-flex align-items-center justify-content-between mb-2" ]
        [ searchInput, onlyActiveInput, showDeletedInput ]
    , table [ class "table table-striped table-sm" ]
        [ thead []
            [ tr [ class "text-center" ]
                [ th [] [ text "ID" ]
                , th [] [ text "SKU" ]
                , th [] [ text "Name" ]
                , th [] [ text "Category" ]
                , th [] [ text "Active" ]
                , th [] []
                ]
            ]
        , tbody [] <| List.map renderProduct <| filterProducts products
        ]
    ]



-- NEW


type alias NewForm =
    Form


initialNewForm : NewForm
initialNewForm =
    initialForm


type NewMsg
    = NewFormMsg FormMsg
    | NewSubmit
    | NewSubmitResponse (WebData (Result Api.FormErrors ProductId))


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
    case msg of
        NewFormMsg subMsg ->
            updateForm key subMsg model
                |> Tuple.mapSecond (Cmd.map NewFormMsg)

        NewSubmit ->
            case validateForm model of
                Ok validVariants ->
                    ( { model | isSaving = True }
                    , Api.post Api.AdminNewProduct
                        |> Api.withJsonBody (encodeForm model validVariants Nothing)
                        |> Api.withErrorHandler Product.idDecoder
                        |> Api.sendRequest NewSubmitResponse
                    )

                Err errors ->
                    ( { model | errors = errors }
                    , Ports.scrollToErrorMessage
                    )

        NewSubmitResponse response ->
            case response of
                RemoteData.Success (Ok productId) ->
                    ( { model | isSaving = False }
                    , Routing.newUrl key <| Admin <| ProductEdit productId
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


new : NewForm -> PageData.AdminSharedProductData -> AddressLocations -> List (Html NewMsg)
new model data locations =
    [ formView "Add Product" NewSubmit NewFormMsg model data locations Nothing
    ]



-- EDIT


{-| Note: The productData & id fields are loaded in Main.update by the
GetAdminEditProductData message.
-}
type alias EditForm =
    { productData : WebData Form
    , id : Maybe ProductId
    }


initialEditForm : EditForm
initialEditForm =
    { productData = RemoteData.NotAsked
    , id = Nothing
    }


type EditMsg
    = EditFormMsg FormMsg
    | EditSubmit
    | EditSubmitResponse (WebData (Result Api.FormErrors ProductId))


updateEditForm : Routing.Key -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key msg model =
    case ( msg, model.productData ) of
        ( EditFormMsg subMsg, RemoteData.Success formData ) ->
            updateForm key subMsg formData
                |> Tuple.mapFirst (\f -> { model | productData = RemoteData.Success f })
                |> Tuple.mapSecond (Cmd.map EditFormMsg)

        ( EditFormMsg _, _ ) ->
            noCommand model

        ( EditSubmit, RemoteData.Success formData ) ->
            case validateForm formData of
                Ok validVariants ->
                    let
                        newData =
                            { formData | isSaving = True }
                    in
                    ( { model | productData = RemoteData.Success newData }
                    , Api.post Api.AdminEditProduct
                        |> Api.withJsonBody (encodeForm formData validVariants model.id)
                        |> Api.withErrorHandler Product.idDecoder
                        |> Api.sendRequest EditSubmitResponse
                    )

                Err errors ->
                    let
                        newData =
                            { formData | errors = errors }
                    in
                    ( { model | productData = RemoteData.Success newData }
                    , Ports.scrollToErrorMessage
                    )

        ( EditSubmit, _ ) ->
            noCommand model

        ( EditSubmitResponse response, RemoteData.Success formData ) ->
            let
                stoppedSaving =
                    { formData | isSaving = False }

                ( newData, cmd ) =
                    case response of
                        RemoteData.Success (Ok productId) ->
                            ( stoppedSaving
                            , Routing.newUrl key <| Admin <| ProductEdit productId
                            )

                        RemoteData.Success (Err errors) ->
                            ( { stoppedSaving | errors = errors }
                            , Ports.scrollToErrorMessage
                            )

                        RemoteData.Failure error ->
                            ( { stoppedSaving | errors = Api.apiFailureToError error }
                            , Ports.scrollToErrorMessage
                            )

                        _ ->
                            noCommand stoppedSaving
            in
            ( { model | productData = RemoteData.Success newData }, cmd )

        ( EditSubmitResponse _, _ ) ->
            noCommand model


editForm : EditForm -> PageData.AdminSharedProductData -> AddressLocations -> List (Html EditMsg)
editForm model productData locations =
    case model.productData of
        RemoteData.Success productForm ->
            [ formView "Update Product" EditSubmit EditFormMsg productForm productData locations model.id
            ]

        RemoteData.Failure error ->
            [ text "There was an error loading this Product's data. Please refresh the page or contact a developer."
            , Api.getErrorHtml "" <| Api.apiFailureToError error
            ]

        RemoteData.Loading ->
            [ text "Loading Product Data..." ]

        RemoteData.NotAsked ->
            [ text "You found a bug! Please inform the developer that the Edit Product form reached a 'NotAsked' state." ]



-- FORM


type alias Form =
    { name : String
    , slug : String
    , categories : Array CategoryId
    , baseSku : String
    , description : String
    , variants : Array Variant
    , imageName : String
    , imageData : String
    , isOrganic : Bool
    , isHeirloom : Bool
    , isSmallGrower : Bool
    , isRegional : Bool
    , keywords : String
    , shippingRestrictions : Array Region
    , errors : Api.FormErrors
    , isSaving : Bool
    , showDeleteConfirm : Bool

    -- ImageUrls is used for the Edit Form
    , imageUrls : Array String
    }


initialForm : Form
initialForm =
    { name = ""
    , slug = ""
    , categories = Array.fromList [ CategoryId 0 ]
    , baseSku = ""
    , description = ""
    , variants = Array.repeat 1 initialVariant
    , imageName = ""
    , imageData = ""
    , isOrganic = False
    , isHeirloom = False
    , isSmallGrower = False
    , isRegional = False
    , keywords = ""
    , shippingRestrictions = Array.empty
    , errors = Api.initialErrors
    , isSaving = False
    , showDeleteConfirm = False
    , imageUrls = Array.empty
    }


encodeForm : Form -> List ValidVariant -> Maybe ProductId -> Value
encodeForm model validVariants maybeProductId =
    let
        seedAttributeValues =
            [ model.isOrganic, model.isHeirloom, model.isRegional, model.isSmallGrower ]

        encodedSeedAttribues =
            if List.all ((==) False) seedAttributeValues then
                Encode.null

            else
                Encode.object
                    [ ( "organic", Encode.bool model.isOrganic )
                    , ( "heirloom", Encode.bool model.isHeirloom )
                    , ( "regional", Encode.bool model.isRegional )
                    , ( "smallGrower", Encode.bool model.isSmallGrower )
                    ]
    in
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "slug", Encode.string model.slug )
        , ( "categories", Encode.array Category.idEncoder model.categories )
        , ( "baseSku", Encode.string model.baseSku )
        , ( "longDescription", Encode.string model.description )
        , ( "imagesData", Encode.array
                (\(fileName, base64Image) ->
                    Encode.object
                        [ ( "fileName", Encode.string fileName )
                        , ( "base64Image", Encode.string base64Image )
                    ]
                ) (Array.fromList [ (model.imageName, model.imageData) ])
          )
        , ( "keywords", Encode.string model.keywords )
        , ( "shippingRestrictions", Encode.array regionEncoder model.shippingRestrictions )
        , ( "seedAttributes", encodedSeedAttribues )
        , ( "variants", Encode.list variantEncoder validVariants )
        , ( "id", Maybe.withDefault Encode.null <| Maybe.map Product.idEncoder maybeProductId )
        ]


formDecoder : Decoder Form
formDecoder =
    let
        fromAttribute field =
            Decode.required "seedAttributes" <|
                Decode.map (Maybe.withDefault False) <|
                    Decode.nullable <|
                        Decode.field field Decode.bool
    in
    Decode.succeed Form
        |> Decode.required "name" Decode.string
        |> Decode.required "slug" Decode.string
        |> Decode.required "categories" (Decode.array Category.idDecoder)
        |> Decode.required "baseSku" Decode.string
        |> Decode.required "longDescription" Decode.string
        |> Decode.required "variants" (Decode.array variantDecoder)
        |> Decode.hardcoded ""
        |> Decode.hardcoded ""
        |> fromAttribute "organic"
        |> fromAttribute "heirloom"
        |> fromAttribute "smallGrower"
        |> fromAttribute "regional"
        |> Decode.required "keywords" Decode.string
        |> Decode.required "shippingRestrictions" (Decode.array regionDecoder)
        |> Decode.hardcoded Api.initialErrors
        |> Decode.hardcoded False
        |> Decode.hardcoded False
        |> Decode.required "imageUrls" (Decode.array Decode.string)


type InventoryPolicy
    = RequireStock
    | AllowBackorder
    | Unlimited

inventoryPolicyToString : InventoryPolicy -> String
inventoryPolicyToString policy =
    case policy of
        RequireStock -> "requireStock"

        AllowBackorder -> "allowBackorder"

        Unlimited -> "unlimited"

decodeInventoryPolicy : Decoder String -> Decoder InventoryPolicy
decodeInventoryPolicy = Decode.andThen
    (\s ->
        case s of
            "requireStock" ->
                Decode.succeed RequireStock

            "allowBackorder" ->
                Decode.succeed AllowBackorder

            "unlimited" ->
                Decode.succeed Unlimited

            _ ->
                Decode.fail ("Unknown inventory policy: " ++ s)
    )

type alias Variant =
    { skuSuffix : String
    , price : String
    , quantity : String
    , lotSizeAmount : String
    , lotSizeSelector : LotSizeSelector
    , isActive : Bool
    , inventoryPolicy : InventoryPolicy
    , id : Maybe Int
    }


initialVariant : Variant
initialVariant =
    { skuSuffix = ""
    , price = ""
    , quantity = ""
    , lotSizeAmount = ""
    , lotSizeSelector = LSMass
    , isActive = True
    , inventoryPolicy = Unlimited
    , id = Nothing
    }


variantDecoder : Decoder Variant
variantDecoder =
    let
        sizeToAmount lotSize =
            case lotSize of
                Just (CustomLotSize s) ->
                    s

                Just (Mass mg) ->
                    milligramsToGrams mg

                Just (Bulbs i) ->
                    String.fromInt i

                Just (Slips i) ->
                    String.fromInt i

                Just (Plugs i) ->
                    String.fromInt i

                Nothing ->
                    ""

        sizeToSelector lotSize =
            case lotSize of
                Nothing ->
                    LSNone

                Just (CustomLotSize _) ->
                    LSCustom

                Just (Mass _) ->
                    LSMass

                Just (Bulbs _) ->
                    LSBulbs

                Just (Slips _) ->
                    LSSlips

                Just (Plugs _) ->
                    LSPlugs
    in
    Decode.map8 Variant
        (Decode.field "skuSuffix" Decode.string)
        (Decode.field "price" <| Decode.map centsToString centsDecoder)
        (Decode.field "quantity" <| Decode.map String.fromInt Decode.int)
        (Decode.field "lotSize" <| Decode.map sizeToAmount <| Decode.nullable lotSizeDecoder)
        (Decode.field "lotSize" <| Decode.map sizeToSelector <| Decode.nullable lotSizeDecoder)
        (Decode.field "isActive" Decode.bool)
        (Decode.field "inventoryPolicy" (decodeInventoryPolicy Decode.string))
        (Decode.field "id" <| Decode.nullable Decode.int)


type LotSizeSelector
    = LSMass
    | LSBulbs
    | LSSlips
    | LSPlugs
    | LSCustom
    | LSNone


type FormMsg
    = InputName String
    | InputSlug String
    | SelectCategory Int CategoryId
    | RemoveCategory Int
    | AddCategory
    | InputBaseSku String
    | InputDescription String
    | ToggleOrganic Bool
    | ToggleHeirloom Bool
    | ToggleSmallGrower Bool
    | ToggleRegional Bool
    | InputKeywords String
      -- Shipping Restrictions
    | AddState
    | SelectState Int Region
    | RemoveState Int
      -- Images
    | SelectImage
    | ImageUploaded File
    | ImageEncoded String
      -- Variant Forms
    | UpdateVariant Int VariantMsg
    | AddVariant
    | RemoveVariant Int
    | Delete ProductId
    | DeleteConfirmed ProductId
    | DeleteResponse (WebData ())


updateForm : Routing.Key -> FormMsg -> Form -> ( Form, Cmd FormMsg )
updateForm key msg model =
    case msg of
        InputName val ->
            noCommand <|
                if slugify model.name == model.slug then
                    { model | name = val, slug = slugify val }

                else
                    { model | name = val }

        InputSlug val ->
            noCommand { model | slug = val }

        SelectCategory index val ->
            noCommand
                { model
                    | categories = updateArray index (always val) model.categories
                }

        RemoveCategory index ->
            noCommand
                { model
                    | categories = removeIndex index model.categories
                }

        AddCategory ->
            noCommand
                { model | categories = Array.push (CategoryId 0) model.categories }

        InputBaseSku val ->
            noCommand { model | baseSku = val }

        InputDescription val ->
            noCommand { model | description = val }

        ToggleOrganic val ->
            noCommand { model | isOrganic = val }

        ToggleHeirloom val ->
            noCommand { model | isHeirloom = val }

        ToggleSmallGrower val ->
            noCommand { model | isSmallGrower = val }

        ToggleRegional val ->
            noCommand { model | isRegional = val }

        InputKeywords val ->
            noCommand { model | keywords = val }

        AddState ->
            { model | shippingRestrictions = Array.push (USState "AL") model.shippingRestrictions }
                |> noCommand

        SelectState index val ->
            { model
                | shippingRestrictions =
                    updateArray index (always val) model.shippingRestrictions
            }
                |> noCommand

        RemoveState index ->
            { model | shippingRestrictions = removeIndex index model.shippingRestrictions }
                |> noCommand

        SelectImage ->
            ( model, selectImageFile ImageUploaded )

        ImageUploaded imageFile ->
            ( { model | imageName = File.name imageFile }
            , Admin.encodeImageData ImageEncoded imageFile
            )

        ImageEncoded imageData ->
            noCommand { model | imageData = imageData }

        UpdateVariant index subMsg ->
            noCommand
                { model
                    | variants =
                        updateArray index (updateVariant subMsg) model.variants
                }

        AddVariant ->
            noCommand
                { model | variants = Array.push initialVariant model.variants }

        RemoveVariant index ->
            noCommand
                { model
                    | variants =
                        removeIndex index model.variants
                }

        Delete _ ->
            noCommand { model | showDeleteConfirm = True }

        DeleteConfirmed productId ->
            ( model
            , Api.delete (Api.AdminDeleteProduct productId)
                |> Api.withJsonResponse (Decode.succeed ())
                |> Api.sendRequest DeleteResponse
            )

        DeleteResponse response ->
            case response of
                RemoteData.Success () ->
                    ( initialForm
                    , Routing.newUrl key <| Admin <| ProductList
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error }
                    , Cmd.none
                    )

                _ ->
                    noCommand model



type VariantMsg
    = InputSkuSuffix String
    | InputPrice String
    | InputQuantity String
    | InputLotSizeAmount String
    | SelectLotSizeSelector LotSizeSelector
    | ToggleVariantIsActive Bool
    | SelectInventoryPolicy InventoryPolicy


updateVariant : VariantMsg -> Variant -> Variant
updateVariant msg model =
    case msg of
        InputSkuSuffix val ->
            { model | skuSuffix = String.toUpper val }

        InputPrice val ->
            { model | price = val }

        InputQuantity val ->
            { model | quantity = val }

        InputLotSizeAmount val ->
            { model | lotSizeAmount = val }

        SelectLotSizeSelector val ->
            { model | lotSizeSelector = val }

        ToggleVariantIsActive val ->
            { model | isActive = val }

        SelectInventoryPolicy val ->
            { model | inventoryPolicy = val }


{-| A validated variant has it's String inputs turned into the types expected
by the API.
-}
type alias ValidVariant =
    { skuSuffix : String
    , price : Cents
    , quantity : Int
    , lotSize : Maybe LotSize
    , isActive : Bool
    , inventoryPolicy : InventoryPolicy
    , id : Maybe Int
    }


variantEncoder : ValidVariant -> Value
variantEncoder variant =
    Encode.object
        [ ( "skuSuffix", Encode.string variant.skuSuffix )
        , ( "price", centsEncoder variant.price )
        , ( "quantity", Encode.int variant.quantity )
        , ( "lotSize"
          , Maybe.map lotSizeEncoder variant.lotSize
                |> Maybe.withDefault Encode.null
          )
        , ( "isActive", Encode.bool variant.isActive )
        , ( "inventoryPolicy"
          , Encode.string (inventoryPolicyToString variant.inventoryPolicy)
          )
        , ( "id"
          , Maybe.map Encode.int variant.id
                |> Maybe.withDefault Encode.null
          )
        ]


{-| Validate the Variant fields in the `Form`, returning a list of validated
variants or an error set.
-}
validateForm : Form -> Result Api.FormErrors (List ValidVariant)
validateForm model =
    Array.toList model.variants
        |> Validation.indexedValidation "variant" validateVariant


validateVariant : Variant -> Validation.FormValidation ValidVariant
validateVariant ({ lotSizeAmount } as variant) =
    let
        validateLotSize : Validation.Validation (Maybe LotSize)
        validateLotSize =
            case variant.lotSizeSelector of
                LSCustom ->
                    Validation.succeed <| Just <| CustomLotSize lotSizeAmount

                LSMass ->
                    Validation.milligrams lotSizeAmount
                        |> Validation.map (Just << Mass)

                LSBulbs ->
                    validateInt Bulbs lotSizeAmount

                LSSlips ->
                    validateInt Slips lotSizeAmount

                LSPlugs ->
                    validateInt Plugs lotSizeAmount

                LSNone ->
                    Validation.succeed Nothing

        validateInt mapper =
            Validation.int >> Validation.map (Just << mapper)
    in
    Validation.formValidation
        (\price quantity lotSize ->
            { skuSuffix = variant.skuSuffix
            , isActive = variant.isActive
            , id = variant.id
            , price = price
            , quantity = quantity
            , lotSize = lotSize
            , inventoryPolicy = variant.inventoryPolicy
            }
        )
        |> Validation.apply "price" (Validation.cents variant.price)
        |> Validation.apply "quantity" (Validation.int variant.quantity)
        |> Validation.apply "lotSize" validateLotSize


{-| Render the form for updating/creating Products.
-}
formView : String -> msg -> (FormMsg -> msg) -> Form -> PageData.AdminSharedProductData -> AddressLocations -> Maybe ProductId -> Html msg
formView buttonText submitMsg msgWrapper model { categories } locations id =
    let
        inputRow s =
            Form.inputRow model.errors (s model)
        imageUrl = List.head (Array.toList model.imageUrls) |> Maybe.withDefault ""

        existingImage =
            if not <| String.isEmpty imageUrl then
                Form.withLabel "Current Image" True <|
                    [ div [ class "image-preview mb-3" ]
                        [ img
                            [ class "img-fluid"
                            , src <| media <| "products/originals/" ++ imageUrl
                            ]
                            []
                        ]
                    ]

            else
                text ""
        deleteAction i =
            if model.showDeleteConfirm then
                DeleteConfirmed i
            else
                Delete i
        deleteWarningText = viewIf model.showDeleteConfirm <|
            div [ class "text-danger" ]
                    [ text "Are you sure? This will completely delete the current product and all its variants."
                    , br [] []
                    , text "Click 'Delete Product' again to confirm."
                    ]
    in
    form [ class <| Admin.formSavingClass model, onSubmit submitMsg ] <|
        List.map (Html.map msgWrapper)
            [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
            , Api.generalFormErrors model
            , h3 [] [ text "Base Product" ]
            , inputRow .name InputName True "Name" "name" "text" "off"
            , inputRow .slug InputSlug True "Slug" "slug" "text" "off"
            , Admin.categorySelects True SelectCategory AddCategory RemoveCategory model categories
            , inputRow .baseSku InputBaseSku True "Base SKU" "baseSku" "text" "off"
            , Form.textareaRow model.errors model.description InputDescription False "Description" "description" 10
            , inputRow .keywords InputKeywords False "Search Keywords" "keywords" "text" "off"
            , shippingRestrictedSelects model.errors locations model.shippingRestrictions
            , Form.checkboxRow model.isOrganic ToggleOrganic "Is Organic" "isOrganic"
            , Form.checkboxRow model.isHeirloom ToggleHeirloom "Is Heirloom" "isHeirloom"
            , Form.checkboxRow model.isSmallGrower ToggleSmallGrower "Is Small Grower" "isSmallGrower"
            , Form.checkboxRow model.isRegional ToggleRegional "Is SouthEast" "isSouthEast"
            , existingImage
            , Admin.imageSelectRow model.imageName model.imageData SelectImage "Image"
            , h3 [] [ text "Variants" ]
            , div [] <|
                List.intersperse (hr [] []) <|
                    Array.toList <|
                        Array.indexedMap (variantForm model.errors) model.variants
            , deleteWarningText
            , div [ class "form-group mb-4" ]
                [ Admin.submitOrSavingButton model buttonText
                , button
                    [ class "ml-3 btn btn-secondary"
                    , type_ "button"
                    , onClick AddVariant
                    ]
                    [ text "Add Variant" ]
                , htmlOrBlank
                    (\(ProductId i) -> a
                        [ class "ml-3 btn btn-secondary"
                        , type_ "button"
                        , href ("/api/admin/products/export/" ++ String.fromInt i)
                        , download ("product-" ++ String.fromInt i ++ "-export.csv")
                        ]
                        [ text "Export as CSV" ]
                    ) id
                , htmlOrBlank
                    (\i -> button
                        [ class "ml-3 btn btn-danger"
                        , type_ "button"
                        , onClick <| deleteAction i
                        ]
                        [ text "Delete Product" ]
                    ) id
                ]
            ]


shippingRestrictedSelects : Api.FormErrors -> AddressLocations -> Array Region -> Html FormMsg
shippingRestrictedSelects errors locations selectedRegions =
    let
        toValue r =
            case r of
                USState c ->
                    "u:" ++ c

                ArmedForces c ->
                    "a:" ++ c

                CAProvince c ->
                    "c:" ++ c

                Custom str ->
                    str

        fromValue str =
            case String.split ":" str of
                [ "u", c ] ->
                    Ok <| USState c

                [ "a", c ] ->
                    Ok <| ArmedForces c

                _ ->
                    Err <| "Expected a USState or ArmedForces: " ++ str

        items =
            List.map (toItem USState) locations.states
                ++ List.map (toItem ArmedForces) locations.armedForces

        toItem region { code, name } =
            { name = name, value = region code }
    in
    { isRequired = False
    , selectMsg = SelectState
    , addMsg = AddState
    , removeMsg = RemoveState
    , errors = errors
    , selected = selectedRegions
    , items = items
    , toValue = toValue
    , fromValue = fromValue
    , blankOption = Admin.MultiSelectItem "Select a State" (USState "")
    , prefix = "shipping-restrictions"
    , label = "Cannot Ship To"
    , addLabel = "State"
    }
        |> Admin.multiSelect


{-| Render the sub-form for ProductVariants.
-}
variantForm : Api.FormErrors -> Int -> Variant -> Html FormMsg
variantForm errors index variant =
    let
        fieldName n =
            "variant-" ++ String.fromInt index ++ "-" ++ n

        variantInput s m r l n t =
            Form.inputRow errors
                (s variant)
                (UpdateVariant index << m)
                r
                l
                (fieldName n)
                t
                "off"

        removeButton =
            if variant.id == Nothing then
                div [ class "text-right form-group" ]
                    [ button
                        [ class "btn btn-danger"
                        , type_ "button"
                        , onClick <| RemoveVariant index
                        ]
                        [ text "Remove Variant" ]
                    ]

            else
                text ""
    in
    fieldset [ class "form-group" ]
        [ variantInput .skuSuffix InputSkuSuffix False "SKU Suffix" "skuSuffix" "text"
        , variantInput .price InputPrice True "Price" "price" "text"
        , variantInput .quantity InputQuantity True "Quantity" "quantity" "number"
        , lotSizeRow errors index variant.lotSizeSelector variant.lotSizeAmount
        , Form.checkboxRow variant.isActive
            (UpdateVariant index << ToggleVariantIsActive)
            "Is Enabled"
            (fieldName "isEnabled")
        , inventoryPolicyRow index variant.inventoryPolicy
        , removeButton
        ]

inventoryPolicyRow : Int -> InventoryPolicy -> Html FormMsg
inventoryPolicyRow index selectedPolicy =
    let
        options = [ RequireStock, AllowBackorder, Unlimited ]
            |> List.map
                (\p ->
                    option
                        [ value <| inventoryPolicyToString p
                        , selected <| p == selectedPolicy
                        ]
                        [ text <| case p of
                            RequireStock -> "Require Stock"
                            AllowBackorder -> "Allow Backorder"
                            Unlimited -> "Unlimited"
                        ]
                )
    in
    Form.withLabel "Inventory Policy"
        True
        [ select
            [ id <| "input-inventoryPolicy"
            , name <| "variant-inventoryPolicy"
            , targetValue |> decodeInventoryPolicy |> Decode.map (UpdateVariant index << SelectInventoryPolicy) |> on "change"
            , class "form-control w-25 d-inline-block"
            ]
            options
        ]


{-| Render the Form row for LotSize selection, with an input for the amount &
dropdown for the label.
-}
lotSizeRow : Api.FormErrors -> Int -> LotSizeSelector -> String -> Html FormMsg
lotSizeRow errors index selectedType enteredAmount =
    let
        fieldErrors =
            Dict.get ("variant-" ++ String.fromInt index ++ "-lotsize") errors
                |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]

        amountId =
            "LotSizeAmount"

        inputAttrs =
            case selectedType of
                LSNone ->
                    [ type_ "hidden" ]

                LSCustom ->
                    [ type_ "text" ]

                LSMass ->
                    [ type_ "number", A.min "0.001", step "0.001" ]

                _ ->
                    [ type_ "number", A.min "1", step "1" ]

        selectId =
            "LotSizeSelector"

        onSelect =
            targetValue
                |> Decode.andThen sizeDecoder
                |> Decode.map (UpdateVariant index << SelectLotSizeSelector)
                |> on "change"

        sizeToValue size =
            case size of
                LSCustom ->
                    "custom"

                LSMass ->
                    "mass"

                LSBulbs ->
                    "bulbs"

                LSSlips ->
                    "slips"

                LSPlugs ->
                    "plugs"

                LSNone ->
                    "none"

        sizeToString size =
            case size of
                LSCustom ->
                    "Custom"

                LSMass ->
                    "Mass (g)"

                LSBulbs ->
                    "Bulbs"

                LSSlips ->
                    "Slips"

                LSPlugs ->
                    "Plugs"

                LSNone ->
                    "No Lot Size"

        sizeDecoder str =
            case str of
                "custom" ->
                    Decode.succeed LSCustom

                "mass" ->
                    Decode.succeed LSMass

                "bulbs" ->
                    Decode.succeed LSBulbs

                "slips" ->
                    Decode.succeed LSSlips

                "plugs" ->
                    Decode.succeed LSPlugs

                "none" ->
                    Decode.succeed LSNone

                _ ->
                    Decode.fail <| "Unrecognized lot size type: " ++ str

        options =
            [ LSMass, LSBulbs, LSSlips, LSPlugs, LSCustom, LSNone ]
                |> List.map
                    (\t ->
                        option
                            [ value <| sizeToValue t
                            , selected <| t == selectedType
                            ]
                            [ text <| sizeToString t ]
                    )

        lotSizePreview =
            if selectedType == LSMass then
                massPreview

            else
                text ""

        massPreview =
            case Validation.milligrams enteredAmount of
                Ok mg ->
                    div [ class "d-inline-block ml-4 text-muted" ]
                        [ text "Will be shown as: "
                        , text <| milligramsToString mg
                        ]

                Err _ ->
                    text ""
    in
    Form.withLabel "Lot Size"
        True
        [ input
            ([ id <| "input" ++ amountId
             , name amountId
             , required True
             , value enteredAmount
             , onInput (UpdateVariant index << InputLotSizeAmount)
             , class "form-control w-50 d-inline-block mr-4"
             ]
                ++ inputAttrs
            )
            []
        , select
            [ id <| "input" ++ selectId
            , name selectId
            , onSelect
            , class "form-control w-25 d-inline-block"
            ]
            options
        , lotSizePreview
        , errorHtml
        ]
