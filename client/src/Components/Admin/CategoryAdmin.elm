module Components.Admin.CategoryAdmin exposing
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

import Components.Admin.Admin as Admin exposing (equalsOriginal, formSavingClass, slugFrom, submitOrSavingButton, updateEditField)
import Components.HorizontalForm as Form
import Data.Api as Api exposing (FormErrors)
import Data.Category as Category exposing (CategoryId(..))
import Data.PageData as PageData
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Dict
import File exposing (File)
import Html exposing (Html, a, button, div, form, img, option, table, tbody, td, text, tr)
import Html.Attributes exposing (class, selected, src, style, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import RemoteData
import Result.Extra as Result
import Utils.Update exposing (noCommand)
import Utils.Utils exposing (slugify)
import Utils.View exposing (routeLinkAttributes, selectImageFile)


list : PageData.AdminCategoryListData -> List (Html msg)
list { roots } =
    let
        depthPadding depth =
            style "margin-left" <| String.fromInt (depth * 20 + 10) ++ "px"

        renderCategory depth (PageData.AdminListCategory c) =
            tr []
                [ td [] [ div [ depthPadding depth ] [ text c.name ] ]
                , td [] [ a (routeLinkAttributes <| Admin <| CategoryEdit c.id) [ text "Edit" ] ]
                ]
                :: List.concatMap (renderCategory <| depth + 1) c.children
    in
    [ a (class "mb-3 btn btn-primary" :: routeLinkAttributes (Admin CategoryNew))
        [ text "New Category" ]
    , table [ class "table table-sm table-striped" ]
        [ tbody [] <|
            List.concatMap (renderCategory 0) roots
        ]
    ]



-- NEW


type NewMsg
    = NInputName String
    | NInputSlug String
    | NInputParent (Maybe CategoryId)
    | NInputDescription String
    | NInputOrder String
    | NSelectImage
    | NImageUploaded File
    | NImageEncoded String
    | SubmitNew
    | SubmitNewResponse (RemoteData.WebData (Result FormErrors Int))


type alias NewForm =
    { name : String
    , slug : String
    , parent : Maybe CategoryId
    , description : String
    , order : String
    , imageName : String
    , imageData : String
    , errors : FormErrors
    , isSaving : Bool
    }


initialNewForm : NewForm
initialNewForm =
    { name = ""
    , slug = ""
    , parent = Nothing
    , description = ""
    , order = "1"
    , imageName = ""
    , imageData = ""
    , errors = Api.initialErrors
    , isSaving = False
    }


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
    case msg of
        NInputName val ->
            noCommand <|
                if model.slug == slugify model.name then
                    { model | name = val, slug = slugify val }

                else
                    { model | name = val }

        NInputSlug val ->
            { model | slug = val }
                |> noCommand

        NInputParent val ->
            { model | parent = val }
                |> noCommand

        NInputDescription val ->
            { model | description = val }
                |> noCommand

        NInputOrder val ->
            { model | order = val }
                |> noCommand

        NSelectImage ->
            ( model, selectImageFile NImageUploaded )

        NImageUploaded imageFile ->
            ( { model | imageName = File.name imageFile }
            , Admin.encodeImageData NImageEncoded imageFile
            )

        NImageEncoded imageData ->
            { model | imageData = imageData }
                |> noCommand

        SubmitNew ->
            case String.toInt model.order of
                Nothing ->
                    { model | errors = Dict.fromList [ ( "order", [ "Value must be a whole number." ] ) ] }
                        |> noCommand

                Just order ->
                    let
                        encodedParent =
                            model.parent
                                |> encodeMaybe (\(CategoryId i) -> Encode.int i)

                        jsonBody =
                            Encode.object
                                [ ( "name", Encode.string model.name )
                                , ( "slug", Encode.string model.slug )
                                , ( "parentId", encodedParent )
                                , ( "description", Encode.string model.description )
                                , ( "imageData", Encode.string model.imageData )
                                , ( "imageName", Encode.string model.imageName )
                                , ( "order", Encode.int order )
                                ]
                    in
                    ( { model | isSaving = True }
                    , Api.post Api.AdminNewCategory
                        |> Api.withJsonBody jsonBody
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest SubmitNewResponse
                    )

        SubmitNewResponse response ->
            case response of
                RemoteData.Success (Ok cId) ->
                    ( initialNewForm
                    , Routing.newUrl key <| Admin <| CategoryEdit <| CategoryId cId
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
                    noCommand model


new : NewForm -> PageData.AdminNewCategoryData -> List (Html NewMsg)
new model categories =
    let
        inputRow =
            Form.inputRow model.errors

        renderCategoryOption { id, name } =
            option
                [ value <| (\(CategoryId i) -> String.fromInt i) id
                , selected <| Just id == model.parent
                ]
                [ text name ]

        blankOption =
            option
                [ value "", selected <| Nothing == model.parent ]
                [ text "" ]
    in
    [ form [ onSubmit SubmitNew, class (formSavingClass model) ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow model.name NInputName True "Name" "name" "text" "off"
        , inputRow model.slug NInputSlug True "Slug" "slug" "text" "off"
        , Form.selectRow Category.maybeIdParser NInputParent "Parent Category" True <|
            blankOption
                :: List.map renderCategoryOption categories
        , Form.textareaRow model.errors model.description NInputDescription False "Description" "description" 10
        , inputRow model.order NInputOrder True "Sort Order" "order" "number" "off"
        , Admin.imageSelectRow model.imageName model.imageData NSelectImage "Image"
        , div [ class "form-group" ]
            [ submitOrSavingButton model "Add Category"
            ]
        ]
    ]



-- EDIT


type EditMsg
    = EInputName String
    | EInputSlug String
    | EInputParent (Maybe CategoryId)
    | EInputDescription String
    | EInputOrder String
    | ESelectImage
    | EImageUploaded File
    | EImageEncoded String
    | SubmitEdit
    | SubmitEditResponse (RemoteData.WebData (Result FormErrors ()))


type alias EditForm =
    { name : Maybe String
    , slug : Maybe String
    , parent : Result () (Maybe CategoryId) -- Err = no change, Ok = new value
    , description : Maybe String
    , order : Maybe String
    , imageName : Maybe String
    , imageData : Maybe String
    , errors : FormErrors
    , isSaving : Bool
    }


initialEditForm : EditForm
initialEditForm =
    { name = Nothing
    , slug = Nothing
    , parent = Err ()
    , description = Nothing
    , order = Nothing
    , imageName = Nothing
    , imageData = Nothing
    , errors = Api.initialErrors
    , isSaving = False
    }


updateEditForm : Routing.Key -> RemoteData.WebData PageData.AdminEditCategoryData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    case msg of
        EInputName val ->
            noCommand <|
                if equalsOriginal val original .name then
                    if slugFromName model original then
                        { model | name = Nothing, slug = Nothing }

                    else
                        { model | name = Nothing }

                else if slugFromName model original then
                    { model | name = Just val, slug = Just (slugify val) }

                else
                    { model | name = Just val }

        EInputSlug val ->
            noCommand <|
                updateEditField val original .slug <|
                    \v -> { model | slug = v }

        EInputParent val ->
            noCommand <|
                updateEditField val original .parent <|
                    \v ->
                        case v of
                            Nothing ->
                                { model | parent = Err () }

                            Just newParent ->
                                { model | parent = Ok newParent }

        EInputDescription val ->
            noCommand <|
                updateEditField val original .description <|
                    \v -> { model | description = v }

        EInputOrder val ->
            noCommand <|
                updateEditField val original (String.fromInt << .order) <|
                    \v -> { model | order = v }

        ESelectImage ->
            ( model, selectImageFile EImageUploaded )

        EImageUploaded imageFile ->
            ( { model | imageName = Just <| File.name imageFile }
            , Admin.encodeImageData EImageEncoded imageFile
            )

        EImageEncoded imageData ->
            { model | imageData = Just imageData }
                |> noCommand

        SubmitEdit ->
            case ( Maybe.map String.toInt model.order, RemoteData.toMaybe original |> Maybe.map .id ) of
                ( Just Nothing, _ ) ->
                    { model | errors = Dict.fromList [ ( "order", [ "Value must be a whole number." ] ) ] }
                        |> noCommand

                ( _, Just categoryId ) ->
                    let
                        encodedParent =
                            Result.unwrap []
                                (\maybeId ->
                                    [ ( "parentId"
                                      , encodeMaybe (\(CategoryId i) -> Encode.int i) maybeId
                                      )
                                    ]
                                )
                                model.parent

                        order =
                            Maybe.andThen String.toInt model.order

                        jsonBody =
                            Encode.object <|
                                [ ( "id", (\(CategoryId cId) -> Encode.int cId) categoryId )
                                , ( "name", encodeMaybe Encode.string model.name )
                                , ( "slug", encodeMaybe Encode.string model.slug )
                                , ( "description", encodeMaybe Encode.string model.description )
                                , ( "imageData", encodeMaybe Encode.string model.imageData )
                                , ( "imageName", encodeMaybe Encode.string model.imageName )
                                , ( "order", encodeMaybe Encode.int order )
                                ]
                                    ++ encodedParent
                    in
                    ( { model | isSaving = True }
                    , Api.patch Api.AdminEditCategory
                        |> Api.withJsonBody jsonBody
                        |> Api.withErrorHandler (Decode.succeed ())
                        |> Api.sendRequest SubmitEditResponse
                    )

                _ ->
                    noCommand model

        SubmitEditResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin CategoryList
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }, Ports.scrollToErrorMessage )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand model


slugFromName : EditForm -> RemoteData.WebData PageData.AdminEditCategoryData -> Bool
slugFromName =
    slugFrom .name .name


edit : CategoryId -> EditForm -> PageData.AdminNewCategoryData -> PageData.AdminEditCategoryData -> List (Html EditMsg)
edit categoryId model categories originalCategory =
    let
        valueWithFallback s1 s2 =
            s1 model
                |> Maybe.withDefault (s2 originalCategory)

        inputRow selector selector2 =
            Form.inputRow model.errors (valueWithFallback selector selector2)

        blankOption =
            option
                [ value "", selected <| Nothing == (model.parent |> Result.withDefault originalCategory.parent) ]
                [ text "" ]

        renderCategoryOption { id, name } =
            option
                [ value <| (\(CategoryId i) -> String.fromInt i) id
                , selected <| Just id == (model.parent |> Result.withDefault originalCategory.parent)
                ]
                [ text name ]

        imagePreview =
            case Maybe.map2 Tuple.pair model.imageData model.imageName of
                Nothing ->
                    div [ class "image-preview mb-4" ]
                        [ img
                            [ class "img-fluid"
                            , src originalCategory.image.original
                            ]
                            []
                        ]

                Just ( imageData, imageName ) ->
                    Admin.base64ImagePreview imageName imageData Nothing
    in
    [ form [ class (formSavingClass model), onSubmit SubmitEdit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow .name .name EInputName True "Name" "name" "text" "off"
        , inputRow .slug .slug EInputSlug True "Slug" "slug" "text" "off"
        , Form.selectRow Category.maybeIdParser EInputParent "Parent Category" True <|
            blankOption
                :: List.map renderCategoryOption
                    (List.filter (\c -> c.id /= categoryId) categories)
        , Form.textareaRow model.errors
            (model.description |> Maybe.withDefault originalCategory.description)
            EInputDescription
            False
            "Description"
            "description"
            10
        , inputRow .order (.order >> String.fromInt) EInputOrder True "Sort Order" "order" "number" "off"
        , Form.withLabel "Image" False <|
            [ imagePreview
            , button
                [ class "btn btn-sm btn-secondary"
                , type_ "button"
                , onClick ESelectImage
                ]
                [ text "Upload Image..." ]
            ]
        , div [ class "form-group" ]
            [ submitOrSavingButton model "Update Category" ]
        ]
    ]



-- UTILS


{-| Encode Nothing to Null and Just using the encoder.
-}
encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder val =
    val
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null
