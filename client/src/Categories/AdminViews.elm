module Categories.AdminViews exposing
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

import Api exposing (FormErrors)
import Base64
import Category exposing (CategoryId(..))
import Dict
import File exposing (File)
import Html exposing (Html, a, button, div, form, img, option, table, tbody, td, text, tr)
import Html.Attributes exposing (class, selected, src, style, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Utils exposing (slugify)
import PageData
import Ports
import RemoteData
import Result.Extra as Result
import Routing exposing (AdminRoute(..), Route(..))
import Task
import Update.Utils exposing (noCommand)
import Views.Admin exposing (formSavingClass, submitOrSavingButton)
import Views.HorizontalForm as Form
import Views.Images exposing (media)
import Views.Utils exposing (routeLinkAttributes, selectImageFile)


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
            , encodeImageData NImageEncoded imageFile
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
                    , Ports.scrollToID "form-errors-text"
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
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
        , Form.selectRow categoryIdParser NInputParent "Parent Category" True <|
            blankOption
                :: List.map renderCategoryOption categories
        , Form.textareaRow model.errors model.description NInputDescription False "Description" "description" 10
        , inputRow model.order NInputOrder True "Sort Order" "order" "number" "off"
        , Form.withLabel "Image" False <|
            [ base64ImagePreview model.imageName model.imageData
            , button
                [ class "btn btn-sm btn-secondary"
                , type_ "button"
                , onClick NSelectImage
                ]
                [ text "Upload Image..." ]
            ]
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
                if equalsOriginal val original .slug then
                    { model | slug = Nothing }

                else
                    { model | slug = Just val }

        EInputParent val ->
            noCommand <|
                if equalsOriginal val original .parent then
                    { model | parent = Err () }

                else
                    { model | parent = Ok val }

        EInputDescription val ->
            noCommand <|
                if equalsOriginal val original .description then
                    { model | description = Nothing }

                else
                    { model | description = Just val }

        EInputOrder val ->
            noCommand <|
                if equalsOriginal val original (.order >> String.fromInt) then
                    { model | order = Nothing }

                else
                    { model | order = Just val }

        ESelectImage ->
            ( model, selectImageFile EImageUploaded )

        EImageUploaded imageFile ->
            ( { model | imageName = Just <| File.name imageFile }
            , encodeImageData EImageEncoded imageFile
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
                    ( { model | errors = errors }, Ports.scrollToID "form-errors-text" )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    noCommand model


type Tuple4 a b c d
    = Tuple4 a b c d


slugFromName : EditForm -> RemoteData.WebData PageData.AdminEditCategoryData -> Bool
slugFromName model original =
    let
        maybeField : RemoteData.WebData a -> (a -> b) -> Maybe b
        maybeField m s =
            RemoteData.toMaybe m |> Maybe.map s

        oName =
            maybeField original .name

        oSlug =
            maybeField original .slug
    in
    case Tuple4 model.name model.slug oName oSlug of
        Tuple4 (Just n) (Just s) _ _ ->
            slugify n == s

        Tuple4 (Just n) Nothing _ (Just s) ->
            slugify n == s

        Tuple4 Nothing (Just s) (Just n) _ ->
            slugify n == s

        Tuple4 _ _ (Just n) (Just s) ->
            slugify n == s

        _ ->
            False


equalsOriginal :
    val
    -> RemoteData.WebData PageData.AdminEditCategoryData
    -> (PageData.AdminEditCategoryData -> val)
    -> Bool
equalsOriginal val original selector =
    original
        |> RemoteData.toMaybe
        |> Maybe.map selector
        |> (\originalField -> originalField == Just val)


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
                            , src <| media originalCategory.image.original
                            ]
                            []
                        ]

                Just ( imageData, imageName ) ->
                    base64ImagePreview imageName imageData
    in
    [ form [ class (formSavingClass model), onSubmit SubmitEdit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow .name .name EInputName True "Name" "name" "text" "off"
        , inputRow .slug .slug EInputSlug True "Slug" "slug" "text" "off"
        , Form.selectRow categoryIdParser EInputParent "Parent Category" True <|
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


{-| Build a command to encode an Image's data as a Base64 string.
-}
encodeImageData : (String -> msg) -> File -> Cmd msg
encodeImageData msg imageFile =
    File.toBytes imageFile
        |> Task.map Base64.fromBytes
        |> Task.map (Maybe.withDefault "")
        |> Task.perform msg


{-| Build a height-limited image preview showing the image name & the image,
using the Base64 encoded image data.
-}
base64ImagePreview : String -> String -> Html msg
base64ImagePreview imageName imageData =
    if String.isEmpty imageData then
        text ""

    else
        div [ class "image-preview mb-4" ]
            [ div [] [ text imageName ]
            , img [ class "img-fluid", src <| "data:*/*;base64," ++ imageData ] []
            ]


{-| Parse a potential Category ID from the string-representation of an Integer.
An empty string signals a Nothing value.
-}
categoryIdParser : String -> Result String (Maybe CategoryId)
categoryIdParser val =
    if String.isEmpty val then
        Ok Nothing

    else
        case String.toInt val of
            Just i ->
                Ok << Just <| CategoryId i

            Nothing ->
                Err "Could not parse category ID."


{-| Encode Nothing to Null and Just using the encoder.
-}
encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoder val =
    val
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null
