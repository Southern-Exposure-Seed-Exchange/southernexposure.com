module Categories.AdminViews exposing
    ( NewForm
    , NewMsg
    , initialNewForm
    , list
    , new
    , updateNewForm
    )

import Api exposing (FormErrors)
import Base64
import Category exposing (CategoryId(..))
import Dict
import File exposing (File)
import Html exposing (Html, a, button, div, form, img, option, table, tbody, td, text, tr)
import Html.Attributes exposing (class, disabled, selected, src, style, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Utils exposing (slugify)
import PageData
import Ports
import RemoteData
import Routing exposing (AdminRoute(..), Route(..))
import Task
import Update.Utils exposing (noCommand)
import Views.HorizontalForm as Form
import Views.Utils exposing (icon, routeLinkAttributes, selectImageFile)


{-| TODO: Add Edit Buttons to Table
-}
list : PageData.AdminCategoryListData -> List (Html msg)
list { roots } =
    let
        depthPadding depth =
            style "margin-left" <| String.fromInt (depth * 20 + 10) ++ "px"

        renderCategory depth (PageData.AdminListCategory c) =
            tr []
                [ td [] [ div [ depthPadding depth ] [ text c.name ] ]
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


type NewMsg
    = InputName String
    | InputSlug String
    | InputParent (Maybe CategoryId)
    | InputDescription String
    | InputOrder String
    | SelectImage
    | ImageUploaded File
    | ImageEncoded String
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
    , isLoading : Bool
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
    , isLoading = False
    }


updateNewForm : NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm msg model =
    case msg of
        InputName val ->
            noCommand <|
                if model.slug == slugify model.name then
                    { model | name = val, slug = slugify val }

                else
                    { model | name = val }

        InputSlug val ->
            { model | slug = val }
                |> noCommand

        InputParent val ->
            { model | parent = val }
                |> noCommand

        InputDescription val ->
            { model | description = val }
                |> noCommand

        InputOrder val ->
            { model | order = val }
                |> noCommand

        SelectImage ->
            ( model, selectImageFile ImageUploaded )

        ImageUploaded imageFile ->
            ( { model | imageName = File.name imageFile }
            , File.toBytes imageFile
                |> Task.map Base64.fromBytes
                |> Task.map (Maybe.withDefault "")
                |> Task.perform ImageEncoded
            )

        ImageEncoded imageData ->
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
                                |> Maybe.map (\(CategoryId i) -> Encode.int i)
                                |> Maybe.withDefault Encode.null

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
                    ( { model | isLoading = True }
                    , Api.post Api.AdminNewCategory
                        |> Api.withJsonBody jsonBody
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest SubmitNewResponse
                    )

        SubmitNewResponse response ->
            case response of
                RemoteData.Success (Ok _) ->
                    ( initialNewForm
                    , Cmd.none
                      -- TODO: Redirect to Edit Category Route
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isLoading = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isLoading = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    noCommand model


new : NewForm -> PageData.AdminNewCategoryData -> List (Html NewMsg)
new model categories =
    let
        inputRow =
            Form.inputRow model.errors

        categoryIdParser val =
            if String.isEmpty val then
                Ok Nothing

            else
                case String.toInt val of
                    Just i ->
                        Ok << Just <| CategoryId i

                    Nothing ->
                        Err "Could not parse category ID."

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

        imagePreview =
            if String.isEmpty model.imageData then
                text ""

            else
                div [ class "image-preview mb-4" ]
                    [ div [] [ text model.imageName ]
                    , img [ class "img-fluid", src <| "data:*/*;base64," ++ model.imageData ] []
                    ]

        formClass =
            if model.isLoading then
                "form-loading"

            else
                ""

        submitButton =
            if model.isLoading then
                button [ class "btn btn-primary", disabled True, type_ "submit" ]
                    [ text "Saving...", icon "spinner fa-spin ml-2" ]

            else
                button [ class "btn btn-primary", type_ "submit" ]
                    [ text "Add Category" ]
    in
    [ form [ onSubmit SubmitNew, class formClass ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , inputRow model.name InputName True "Name" "name" "text" "off"
        , inputRow model.slug InputSlug True "Slug" "slug" "text" "off"
        , Form.selectRow categoryIdParser InputParent "Parent Category" True <|
            blankOption
                :: List.map renderCategoryOption categories
        , Form.textareaRow model.errors model.description InputDescription False "Description" "description" 10
        , inputRow model.order InputOrder True "Sort Order" "order" "number" "off"
        , Form.withLabel "Image" False <|
            [ imagePreview
            , button
                [ class "btn btn-sm btn-secondary"
                , type_ "button"
                , onClick SelectImage
                ]
                [ text "Upload Image..." ]
            ]
        , div [ class "form-group" ]
            [ submitButton
            ]
        ]
    ]
