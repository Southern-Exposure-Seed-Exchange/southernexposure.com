module Views.StaticPageAdmin exposing
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

import Api
import Dict
import Html exposing (Html, a, div, form, h1, hr, table, tbody, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Utils exposing (slugify)
import PageData
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import StaticPage exposing (StaticPageId)
import Update.Utils exposing (noCommand)
import Views.Admin as Admin exposing (equalsOriginal, formSavingClass)
import Views.HorizontalForm as Form
import Views.Utils exposing (rawHtml, routeLinkAttributes)



-- LIST


list : PageData.AdminPageListData -> List (Html msg)
list { pages } =
    let
        renderPage { name, id } =
            tr []
                [ td [] [ text name ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| PageEdit id)
                        [ text "Edit" ]
                    ]
                ]
    in
    [ a (class "mb-3 btn btn-primary" :: routeLinkAttributes (Admin PageNew))
        [ text "New Page" ]
    , table [ class "table table-sm table-striped" ]
        [ tbody [] <| List.map renderPage pages ]
    ]



-- NEW


type alias NewForm =
    { title : String
    , slug : String
    , content : String
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialNewForm : NewForm
initialNewForm =
    { title = ""
    , slug = ""
    , content = ""
    , errors = Api.initialErrors
    , isSaving = False
    }


type NewMsg
    = NInputName String
    | NInputSlug String
    | NInputContent String
    | SubmitNew
    | SubmitNewResponse (WebData (Result Api.FormErrors StaticPageId))


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
    case msg of
        NInputName val ->
            noCommand <|
                if slugify model.title == model.slug then
                    { model | title = val, slug = slugify val }

                else
                    { model | title = val }

        NInputSlug val ->
            { model | slug = val }
                |> noCommand

        NInputContent val ->
            { model | content = val }
                |> noCommand

        SubmitNew ->
            let
                jsonBody =
                    Encode.object
                        [ ( "title", Encode.string model.title )
                        , ( "slug", Encode.string model.slug )
                        , ( "content", Encode.string model.content )
                        ]
            in
            ( { model | isSaving = True }
            , Api.post Api.AdminNewPage
                |> Api.withJsonBody jsonBody
                |> Api.withErrorHandler StaticPage.idDecoder
                |> Api.sendRequest SubmitNewResponse
            )

        SubmitNewResponse response ->
            case response of
                RemoteData.Success (Ok pageId) ->
                    ( initialNewForm
                    , Routing.newUrl key <| Admin <| PageEdit pageId
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                RemoteData.Failure errors ->
                    ( { model | errors = Api.apiFailureToError errors, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    noCommand model


new : NewForm -> List (Html NewMsg)
new model =
    let
        inputRow =
            Form.inputRow model.errors
    in
    [ form [ class (formSavingClass model), onSubmit SubmitNew ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow model.title NInputName True "Title" "title" "text" "off"
        , inputRow model.slug NInputSlug True "Slug" "slug" "text" "off"
        , Form.textareaRow model.errors model.content NInputContent True "Content" "content" 20
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Add Page"
            ]
        ]
    , hr [ class "my-4" ] []
    , contentPreview model.content
    ]



-- Edit


type alias EditForm =
    { title : Maybe String
    , slug : Maybe String
    , content : Maybe String
    , errors : Api.FormErrors
    , isSaving : Bool
    }


initialEditForm : EditForm
initialEditForm =
    { title = Nothing
    , slug = Nothing
    , content = Nothing
    , errors = Api.initialErrors
    , isSaving = False
    }


type EditMsg
    = EInputTitle String
    | EInputSlug String
    | EInputContent String
    | SubmitEdit
    | SubmitEditResponse (WebData (Result Api.FormErrors ()))


updateEditForm : Routing.Key -> WebData PageData.AdminEditPageData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    case msg of
        EInputTitle val ->
            noCommand <|
                if equalsOriginal val original .title then
                    if slugFromTitle model original then
                        { model | title = Nothing, slug = Nothing }

                    else
                        { model | title = Nothing }

                else if slugFromTitle model original then
                    { model | title = Just val, slug = Just <| slugify val }

                else
                    { model | title = Just val }

        EInputSlug val ->
            noCommand <|
                updateEditField val original .slug <|
                    \v -> { model | slug = v }

        EInputContent val ->
            noCommand <|
                updateEditField val original .content <|
                    \v -> { model | content = v }

        SubmitEdit ->
            case RemoteData.toMaybe original |> Maybe.map .id of
                Just pageId ->
                    let
                        jsonBody =
                            Encode.object <|
                                [ ( "id", StaticPage.idEncoder pageId )
                                , ( "title", encodeMaybe Encode.string model.title )
                                , ( "slug", encodeMaybe Encode.string model.slug )
                                , ( "content", encodeMaybe Encode.string model.content )
                                ]

                        encodeMaybe e val =
                            Maybe.map e val |> Maybe.withDefault Encode.null
                    in
                    ( { model | isSaving = True }
                    , Api.patch Api.AdminEditPage
                        |> Api.withJsonBody jsonBody
                        |> Api.withErrorHandler (Decode.succeed ())
                        |> Api.sendRequest SubmitEditResponse
                    )

                Nothing ->
                    noCommand model

        SubmitEditResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin PageList
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                RemoteData.Failure errors ->
                    ( { model | errors = Api.apiFailureToError errors, isSaving = False }
                    , Ports.scrollToID "form-errors-text"
                    )

                _ ->
                    noCommand model


{-| Update a field of an Edit Form, setting it to Nothing if the new value
matches the original value.
-}
updateEditField : val -> WebData original -> (original -> val) -> (Maybe val -> model) -> model
updateEditField val original selector updater =
    if equalsOriginal val original selector then
        updater Nothing

    else
        updater <| Just val


slugFromTitle : EditForm -> WebData PageData.AdminEditPageData -> Bool
slugFromTitle =
    Admin.slugFrom .title .title


edit : EditForm -> PageData.AdminEditPageData -> List (Html EditMsg)
edit model originalPage =
    let
        inputRow selector1 selector2 =
            Form.inputRow model.errors (selector1 model |> Maybe.withDefault (selector2 originalPage))
    in
    [ form [ class (formSavingClass model), onSubmit SubmitEdit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Api.generalFormErrors model
        , inputRow .title .title EInputTitle True "Title" "title" "text" "off"
        , inputRow .slug .slug EInputSlug True "Slug" "slug" "text" "off"
        , Form.textareaRow model.errors
            (model.content |> Maybe.withDefault originalPage.content)
            EInputContent
            True
            "Content"
            "content"
            20
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Update Page"
            ]
        ]
    , hr [ class "my-4" ] []
    , contentPreview <| Maybe.withDefault originalPage.content model.content
    ]



-- Utils


contentPreview : String -> Html msg
contentPreview content =
    div [ class "container" ]
        [ h1 [ class "text-center" ] [ text "Content Preview" ]
        , rawHtml content
        ]
