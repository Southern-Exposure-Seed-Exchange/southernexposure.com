module Views.StaticPageAdmin exposing
    ( EditForm
    , EditMsg
    , ListForm
    , ListMsg
    , NewForm
    , NewMsg
    , edit
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
import Dict
import Html exposing (Html, a, div, form, h1, hr, table, tbody, td, text, th, thead, tr)
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
import Views.Admin as Admin exposing (equalsOriginal, formSavingClass, updateEditField)
import Views.HorizontalForm as Form
import Views.Utils exposing (htmlOrBlank, rawHtml, routeLinkAttributes)



-- LIST


type alias ListForm =
    { query : String }


initialListForm : ListForm
initialListForm =
    { query = "" }


type ListMsg
    = InputQuery String


updateListForm : ListMsg -> ListForm -> ListForm
updateListForm msg model =
    case msg of
        InputQuery val ->
            { model | query = val }


list : ListForm -> PageData.AdminPageListData -> List (Html ListMsg)
list listForm { pages, homePageId } =
    let
        renderPage { name, slug, id } =
            tr []
                [ td [] [ text name ]
                , td [] [ text slug ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| PageEdit id)
                        [ text "Edit" ]
                    ]
                ]

        headerCells =
            [ th [] [ text "Name" ]
            , th [] [ text "Slug" ]
            , th [] []
            ]

        editHomepageButton =
            htmlOrBlank
                (\id ->
                    a
                        (class "ml-3 btn btn-secondary"
                            :: routeLinkAttributes (Admin <| PageEdit id)
                        )
                        [ text "Edit Homepage" ]
                )
                homePageId

        ( searchInput, filterPages ) =
            Admin.searchInput InputQuery (\p t -> iContains t p.name) listForm

        iContains s1 s2 =
            String.contains s1 (String.toLower s2)
    in
    [ div [ class "row" ]
        [ div [ class "col-md-8 mb-3" ]
            [ a (class "btn btn-primary" :: routeLinkAttributes (Admin PageNew))
                [ text "New Page" ]
            , editHomepageButton
            ]
        , div [ class "col-md-4 text-md-right mb-3" ] [ searchInput ]
        ]
    , table [ class "table table-sm table-striped" ]
        [ thead [] [ tr [] headerCells ]
        , tbody [] <| List.map renderPage <| filterPages pages
        ]
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
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure errors ->
                    ( { model | errors = Api.apiFailureToError errors, isSaving = False }
                    , Ports.scrollToErrorMessage
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
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure errors ->
                    ( { model | errors = Api.apiFailureToError errors, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand model


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
