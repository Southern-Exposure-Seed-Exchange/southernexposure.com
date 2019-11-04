module Views.StaticPageAdmin exposing
    ( NewForm
    , NewMsg
    , initialNewForm
    , list
    , new
    , updateNewForm
    )

import Api
import Dict
import Html exposing (Html, a, div, form, table, tbody, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Json.Encode as Encode
import Models.Utils exposing (slugify)
import PageData
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import StaticPage exposing (StaticPageId)
import Update.Utils exposing (noCommand)
import Views.Admin exposing (formSavingClass, submitOrSavingButton)
import Views.HorizontalForm as Form
import Views.Utils exposing (routeLinkAttributes)



-- LIST


list : PageData.AdminPageListData -> List (Html msg)
list { pages } =
    let
        renderPage { name } =
            tr []
                [ td [] [ text name ] ]
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


updateNewForm : NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm msg model =
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
                RemoteData.Success (Ok pId) ->
                    -- TODO: Redirect to Edit page once it exists
                    ( initialNewForm, Cmd.none )

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
            [ submitOrSavingButton model "Add Page"
            ]
        ]
    ]
