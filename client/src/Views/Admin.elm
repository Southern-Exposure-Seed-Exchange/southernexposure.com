module Views.Admin exposing
    ( SearchableTableConfig
    , activeIcon
    , base64ImagePreview
    , encodeImageData
    , equalsOriginal
    , formSavingClass
    , imageSelectRow
    , searchableTable
    , slugFrom
    , submitOrSavingButton
    , updateEditField
    )

{-| Helper functions for the Admin views.
-}

import Base64
import File exposing (File)
import Html exposing (Html, button, div, form, img, input, table, tbody, text)
import Html.Attributes exposing (class, disabled, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Models.Utils exposing (slugify)
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Task
import Views.HorizontalForm as Form
import Views.Pager as Pager
import Views.Utils exposing (icon)


{-| Render a Green Checkmark for Active items & a Red X for Inactive Ones.
-}
activeIcon : Bool -> Html msg
activeIcon isActive =
    if isActive then
        icon "check-circle text-success"

    else
        icon "times-circle text-danger"


{-| Show a submit button, or a disabled saving button with a spinner if the
form is being saved.
-}
submitOrSavingButton : { model | isSaving : Bool } -> String -> Html msg
submitOrSavingButton { isSaving } content =
    if isSaving then
        button [ class "btn btn-primary", disabled True, type_ "submit" ]
            [ text "Saving...", icon "spinner fa-spin ml-2" ]

    else
        button [ class "btn btn-primary", type_ "submit" ]
            [ text content ]


{-| Configuration parameters for the `searchableTable` rendering function.
-}
type alias SearchableTableConfig a msg =
    { itemDescription : String
    , searchFormQuery : String
    , searchMsg : msg
    , queryInputMsg : String -> msg
    , pager : Pager.Elements msg
    , tableHeader : Html msg
    , rowRenderer : a -> Html msg
    }


{-| A re-usable view for Admin pages containing searchable, paginated items in
a table view.
-}
searchableTable : SearchableTableConfig a msg -> Paginated a b c -> List (Html msg)
searchableTable cfg items =
    let
        searchForm =
            form [ onSubmit cfg.searchMsg ]
                [ div [ class "input-group input-group-sm" ]
                    [ input
                        [ class "form-control"
                        , value cfg.searchFormQuery
                        , type_ "search"
                        , onInput cfg.queryInputMsg
                        ]
                        []
                    , div [ class "input-group-append" ]
                        [ button [ class "btn btn-primary", type_ "submit" ] [ icon "search" ] ]
                    ]
                ]

        renderedTable =
            if Paginate.isLoading items then
                div [ class "p-4 text-center" ]
                    [ text "Loading..."
                    , icon "spinner fa-spin ml-2"
                    ]

            else if Paginate.hasNone items then
                div [ class "p-4 text-center" ]
                    [ text <| "No " ++ cfg.itemDescription ++ " Found" ]

            else
                table [ class "table table-striped table-sm my-2 customer-table" ]
                    [ cfg.tableHeader
                    , tbody [] <| List.map cfg.rowRenderer <| Paginate.getCurrent items
                    ]
    in
    [ div [ class "d-flex justify-content-between align-items-center mb-2" ]
        [ searchForm, cfg.pager.perPageLinks () ]
    , cfg.pager.viewTop ()
    , renderedTable
    , cfg.pager.viewBottom ()
    ]


{-| Return the class to indicate form saving if the saving state is True.
-}
formSavingClass : { model | isSaving : Bool } -> String
formSavingClass { isSaving } =
    if isSaving then
        " form-saving "

    else
        ""


{-| Check to see if the new value for an Edit Form's field is equal to the
original item's value. Returns False if the original has not been loaded yet.
-}
equalsOriginal : a -> WebData m -> (m -> a) -> Bool
equalsOriginal val original selector =
    original
        |> RemoteData.toMaybe
        |> Maybe.map selector
        |> (\originalVal -> originalVal == Just val)


{-| Determine if the current Form slug has been generated from a source field.

    >>> slugFrom .name .name model original
    True

-}
slugFrom :
    ({ m | slug : Maybe String } -> Maybe String)
    -> ({ o | slug : String } -> String)
    -> { m | slug : Maybe String }
    -> WebData { o | slug : String }
    -> Bool
slugFrom mSelector oSelector model original =
    let
        maybeField : WebData a -> (a -> b) -> Maybe b
        maybeField m s =
            RemoteData.toMaybe m |> Maybe.map s

        oSource =
            maybeField original oSelector

        oSlug =
            maybeField original .slug
    in
    case Tuple4 (mSelector model) model.slug oSource oSlug of
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


{-| Helper type for case matching in the 'slugFrom' function.
-}
type Tuple4 a b c d
    = Tuple4 a b c d


{-| Update a field of an Edit Form, setting it to Nothing if the new value
matches the original value.
-}
updateEditField : val -> WebData original -> (original -> val) -> (Maybe val -> model) -> model
updateEditField val original selector updater =
    if equalsOriginal val original selector then
        updater Nothing

    else
        updater <| Just val


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


{-| Show an "Upload Image..." button with a preview if an image has been uploaded.
-}
imageSelectRow : String -> String -> msg -> String -> Html msg
imageSelectRow imageName imageData msg label =
    Form.withLabel label False <|
        [ base64ImagePreview imageName imageData
        , button
            [ class "btn btn-sm btn-secondary"
            , type_ "button"
            , onClick msg
            ]
            [ text "Upload Image..." ]
        ]
