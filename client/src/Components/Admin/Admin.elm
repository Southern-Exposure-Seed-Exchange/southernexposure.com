module Components.Admin.Admin exposing
    ( MultiSelectConfig
    , MultiSelectItem
    , SearchableTableConfig
    , SelectInputRowConfig
    , activeIcon
    , base64ImagePreview
    , categorySelects
    , encodeImageData
    , encodeImageDatas
    , equalsOriginal
    , formSavingClass
    , imageSelectRow
    , imageSelectRows
    , multiSelect
    , searchInput
    , searchableTable
    , selectInputRow
    , slugFrom
    , submitOrSavingButton
    , updateEditField
    )

{-| Helper functions for the Admin Components.Admin.
-}

import Array exposing (Array)
import Base64
import Components.HorizontalForm as Form
import Components.Pager as Pager
import Data.Api as Api
import Data.Category as Category exposing (CategoryId(..))
import Data.PageData as PageData
import Dict
import File exposing (File)
import Html exposing (Attribute, Html, br, button, div, form, img, input, option, select, span, table, tbody, text)
import Html.Attributes exposing (class, disabled, id, name, required, selected, src, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Json.Decode as Decode
import Paginate exposing (Paginated)
import RemoteData exposing (WebData)
import Task
import Utils.Utils exposing (slugify)
import Utils.View exposing (icon)


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


{-| A re-usesable view for Admin pages with client-side searching functionality.

It takes a message constructor for updating the query string, a function for
matching an item against a single lowercase search term, and a model containing
the current search query.

It returns an HTML element for rendering the search input and a function for
filtering the items.

-}
searchInput :
    (String -> msg)
    -> (a -> String -> Bool)
    -> { model | query : String }
    -> ( Html msg, List a -> List a )
searchInput inputMsg queryMatcher { query } =
    let
        queryTerms =
            String.words <| String.toLower query

        matcher item =
            List.foldr (\term bool -> bool && queryMatcher item term) True queryTerms

        searchInput_ =
            div [ class "input-group mr-4" ]
                [ div [ class "input-group-prepend" ] [ span [ class "input-group-text" ] [ icon "search" ] ]
                , input
                    [ type_ "search"
                    , name "search"
                    , value query
                    , onInput inputMsg
                    , class "form-control"
                    ]
                    []
                ]
    in
    ( searchInput_, List.filter matcher )


{-| Return the class to indicate form saving if the saving state is True.
-}
formSavingClass : { model | isSaving : Bool } -> String
formSavingClass { isSaving } =
    if isSaving then
        " form-saving "

    else
        ""


type alias MultiSelectItem a =
    { name : String
    , value : a
    }


type alias MultiSelectConfig msg a =
    { isRequired : Bool
    , selectMsg : Int -> a -> msg
    , addMsg : msg
    , removeMsg : Int -> msg
    , errors : Api.FormErrors
    , selected : Array a
    , items : List (MultiSelectItem a)
    , toValue : a -> String
    , fromValue : String -> Result String a
    , blankOption : MultiSelectItem a
    , prefix : String
    , label : String
    , addLabel : String
    }


multiSelect : MultiSelectConfig msg a -> Html msg
multiSelect cfg =
    let
        renderSelect index item =
            div [ class "mb-2 d-flex align-items-center" ]
                [ select
                    [ class "form-control d-inline-block  w-75"
                    , onSelect index
                    ]
                  <|
                    blankOption item
                        ++ List.map (renderOption item) cfg.items
                , if index /= 0 || not cfg.isRequired then
                    button
                        [ class "btn btn-sm btn-danger ml-2"
                        , onClick <| cfg.removeMsg index
                        , type_ "button"
                        ]
                        [ icon "times" ]

                  else
                    text ""
                , Api.getErrorHtml (cfg.prefix ++ "-" ++ String.fromInt index) cfg.errors
                ]

        onSelect index =
            targetValue
                |> Decode.andThen decoder
                |> Decode.map (cfg.selectMsg index)
                |> on "change"

        decoder str =
            case cfg.fromValue str of
                Ok x ->
                    Decode.succeed x

                Err e ->
                    Decode.fail e

        renderOption current opt =
            option
                [ value <| cfg.toValue opt.value
                , selected <| opt.value == current
                ]
                [ text opt.name ]

        blankOption item =
            if item == cfg.blankOption.value then
                [ option
                    [ value <| cfg.toValue cfg.blankOption.value
                    , selected True
                    ]
                    [ text cfg.blankOption.name ]
                ]

            else
                []
    in
    Form.withLabel cfg.label
        cfg.isRequired
    <|
        [ div [] <|
            Array.toList <|
                Array.indexedMap renderSelect cfg.selected
        , button
            [ class "btn btn-sm btn-secondary"
            , type_ "button"
            , onClick cfg.addMsg
            ]
            [ text <| "Add " ++ cfg.addLabel ]
        ]


{-| Render a Dropdown of Categories with buttons for removing & adding additional dropdowns.
-}
categorySelects :
    Bool
    -> (Int -> CategoryId -> msg)
    -> msg
    -> (Int -> msg)
    -> { a | errors : Api.FormErrors, categories : Array CategoryId }
    -> List PageData.AdminCategorySelect
    -> Html msg
categorySelects isRequired selectMsg addMsg removeMsg model categories =
    multiSelect
        { isRequired = isRequired
        , selectMsg = selectMsg
        , addMsg = addMsg
        , removeMsg = removeMsg
        , errors = model.errors
        , selected = model.categories
        , items = List.map (\c -> { name = c.name, value = c.id }) categories
        , toValue = \(CategoryId i) -> String.fromInt i
        , fromValue = Category.idParser
        , blankOption = { name = "", value = CategoryId 0 }
        , prefix = "category"
        , label = "Categories"
        , addLabel = "Category"
        }


{-| Configuration parameters for the `selectInputRow` rendering functin.
-}
type alias SelectInputRowConfig a msg =
    { label : String
    , isRequired : Bool
    , selectMsg : a -> msg
    , inputMsg : String -> msg
    , selectedValue : a
    , selectId : String
    , selectOptions : List a
    , selectToValue : a -> String
    , selectToString : a -> String
    , selectValueParser : String -> Result String a
    , inputValue : String
    , inputId : String
    , inputAttributes : a -> List (Attribute msg)
    , errors : Api.FormErrors
    , errorField : String
    }


{-| A re-useable view for Admin form that requiring rendering a `select` and
`input` element on the same row.
-}
selectInputRow : SelectInputRowConfig a msg -> Html msg
selectInputRow cfg =
    let
        options =
            cfg.selectOptions
                |> List.map
                    (\opt ->
                        option [ value <| cfg.selectToValue opt, selected <| opt == cfg.selectedValue ]
                            [ text <| cfg.selectToString opt ]
                    )

        fieldErrors =
            Dict.get cfg.errorField cfg.errors |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
    Form.withLabel cfg.label
        cfg.isRequired
        [ Form.selectElement cfg.selectId "w-25 d-inline-block" cfg.selectValueParser cfg.selectMsg options
        , input
            ([ id <| "input" ++ cfg.inputId
             , name cfg.inputId
             , required cfg.isRequired
             , value cfg.inputValue
             , onInput cfg.inputMsg
             , class "form-control w-50 d-inline-block ml-4"
             ]
                ++ cfg.inputAttributes cfg.selectedValue
            )
            []
        , errorHtml
        ]


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


encodeImageDatas : (List String -> msg) -> List File -> Cmd msg
encodeImageDatas msg imageFiles =
    let
        exec =
            imageFiles
                |> List.map
                    (\f ->
                        File.toBytes f
                            |> Task.map Base64.fromBytes
                            |> Task.map (Maybe.withDefault "")
                    )
                |> Task.sequence
                |> Task.perform msg
    in
    exec


{-| Build a height-limited image preview showing the image name & the image,
using the Base64 encoded image data.
-}
base64ImagePreview : String -> String -> Maybe (String -> msg) -> Html msg
base64ImagePreview imageName imageData maybeDeleteAddedImgMsg =
    if String.isEmpty imageData then
        text ""

    else
        div [ class "mb-4" ]
            [ div [] [ text imageName ]
            , div [ class "image-preview tw:flex tw:gap-[16px] tw:items-start" ]
                [ img [ class "img-fluid", src <| "data:*/*;base64," ++ imageData ] []
                , case maybeDeleteAddedImgMsg of
                    Just deleteMsg ->
                        button [ class " tw:text-red-400", type_ "button", onClick <| deleteMsg imageName ]
                            [ text "Delete"
                            ]

                    Nothing ->
                        text ""
                ]
            ]


{-| Show an "Upload Image..." button with a preview if an image has been uploaded.
-}
imageSelectRow : String -> String -> msg -> String -> Html msg
imageSelectRow imageName imageData msg label =
    Form.withLabel label False <|
        [ base64ImagePreview imageName imageData Nothing
        , button
            [ class "btn btn-sm btn-secondary"
            , type_ "button"
            , onClick msg
            ]
            [ text "Upload Image" ]
        ]


{-| Show an "Upload Image" button with multiple preview
-}
imageSelectRows : List { fileName : String, base64Image : String } -> msg -> (String -> msg) -> String -> Html msg
imageSelectRows xs addNewImgMsg deleteAddedImgMsg label =
    -- let
    --     ( imageName, imageData ) =
    --         x
    -- in
    Form.withLabelStart label False <|
        [ div []
            (List.map
                (\{ fileName, base64Image } ->
                    base64ImagePreview fileName base64Image (Just deleteAddedImgMsg)
                )
                xs
            )
        , button
            [ class "btn btn-sm btn-secondary"
            , type_ "button"
            , onClick addNewImgMsg
            ]
            [ text "Add images" ]
        ]
