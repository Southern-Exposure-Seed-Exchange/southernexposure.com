module Views.Admin exposing
    ( equalsOriginal
    , formSavingClass
    , slugFrom
    , submitOrSavingButton
    , updateEditField
    )

{-| Helper functions for the Admin views.
-}

import Html exposing (Html, button, text)
import Html.Attributes exposing (class, disabled, type_)
import Models.Utils exposing (slugify)
import RemoteData exposing (WebData)
import Views.Utils exposing (icon)


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
