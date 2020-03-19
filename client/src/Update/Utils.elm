module Update.Utils exposing
    ( noCommand, withCommand, batchCommand, maybeCommand, extraCommand, updateAndCommand, discardCommand, nothingAndNoCommand
    , updateArray, removeIndex
    )

{-|


# Update Helpers

@docs noCommand, withCommand, batchCommand, maybeCommand, extraCommand, updateAndCommand, discardCommand, nothingAndNoCommand


# Array Updaters

@docs updateArray, removeIndex

-}

import Array exposing (Array)


noCommand : model -> ( model, Cmd msg )
noCommand m =
    ( m, Cmd.none )


withCommand : (model -> Cmd msg) -> model -> ( model, Cmd msg )
withCommand cmdFunction model =
    ( model, cmdFunction model )


batchCommand : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
batchCommand cmd =
    Tuple.mapSecond (\c -> Cmd.batch [ c, cmd ])


maybeCommand : (a -> Cmd msg) -> Maybe a -> Cmd msg
maybeCommand cmdConstructor =
    Maybe.map cmdConstructor >> Maybe.withDefault Cmd.none


extraCommand : (model -> Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
extraCommand newCmd ( model, cmd ) =
    ( model, Cmd.batch [ cmd, newCmd model ] )


updateAndCommand : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateAndCommand func ( model, cmd ) =
    func model
        |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ])


discardCommand : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
discardCommand updater ( model, _ ) =
    updater model


nothingAndNoCommand : model -> ( model, Maybe a, Cmd msg )
nothingAndNoCommand model =
    ( model, Nothing, Cmd.none )


{-| Update the item at the given array index
-}
updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray index updater arr =
    Array.get index arr
        |> Maybe.map (\v -> Array.set index (updater v) arr)
        |> Maybe.withDefault arr


{-| Remove the item at the index of the given array.
-}
removeIndex : Int -> Array a -> Array a
removeIndex index arr =
    Array.append
        (Array.slice 0 index arr)
        (Array.slice (index + 1) (Array.length arr) arr)
