module Update.Utils
    exposing
        ( noCommand
        , withCommand
        , batchCommand
        , maybeCommand
        , extraCommand
        , updateAndCommand
        , discardCommand
        , nothingAndNoCommand
        )


noCommand : model -> ( model, Cmd msg )
noCommand =
    flip (,) Cmd.none


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
