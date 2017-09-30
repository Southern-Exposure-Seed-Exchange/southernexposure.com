module Update.Utils
    exposing
        ( noCommand
        , withCommand
        , extraCommand
        , updateAndCommand
        , discardCommand
        )


noCommand : model -> ( model, Cmd msg )
noCommand =
    flip (,) Cmd.none


withCommand : (model -> Cmd msg) -> model -> ( model, Cmd msg )
withCommand cmdFunction model =
    ( model, cmdFunction model )


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
