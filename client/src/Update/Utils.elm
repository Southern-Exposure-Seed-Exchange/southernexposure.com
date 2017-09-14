module Update.Utils
    exposing
        ( noCommand
        , withCommand
        , discardCommand
        )


noCommand : model -> ( model, Cmd msg )
noCommand =
    flip (,) Cmd.none


withCommand : (model -> Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
withCommand newCmd ( model, cmd ) =
    ( model, Cmd.batch [ cmd, newCmd model ] )


discardCommand : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
discardCommand updater ( model, _ ) =
    updater model
