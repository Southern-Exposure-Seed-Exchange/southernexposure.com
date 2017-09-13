module Update.Utils exposing (noCommand)


noCommand : a -> ( a, Cmd msg )
noCommand =
    flip (,) Cmd.none
