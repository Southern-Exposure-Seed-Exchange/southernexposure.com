module Auth.Utils exposing (noCommandOrStatus)


noCommandOrStatus : a -> ( a, Maybe b, Cmd msg )
noCommandOrStatus model =
    ( model, Nothing, Cmd.none )
