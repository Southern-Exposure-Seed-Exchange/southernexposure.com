module Utils.Images exposing (media, noImagePath, static)


static : String -> String
static path =
    "/static/img/" ++ path


media : String -> String
media path =
    "/media/" ++ path


noImagePath : String
noImagePath =
    static "no-picture-available.png"
