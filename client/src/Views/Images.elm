module Views.Images exposing (media, static)


static : String -> String
static path =
    "/static/img/" ++ path


media : String -> String
media path =
    "/media/" ++ path
