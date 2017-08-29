module Views.Images exposing (static, media)


static : String -> String
static path =
    "/static/img/" ++ path


media : String -> String
media path =
    "/media/" ++ path
