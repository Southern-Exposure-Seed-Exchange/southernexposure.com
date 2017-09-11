port module Ports
    exposing
        ( setPageTitle
        , scrollToTop
        , scrollToID
        , collapseMobileMenus
        , storeAuthDetails
        , removeAuthDetails
        , loggedIn
        , loggedOut
        )

-- Page Change


port setPageTitle : String -> Cmd msg


port scrollToSelector : String -> Cmd msg


port collapseMobileMenus : () -> Cmd msg


scrollToTop : Cmd msg
scrollToTop =
    scrollToID "main"


scrollToID : String -> Cmd msg
scrollToID id =
    scrollToSelector <| "#" ++ id



-- Auth


port storeAuthDetails : ( String, Int ) -> Cmd msg


port removeAuthDetails : () -> Cmd msg


port loggedIn : ({ userId : Int, token : String } -> msg) -> Sub msg


port loggedOut : (() -> msg) -> Sub msg
