port module Ports
    exposing
        ( setPageTitle
        , scrollToTop
        , scrollToID
        , collapseMobileMenus
        )


port setPageTitle : String -> Cmd msg


port scrollToSelector : String -> Cmd msg


port collapseMobileMenus : () -> Cmd msg


scrollToTop : Cmd msg
scrollToTop =
    scrollToID "main"


scrollToID : String -> Cmd msg
scrollToID id =
    scrollToSelector <| "#" ++ id
