port module Ports exposing
    ( cartItemCountChanged
    , collapseMobileMenus
    , collectStripeToken
    , loggedIn
    , loggedOut
    , newCartSessionToken
    , removeAuthDetails
    , removeCartSessionToken
    , scrollToID
    , scrollToTop
    , setCartItemCount
    , setPageTitle
    , storeAuthDetails
    , storeCartSessionToken
    , stripeTokenReceived
    )

-- Page Change


port setPageTitle : String -> Cmd msg


port scrollToSelector : String -> Cmd msg


port collapseMobileMenus : () -> Cmd msg


scrollToTop : Cmd msg
scrollToTop =
    scrollToID "site-header"


scrollToID : String -> Cmd msg
scrollToID id =
    scrollToSelector <| "#" ++ id



-- Auth


port storeAuthDetails : Int -> Cmd msg


port removeAuthDetails : () -> Cmd msg


port loggedIn : (Int -> msg) -> Sub msg


port loggedOut : (() -> msg) -> Sub msg



-- Cart Sessions


port storeCartSessionToken : String -> Cmd msg


port removeCartSessionToken : () -> Cmd msg


port newCartSessionToken : (String -> msg) -> Sub msg



-- Cart Item Counts


port setCartItemCount : Int -> Cmd msg


port cartItemCountChanged : (Int -> msg) -> Sub msg



-- Checkout


port collectStripeToken : ( String, Int ) -> Cmd msg


port stripeTokenReceived : (String -> msg) -> Sub msg
