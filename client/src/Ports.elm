port module Ports exposing
    ( PageMetadata
    , cartItemCountChanged
    , collapseMobileMenus
    , collectStripeToken
    , logPurchase
    , logStatusCode
    , loggedIn
    , loggedOut
    , newCartSessionToken
    , removeAuthDetails
    , removeCartSessionToken
    , scrollToErrorMessage
    , scrollToID
    , scrollToName
    , scrollToTop
    , setCartItemCount
    , setPageTitle
    , storeAuthDetails
    , storeCartSessionToken
    , stripeTokenReceived
    , updatePageMetadata
    )

import Json.Encode exposing (Value)



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


scrollToErrorMessage : Cmd msg
scrollToErrorMessage =
    scrollToID "form-errors-text"


scrollToName : String -> Cmd msg
scrollToName name =
    scrollToSelector <| "*[name='" ++ name ++ "']"



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



-- SEO / Analytics


port logPurchase : Value -> Cmd msg


port logStatusCode : Int -> Cmd msg


{-| Send the URL, Title, & an Optional Image for updating Analytics & SEO tags.
-}
port updatePageMetadata : PageMetadata -> Cmd msg


type alias PageMetadata =
    { url : String
    , title : String
    , description : String
    , image : Maybe String
    }
