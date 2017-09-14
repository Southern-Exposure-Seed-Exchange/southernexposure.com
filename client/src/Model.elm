module Model exposing (Model, initial)

import RemoteData exposing (WebData)
import Auth.CreateAccount as CreateAccount
import Auth.EditContact as EditContact
import Auth.EditLogin as EditLogin
import Auth.Login as Login
import PageData exposing (PageData)
import Routing exposing (Route)
import Search
import SiteUI exposing (NavigationData)
import User exposing (AuthStatus)


type alias Model =
    { navigationData : WebData NavigationData
    , route : Route
    , pageData : PageData
    , searchData : Search.Data
    , advancedSearchData : Search.Data
    , createAccountForm : CreateAccount.Form
    , loginForm : Login.Form
    , editLoginForm : EditLogin.Form
    , editContactForm : EditContact.Form
    , currentUser : AuthStatus
    }


initial : Route -> Model
initial route =
    { navigationData = RemoteData.Loading
    , route = route
    , pageData = PageData.initial
    , searchData = Search.initial
    , advancedSearchData = Search.initial
    , createAccountForm = CreateAccount.initial
    , loginForm = Login.initial
    , editLoginForm = EditLogin.initial
    , editContactForm = EditContact.initial
    , currentUser = User.unauthorized
    }
