module Messages exposing (Msg(..))

import RemoteData exposing (WebData)
import PageData
import Routing exposing (Route)
import SiteUI exposing (NavigationData)


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | GetProductDetailsData (WebData PageData.ProductDetails)
    | GetCategoryDetailsData (WebData PageData.CategoryDetails)
    | GetNavigationData (WebData NavigationData)
