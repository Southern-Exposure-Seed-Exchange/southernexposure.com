module Messages exposing (Msg(..))

import RemoteData exposing (WebData)
import PageData
import Routing exposing (Route)
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | SearchMsg SiteSearch.Msg
    | GetProductDetailsData (WebData PageData.ProductDetails)
    | GetCategoryDetailsData (WebData PageData.CategoryDetails)
    | GetSearchResultsData (WebData PageData.SearchResults)
    | GetNavigationData (WebData NavigationData)
