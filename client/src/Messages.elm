module Messages exposing (Msg(..))

import Paginate
import RemoteData exposing (WebData)
import PageData exposing (ProductData)
import Routing exposing (Route)
import SiteUI exposing (NavigationData)
import SiteUI.Search as SiteSearch


type Msg
    = UrlUpdate Route
    | NavigateTo Route
    | SearchMsg SiteSearch.Msg
    | GetProductDetailsData (WebData PageData.ProductDetails)
    | GetCategoryDetailsData (WebData PageData.CategoryDetails)
    | GetNavigationData (WebData NavigationData)
    | CategoryPaginationMsg (Paginate.Msg ProductData)
    | SearchPaginationMsg (Paginate.Msg ProductData)
