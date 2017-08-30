module SiteUI.Header exposing (view)

import Html exposing (Html, div, a, img, h1, br, text)
import Html.Attributes exposing (id, class, href, src)
import SiteUI.Search as SiteSearch


view : (SiteSearch.Msg -> msg) -> SiteSearch.Data -> Html msg
view tagger searchQuery =
    div [ class "container" ]
        [ div [ id "site-header", class "row clearfix" ]
            [ div [ class "col-sm-7 col-lg-6" ]
                [ div [ class "media" ]
                    [ a [ href "/" ]
                        [ img
                            [ id "site-logo"
                            , class "float-left mx-3"
                            , src "/static/img/logos/sese.png"
                            ]
                            []
                        ]
                    , div [ id "site-title", class "media-body my-auto" ]
                        [ h1 [ class "media-heading m-0" ]
                            [ a [ href "/" ]
                                [ text "Southern Exposure"
                                , br [] []
                                , text "Seed Exchange"
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "col-auto ml-auto d-none d-sm-block text-right" ]
                [ text "QUICK LINKS"
                , SiteSearch.form tagger searchQuery
                ]
            ]
        ]
