module Mock.Home exposing (..)

import Components.Button as Button exposing (defaultButton)
import Html exposing (..)
import Html.Attributes exposing (..)
import Views.Images as Images



-- Check Mock/README.md on how to use this


imageLinkView hrefLink imageLink altText =
    a [ href hrefLink ]
        [ img
            [ class "tw:hover:brightness-80 tw:transition-all"
            , src <| Images.static imageLink
            , alt altText
            ]
            []
        ]


section1 =
    div []
        [ h1 [ class "tw:pb-[32px] tw:whitespace-pre-line" ] [ text "New & Returning Varieties\nfor 2025" ]
        , div [ class "tw:grid tw:grid-cols-3 tw:gap-[12px] tw:w-fit tw:pb-[20px]" ]
            [ imageLinkView "/products/granny-hobbs-collards/" "homepage/1-1.png" "granny hobbs collard"
            , imageLinkView "/products/purple-lovegrass/" "homepage/1-2.png" "purple lovegrass"
            , imageLinkView "/products/cajun-jewel-okra/" "homepage/1-3.png" "Cajun Jewel Okra"
            ]
        , p []
            [ text " For 2025 we’re adding 21 new varieties to our listings. "
            , a
                [ href "https://southernexposure.com/products/cajun-jewel-okra/"
                ]
                [ text " Cajun Jewel okra" ]
            , text " (pictured above right) is a great early-bearing Louisiana variety. "
            , a
                [ href "https://southernexposure.com/products/purple-lovegrass/"
                ]
                [ text "Purple Lovegrass " ]
            , text " (pictured above center) and "
            , a
                [ href "https://southernexposure.com/products/mountain-mint-short-toothed/"
                ]
                [ text "Short-Toothed Mountain Mint " ]
            , text "are native plants newly listed by SESE this year: we hope you’ll seize the opportunity to support pollinators and other wildlife. "
            , a
                [ href "https://southernexposure.com/products/granny-hobbs-collards/"
                ]
                [ text "Granny Hobbs collards" ]
            , text " (pictured above left) and "
            , a
                [ href "https://southernexposure.com/products/minnie-mizelle-collards/"
                ]
                [ text "Minnie Mizelle collards " ]
            , text "are treasured family heirlooms whose preservation is thanks to the Heirloom Collard Project. We’re delighted to bring back "
            , a
                [ href "https://southernexposure.com/products/sieva-carolina-pole-lima-bean/"
                ]
                [ text "Sieva" ]
            , text ", a small white-seeded pole lima with great flavor that dates back to the 1700s. "
            , a
                [ href "https://southernexposure.com/products/petunia-balcony/"
                ]
                [ text "Balcony petunia" ]
            , text " is a fragrant heirloom that grows luxuriously either in the ground or a balcony planter. "
            ]
        ]


section2 =
    div []
        [ h1 [ class "tw:pb-[32px]" ] [ text "Featured Customer Favorites" ]
        , div [ class "tw:grid tw:grid-cols-3 tw:gap-[12px] tw:w-fit tw:pb-[20px]" ]
            [ imageLinkView "/products/alabama-blue-collards/" "homepage/2-1.png" "alabama blue collards"
            , imageLinkView "/products/cossack-pineapple-ground-cherry/" "homepage/2-2.png" "cossack pineapple ground cherry"
            , imageLinkView "products/cherokee-purple-tomato/" "homepage/2-3.png" "cherokee purple tomato"
            ]
        , p []
            [ text " Pictured above, "
            , a
                [ href "/products/alabama-blue-collards/"
                ]
                [ text "Alabama Blue Collards," ]
            , a
                [ href "/products/cossack-pineapple-ground-cherry/"
                ]
                [ text "Cossack Pineapple ground cherries" ]
            , text ", and "
            , a
                [ href "/products/cherokee-purple-tomato/"
                ]
                [ text "Cherokee Purple Tomato" ]
            , text ". Other customer favorites include "
            , a
                [ href "/products/lacinato-dinosaur-kale/"
                ]
                [ text "Lacinato kale" ]
            , text ", "
            , a
                [ href "/products/seminole-pumpkin/"
                ]
                [ text "Seminole Pumpkin," ]
            , a
                [ href "/products/early-prolific-straightneck-summer-squash/"
                ]
                [ text "Early Prolific Straightneck Summer Squash" ]
            , text ", "
            , a
                [ href "/products/blue-lake-bush-blue-lake-274-bush-snap-bean/"
                ]
                [ text "Blue Lake Bush Snap Bean" ]
            , text ", and "
            , a
                [ href "/products/zinnia-state-fair-mixed-colors/"
                ]
                [ text "State Fair Mixed Colors Zinnia" ]
            , text ". "
            ]
        ]


section3 =
    div []
        [ h1 [ class "tw:pb-[32px]" ] [ text "Featured Customer Favorites" ]
        , div [ class "tw:grid tw:grid-cols-3 tw:gap-[12px] tw:w-fit" ]
            [ imageLinkView "/vegetable-gardening-in-the-southeast-the-timber-press-guide-to-p-1722.html" "homepage/3-1.png" "Vegetable Gardening in the SE"
            , imageLinkView "/products/grow-great-vegetables-in-virginia/" "homepage/3-2.png" "Grow Great Virginia Vegetables"
            , imageLinkView "/products/grow-great-vegetables-in-north-carolina/" "homepage/3-3.png" "Grow Great North Carolina Vegetables"
            , imageLinkView "/products/grow-great-vegetables-in-tennessee/" "homepage/3-4.png" "Grow Great Tennessee Vegetables"
            , imageLinkView "/products/grow-great-vegetables-in-georgia/" "homepage/3-5.png" "Grow Great Georgia Vegetables"
            , imageLinkView "/products/grow-great-vegetables-in-south-carolina/" "homepage/3-6.png" "Grow Great South Carolina Vegetables"
            ]
        ]


sidebar =
    div [ class "tw:w-[218px] tw:shrink-0 tw:flex tw:flex-col tw:gap-[16px]" ]
        [ div [ class "tw:p-[20px] tw:bg-[rgba(254,245,233,0.6)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px]" ]
            [ imageLinkView "/retail-stores/" "homepage/4.png" "Seed Rack"
            , p [] [ text "Find our seeds at trusted local retailers near you." ]
            , Button.view { defaultButton | label = "Shop near you", style = Button.Outline, type_ = Button.Link "/retail-stores/" }
            ]
        , div [ class "tw:p-[20px] tw:bg-[rgba(254,245,233,0.6)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px]" ]
            [ imageLinkView "/categories/request-a-catalog/" "homepage/5.png" "Southern Exposure Catalog"
            , p [] [ text "Your garden starts here. Get our free Catalog & Guide." ]
            , Button.view { defaultButton | label = "Request Catalog", style = Button.Outline, type_ = Button.Link "/categories/request-a-catalog/" }
            ]
        , div [ class "tw:p-[20px] tw:bg-[rgba(254,245,233,0.6)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px]" ]
            [ p []
                [ text " For fastest service, "
                , strong []
                    [ text "we recommend ordering online" ]
                , text ". Browse the listings above to fill your cart or "
                , a
                    [ href "/quick-order"
                    ]
                    [ text " Quick Order by Item Number" ]
                , text ". If you want to mail in your order instead, you can print our "
                , a
                    [ href "/catalog/sese-order-form-2024.pdf"
                    ]
                    [ text " mail-in order form" ]
                , text ". "
                ]
            ]
        , div [ class "tw:p-[20px] tw:bg-[rgba(254,245,233,0.6)] tw:rounded-[16px] tw:flex tw:flex-col tw:gap-[16px]" ]
            [ imageLinkView "/gardenplanner" "homepage/6.png" "Southern Exposure Garden Planner"
            , p []
                [ text " This online tool helps manage your garden throughout the season and from year to year. You can order our seeds directly from the planner. "
                , a
                    [ href "/gardenplanner"
                    ]
                    [ text "Try our Garden Planner Tool free for 1 week "
                    , span
                        [ class "glyphicon glyphicon-menu-right"
                        ]
                        []
                    ]
                ]
            , Button.view { defaultButton | label = "Start Planning", style = Button.Outline, type_ = Button.Link "/gardenplanner" }
            ]
        ]


view =
    div [ class "tw:w-full tw:flex tw:flex-col tw:gap-[40px]" ]
        [ div [ class "" ]
            [ a [ href "/growing-guides/" ]
                [ img
                    [ class "clickable-image"
                    , src <| Images.static "homepage/banner.png"
                    ]
                    []
                ]
            ]
        , div [ class "tw:flex tw:w-full tw:gap-[40px]" ]
            [ -- Section list
              div [ class "tw:grow tw:flex tw:flex-col tw:gap-[80px]" ]
                [ section1
                , section2
                , section3
                ]

            -- Sidebar
            , sidebar
            ]
        ]
