module Components.Pager exposing (Config, Elements, elements)

{-| This module is responsible for generating the various bits of HTML used in
pagianted Components.Admin.

You pass in a `Config` to the `elements` function and it will spit out lazy
functions that can be used to render the pager, perPage links, descriptive
text, etc.

-}

import Components.Aria as Aria
import Components.Svg exposing (chevronLeftSvg, chevronRightSvg)
import Data.Routing.Routing exposing (Route)
import Html exposing (Html, a, b, div, node, span, text)
import Html.Attributes exposing (class, tabindex)
import Paginate exposing (Paginated)
import Utils.View exposing (routeLinkAttributes)


{-| Configuration data for the pager.

  - `itemDescription`: What are we displaying? E.g., "Products" or "Orders"
  - `pagerAriaLabel`: The accessibility label for the acual Pager.
  - `pagerCssClass`: A css class to apply to the Pager's `nav` element.
  - `pageSizes`: Available `perPage` amounts.
  - `routeConstructor`: Build a new route for page/perPage changes.

-}
type alias Config =
    { itemDescription : String
    , pagerAriaLabel : String
    , pagerCssClass : String
    , pageSizes : List Int
    , routeConstructor : { page : Int, perPage : Int } -> Route
    }


{-| Generated HTML elements for rendering the pager & associated information.

  - `pagingText`: Renders the `Displaying x to x (of x items)` text.
  - `perPageLinks`: Render the `25 | 50 | 100` link text for changing the perPage size.
  - `view`: Renders the actual pager.
  - `viewTop`: Combines the Pager & Paging Text, hiding the pager on small screens.
  - `viewBottom`: Combines the Pager & Paging Text, hiding the text on small screens.

-}
type alias Elements msg =
    { pagingText : () -> Html msg
    , perPageLinks : () -> Html msg
    , view : () -> Html msg
    , viewTop : () -> Html msg
    , viewBottom : () -> Html msg
    }


customBootstrapPager : (Int -> List (Html.Attribute msg)) -> Int -> Int -> Paginated a b c -> List (Html msg)
customBootstrapPager linkAttributes endPagesToShow middlePagesToShow pagination =
    let
        class_ =
            "tw:block tw:flex tw:items-center tw:justify-center tw:w-[36px] tw:h-[36px] tw:rounded-[8px] tw:hover:no-underline! tw:hover:bg-[#4DAA9A]! tw:hover:text-white!"

        itemClass isCurrent =
            if isCurrent then
                class_ ++ " tw:bg-[#4DAA9A]! tw:text-white! "

            else
                class_

        renderItem page isCurrent =
            a (class (itemClass isCurrent) :: linkAttributes page)
                [ span [] [ text <| String.fromInt page ] ]

        dots =
            div [ class "page-item disabled tw:border-none!" ]
                [ span [ class "page-link tw:border-none!" ] [ text "..." ] ]
    in
    Paginate.getPagerSections endPagesToShow middlePagesToShow pagination
        |> List.map (List.map <| \( a, b ) -> renderItem a b)
        |> List.intersperse [ dots ]
        |> List.concat


elements : Config -> Paginated a b c -> Elements msg
elements cfg items =
    let
        currentPage =
            Paginate.getPage items

        totalPages =
            Paginate.getTotalPages items

        totalItems =
            Paginate.getTotalItems items

        perPage =
            Paginate.getPerPage items

        pagingStart _ =
            String.fromInt <|
                (currentPage - 1)
                    * perPage
                    + 1

        pagingEnd _ =
            String.fromInt <|
                if (not << Paginate.hasNext) items || totalItems < perPage then
                    totalItems

                else
                    currentPage * perPage

        pagingText _ =
            if totalItems == 0 then
                text ""

            else
                span []
                    [ text "Displaying "
                    , b [] [ text <| pagingStart () ]
                    , text " to "
                    , b [] [ text <| pagingEnd () ]
                    , text " (of "
                    , b [] [ text <| String.fromInt totalItems ]
                    , text " "
                    , text <| String.toLower cfg.itemDescription
                    , text ")"
                    ]

        perPageLinks _ =
            cfg.pageSizes
                |> List.map
                    (\c ->
                        if c == perPage then
                            span [ class "tw:font-semibold" ]
                                [ text <| String.fromInt c ]

                        else
                            a (routeLinkAttributes <| cfg.routeConstructor { page = 1, perPage = c })
                                [ text <| String.fromInt c ]
                    )
                |> List.intersperse (text " | ")
                |> (\ps ->
                        span [ class "tw:font-semibold" ] [ text <| cfg.itemDescription ++ " per page: " ]
                            :: ps
                            |> span [ class "d-none d-md-block tw:text-[14px]" ]
                   )

        previousLink _ =
            let
                previousPage =
                    max 1 (currentPage - 1)

                previousRoute =
                    cfg.routeConstructor { page = previousPage, perPage = perPage }
            in
            prevNextLink items (not << Paginate.hasPrevious) previousRoute chevronLeftSvg

        nextLink _ =
            let
                nextPage =
                    min totalPages (currentPage + 1)

                nextRoute =
                    cfg.routeConstructor { page = nextPage, perPage = perPage }
            in
            prevNextLink items (not << Paginate.hasNext) nextRoute chevronRightSvg

        renderSections _ =
            customBootstrapPager
                (\p -> routeLinkAttributes <| cfg.routeConstructor { page = p, perPage = perPage })
                2
                2
                items

        view _ =
            if totalPages <= 1 then
                text ""

            else
                node "nav"
                    [ Aria.label cfg.pagerAriaLabel, class cfg.pagerCssClass ]
                    [ div [ class "tw:flex tw:gap-[16px] tw:items-center" ] <|
                        previousLink ()
                            :: renderSections ()
                            ++ [ nextLink () ]
                    ]

        viewTop _ =
            div [ class "tw:flex tw:justify-between" ] [ pagingText (), div [ class "d-none d-md-block" ] [ view () ] ]

        viewBottom _ =
            div [ class "tw:flex tw:justify-center tw:w-full tw:pt-[40px]" ]
                [ --     div [ class "d-none d-md-block" ] [ pagingText () ]
                  -- ,
                  view ()
                ]
    in
    { pagingText = pagingText
    , perPageLinks = perPageLinks
    , view = view
    , viewTop = viewTop
    , viewBottom = viewBottom
    }


{-| Helper to make the Previous/Next links for the pager, with the ability to
disable them for the first/last pages.
-}
prevNextLink : Paginated a b c -> (Paginated a b c -> Bool) -> Route -> Html msg -> Html msg
prevNextLink items isDisabled route content =
    let
        ( itemClass, linkAttrs ) =
            if isDisabled items then
                ( " disabled", [ tabindex -1, Aria.disabled True ] )

            else
                ( "", [] )
    in
    div [ class <| "" ++ itemClass ]
        [ a (class "tw:block tw:p-[4px]" :: linkAttrs ++ routeLinkAttributes route)
            [ content ]
        ]
