module Views.Pager exposing (elements)

{-| This module is responsible for generating the various bits of HTML used in
pagianted Views.

You pass in a `Config` to the `elements` function and it will spit out lazy
functions that can be used to render the pager, perPage links, descriptive
text, etc.

-}

import Html exposing (Html, a, b, div, li, node, span, text, ul)
import Html.Attributes exposing (attribute, class, tabindex)
import Paginate exposing (Paginated)
import Routing exposing (Route)
import Views.Utils exposing (routeLinkAttributes)


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
                            span [ class "font-weight-bold" ]
                                [ text <| String.fromInt c ]

                        else
                            a (routeLinkAttributes <| cfg.routeConstructor { page = 1, perPage = c })
                                [ text <| String.fromInt c ]
                    )
                |> List.intersperse (text " | ")
                |> (\ps ->
                        span [ class "font-weight-bold" ] [ text "Products per page: " ]
                            :: ps
                            |> span [ class "d-none d-md-block" ]
                   )

        previousLink _ =
            let
                previousPage =
                    max 1 (currentPage - 1)

                previousRoute =
                    cfg.routeConstructor { page = previousPage, perPage = perPage }
            in
            prevNextLink items (not << Paginate.hasPrevious) previousRoute "« Prev"

        nextLink _ =
            let
                nextPage =
                    min totalPages (currentPage + 1)

                nextRoute =
                    cfg.routeConstructor { page = nextPage, perPage = perPage }
            in
            prevNextLink items (not << Paginate.hasNext) nextRoute "Next »"

        renderSections _ =
            Paginate.bootstrapPager
                (\p -> routeLinkAttributes <| cfg.routeConstructor { page = p, perPage = perPage })
                2
                2
                items

        view _ =
            if totalPages <= 1 then
                text ""

            else
                node "nav"
                    [ attribute "aria-label" cfg.pagerAriaLabel, class cfg.pagerCssClass ]
                    [ ul [ class "pagination pagination-sm mb-0" ] <|
                        previousLink ()
                            :: renderSections ()
                            ++ [ nextLink () ]
                    ]

        paginationHtml content =
            div [ class "d-flex mb-2 justify-content-between align-items-center" ] content

        viewTop _ =
            paginationHtml [ pagingText (), div [ class "d-none d-md-block" ] [ view () ] ]

        viewBottom _ =
            paginationHtml [ div [ class "d-none d-md-block" ] [ pagingText () ], view () ]
    in
    { pagingText = pagingText
    , perPageLinks = perPageLinks
    , view = view
    , viewTop = viewTop
    , viewBottom = viewBottom
    }


prevNextLink : Paginated a b c -> (Paginated a b c -> Bool) -> Route -> String -> Html msg
prevNextLink items isDisabled route content =
    let
        ( itemClass, linkAttrs ) =
            if isDisabled items then
                ( " disabled", [ tabindex -1 ] )

            else
                ( "", [] )
    in
    li [ class <| "page-item" ++ itemClass ]
        [ a (class "page-link" :: linkAttrs ++ routeLinkAttributes route)
            [ text content ]
        ]
