module Views.Admin exposing
    ( formSavingClass
    , submitOrSavingButton
    )

{-| Helper functions for the Admin views.
-}

import Html exposing (Html, button, text)
import Html.Attributes exposing (class, disabled, type_)
import Views.Utils exposing (icon)


{-| Show a submit button, or a disabled saving button with a spinner if the
form is being saved.
-}
submitOrSavingButton : Bool -> String -> Html msg
submitOrSavingButton isSaving content =
    if isSaving then
        button [ class "btn btn-primary", disabled True, type_ "submit" ]
            [ text "Saving...", icon "spinner fa-spin ml-2" ]

    else
        button [ class "btn btn-primary", type_ "submit" ]
            [ text content ]


{-| Return the class to indicate form saving if the saving state is True.
-}
formSavingClass : Bool -> String
formSavingClass isSaving =
    if isSaving then
        " form-saving "

    else
        ""
