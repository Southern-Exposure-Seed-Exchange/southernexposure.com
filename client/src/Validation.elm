module Validation exposing
    ( FormValidation, formValidation, mapFormErrors, mergeFormValidation, addFormError, indexedValidation
    , FormErrors, mapFieldNames
    , Validation, succeed, map, apply, fromMaybe
    , int, natural, cents, milligrams, date
    )

{-| This module allows us to validate & transform strings received from input
elements.


# Form Validation

@docs FormValidation, formValidation, mapFormErrors, mergeFormValidation, addFormError, indexedValidation


# Errors

@docs FormErrors, mapFieldNames


# Field Validation

@docs Validation, succeed, map, apply, fromMaybe

@docs int, natural, cents, milligrams, date

TODO: Move the FormErrors types & manipulation functions from the Api module to
here.

-}

import Api
import Dict
import Iso8601
import Models.Fields exposing (Cents, Milligrams, centsFromString, milligramsFromString)
import Time exposing (Posix)


{-| The result of a form validation.
-}
type alias FormValidation a =
    Result FormErrors a


{-| Begin validation of a form model.
-}
formValidation : a -> FormValidation a
formValidation =
    Result.Ok


{-| Modify the errors in a form validation if any are present.
-}
mapFormErrors : (FormErrors -> FormErrors) -> FormValidation a -> FormValidation a
mapFormErrors updater =
    Result.mapError updater


{-| Turn a list of valiated forms in a form of validation lists.
-}
mergeFormValidation : List (FormValidation a) -> FormValidation (List a)
mergeFormValidation =
    let
        merge :
            FormValidation a
            -> FormValidation (List a)
            -> FormValidation (List a)
        merge validation currentResult =
            case ( validation, currentResult ) of
                ( Err e1, Err e2 ) ->
                    Err <|
                        Dict.merge
                            Dict.insert
                            (\f l r errs -> Dict.insert f (l ++ r) errs)
                            Dict.insert
                            e1
                            e2
                            Dict.empty

                ( Ok _, Err _ ) ->
                    currentResult

                ( Err e, Ok _ ) ->
                    Err e

                ( Ok v1, Ok vs ) ->
                    Ok <| v1 :: vs
    in
    List.foldr merge (formValidation [])


{-| Add all errors from the first validation to the second, doing nothing if the first is Ok.
-}
addFormError : FormValidation a -> FormValidation b -> FormValidation b
addFormError toAdd current =
    case ( toAdd, current ) of
        ( Ok _, _ ) ->
            current

        ( Err e, Ok _ ) ->
            Err e

        ( Err e1, Err e2 ) ->
            Dict.toList e1
                |> List.foldr
                    (\( k, vs ) err ->
                        List.foldr (\v err_ -> Api.addError k v err_) err vs
                    )
                    e2
                |> Err


{-| Validate a list of items, adding a prefix and index to the field names.
-}
indexedValidation : String -> (a -> FormValidation b) -> List a -> FormValidation (List b)
indexedValidation prefix validator =
    let
        prefixFields index =
            mapFormErrors <|
                mapFieldNames <|
                    \name ->
                        prefix ++ "-" ++ String.fromInt index ++ "-" ++ name
    in
    mergeFormValidation
        << List.indexedMap
            (\index item ->
                validator item
                    |> prefixFields index
            )


{-| Re-export of the 'Api.FormErrors' type.
-}
type alias FormErrors =
    Api.FormErrors


{-| Transform all the field names of an error set.
-}
mapFieldNames : (String -> String) -> FormErrors -> FormErrors
mapFieldNames updater =
    Dict.foldr (\k v errs -> Dict.insert (updater k) v errs) Dict.empty


{-| A validation has an error message or the validated value.
-}
type alias Validation a =
    Result String a


{-| Mark a value as valid.
-}
succeed : a -> Validation a
succeed =
    Result.Ok


{-| Transform a Validated value.
-}
map : (a -> b) -> Validation a -> Validation b
map =
    Result.map


{-| Apply a validation to a FormValidation by specifying the field name for
errors.
-}
apply : String -> Validation a -> FormValidation (a -> b) -> FormValidation b
apply fieldName validation result =
    case ( validation, result ) of
        ( Err msg, Err r ) ->
            Err <|
                Api.addError fieldName msg r

        ( Err msg, Ok _ ) ->
            Err <| Api.addError fieldName msg Api.initialErrors

        ( Ok _, Err r ) ->
            Err r

        ( Ok a, Ok aToB ) ->
            Ok <| aToB a


{-| Validate a String is a whole number.
-}
int : String -> Validation Int
int =
    String.toInt >> fromMaybe "Please enter a whole number."


{-| Validate a String is a whole number greater than or equal to zero.
-}
natural : String -> Validation Int
natural str =
    case String.toInt str of
        Nothing ->
            Err "Please enter a whole number."

        Just x ->
            if x >= 0 then
                Ok x

            else
                Err "Please enter a number greater than or equal to 0."


{-| Validate a dollar amount.
-}
cents : String -> Validation Cents
cents =
    centsFromString >> fromMaybe "Please enter a valid dollar amount."


{-| Validate a mass amount.
-}
milligrams : String -> Validation Milligrams
milligrams =
    milligramsFromString >> fromMaybe "Please enter a valid decimal amount."


{-| Validate a YYYY-MM-DD date from a `date` input.
-}
date : String -> Validation Posix
date =
    Iso8601.toTime >> Result.mapError (always <| "Please enter a valid YYYY-MM-DD date.")


{-| Transform a Maybe type into a Validation by using the given error message
if the value is Nothing.
-}
fromMaybe : String -> Maybe a -> Validation a
fromMaybe msg =
    Maybe.map Ok
        >> Maybe.withDefault (Err msg)
