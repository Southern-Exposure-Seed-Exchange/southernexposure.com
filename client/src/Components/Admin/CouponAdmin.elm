module Components.Admin.CouponAdmin exposing
    ( EditForm
    , EditMsg
    , NewForm
    , NewMsg
    , edit
    , initialEditForm
    , initialNewForm
    , list
    , new
    , updateEditForm
    , updateNewForm
    )

import Components.Admin.Admin as Admin exposing (updateEditField)
import Components.HorizontalForm as Form
import Data.Api as Api
import Data.Fields exposing (Cents, centsEncoder, centsToString)
import Data.PageData as PageData exposing (CouponType(..))
import Data.Routing.Routing as Routing exposing (AdminRoute(..), Route(..))
import Data.Validation as Validation exposing (FormErrors, formValidation)
import Dict
import Html exposing (Html, a, div, form, table, tbody, td, text, th, thead, tr)
import Html.Attributes as A exposing (class, type_)
import Html.Events exposing (onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (WebData)
import Time exposing (Posix)
import Utils.Format as Format
import Utils.Update exposing (noCommand)
import Utils.Utils exposing (posixToDateString)
import Utils.View exposing (routeLinkAttributes)



-- LIST


list : Time.Zone -> PageData.AdminCouponListData -> List (Html msg)
list zone { coupons } =
    let
        headerRow =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , th [] [ text "Code" ]
                , th [] [ text "Active" ]
                , th [] [ text "Expires" ]
                , th [] [ text "Edit" ]
                ]

        renderCoupon coupon =
            tr []
                [ td [] [ text coupon.name ]
                , td [] [ text <| renderType coupon.discount ]
                , td [] [ text coupon.code ]
                , td [ class "text-center" ] [ Admin.activeIcon coupon.isActive ]
                , td [] [ text <| Format.date zone coupon.expires ]
                , td []
                    [ a (routeLinkAttributes <| Admin <| CouponEdit coupon.id)
                        [ text "Edit" ]
                    ]
                ]

        renderType couponType =
            case couponType of
                FreeShipping ->
                    "Free Shipping"

                FlatDiscount cents ->
                    Format.cents cents

                PercentageDiscount percent ->
                    String.fromInt percent ++ "%"
    in
    [ a (class "mb-3 btn btn-primary" :: routeLinkAttributes (Admin CouponNew))
        [ text "New Coupon" ]
    , table [ class "table table-sm table-striped" ]
        [ thead [] [ headerRow ]
        , tbody [] <| List.map renderCoupon coupons
        ]
    ]



-- NEW


type alias NewForm =
    { code : String
    , name : String
    , description : String
    , isActive : Bool
    , discountType : DiscountType
    , discountAmount : String
    , minimumOrder : String
    , expires : String
    , totalUses : String
    , usesPerCustomer : String
    , isSaving : Bool
    , errors : FormErrors
    }


initialNewForm : NewForm
initialNewForm =
    { code = ""
    , name = ""
    , description = ""
    , isActive = True
    , discountType = Shipping
    , discountAmount = ""
    , minimumOrder = "0.00"
    , expires = ""
    , totalUses = "0"
    , usesPerCustomer = "0"
    , isSaving = False
    , errors = Api.initialErrors
    }


type DiscountType
    = Shipping
    | Percentage
    | Flat


type NewMsg
    = NInputCode String
    | NInputName String
    | NInputDescription String
    | NToggleActive Bool
    | NSelectType DiscountType
    | NInputAmount String
    | NInputMinimum String
    | NInputExpires String
    | NInputTotalUses String
    | NInputCustomerUses String
    | SubmitNew
    | SubmitNewResponse (WebData (Result FormErrors Int))


updateNewForm : Routing.Key -> NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm key msg model =
    case msg of
        NInputCode val ->
            noCommand { model | code = val }

        NInputName val ->
            noCommand { model | name = val }

        NInputDescription val ->
            noCommand { model | description = val }

        NToggleActive val ->
            noCommand { model | isActive = val }

        NSelectType val ->
            noCommand { model | discountType = val }

        NInputAmount val ->
            noCommand { model | discountAmount = val }

        NInputMinimum val ->
            noCommand { model | minimumOrder = val }

        NInputExpires val ->
            noCommand { model | expires = val }

        NInputTotalUses val ->
            noCommand { model | totalUses = val }

        NInputCustomerUses val ->
            noCommand { model | usesPerCustomer = val }

        SubmitNew ->
            case validateNewForm model of
                Err e ->
                    ( { model | errors = e }
                    , Ports.scrollToErrorMessage
                    )

                Ok validModel ->
                    ( { model | isSaving = True }
                    , Api.post Api.AdminNewCoupon
                        |> Api.withJsonBody (encodeNewForm validModel)
                        |> Api.withErrorHandler Decode.int
                        |> Api.sendRequest SubmitNewResponse
                    )

        SubmitNewResponse response ->
            case response of
                RemoteData.Success (Ok couponId) ->
                    ( initialNewForm
                    , Routing.newUrl key <| Admin <| CouponEdit couponId
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | isSaving = False, errors = errors }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure err ->
                    ( { model | isSaving = False, errors = Api.apiFailureToError err }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    ( { model | isSaving = False }, Cmd.none )


type alias ValidNewForm =
    { code : String
    , name : String
    , description : String
    , isActive : Bool
    , discount : CouponType
    , minimumOrder : Cents
    , expires : Posix
    , totalUses : Int
    , usesPerCustomer : Int
    }


validateNewForm : NewForm -> Result FormErrors ValidNewForm
validateNewForm model =
    formValidation
        (\discount min expires total customer ->
            { code = model.code
            , name = model.name
            , description = model.description
            , isActive = model.isActive
            , discount = discount
            , minimumOrder = min
            , expires = expires
            , totalUses = total
            , usesPerCustomer = customer
            }
        )
        |> Validation.apply "discount"
            (validateDiscount ( model.discountType, model.discountAmount ))
        |> Validation.apply "minimumOrder" (Validation.cents model.minimumOrder)
        |> Validation.apply "expires" (Validation.date model.expires)
        |> Validation.apply "totalUses" (Validation.natural model.totalUses)
        |> Validation.apply "usesPerCustomer" (Validation.natural model.usesPerCustomer)


encodeNewForm : ValidNewForm -> Value
encodeNewForm model =
    Encode.object
        [ ( "code", Encode.string model.code )
        , ( "name", Encode.string model.name )
        , ( "description", Encode.string model.description )
        , ( "isActive", Encode.bool model.isActive )
        , ( "discount", PageData.couponTypeEncoder model.discount )
        , ( "minimumOrder", centsEncoder model.minimumOrder )
        , ( "expires", Iso8601.encode model.expires )
        , ( "totalUses", Encode.int model.totalUses )
        , ( "usesPerCustomer", Encode.int model.usesPerCustomer )
        ]


new : NewForm -> List (Html NewMsg)
new model =
    let
        inputRow =
            Form.inputRow model.errors

        formDescription =
            "Use this form to create a new Coupon for Customers to use during Checkout. "
                ++ helpText
    in
    [ form [ class (Admin.formSavingClass model), onSubmit SubmitNew ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Html.p [] [ text formDescription ]
        , Api.generalFormErrors model
        , inputRow model.code NInputCode True "Code" "code" "text" "off"
        , inputRow model.name NInputName True "Name" "name" "text" "off"
        , inputRow model.description NInputDescription False "Description" "description" "text" "off"
        , couponTypeRow model.errors
            model.discountType
            model.discountAmount
            NSelectType
            NInputAmount
        , inputRow model.minimumOrder NInputMinimum True "Minimum Order Size" "minimumOrder" "text" "off"
        , inputRow model.totalUses NInputTotalUses True "Total Uses" "totalUses" "number" "off"
        , inputRow model.usesPerCustomer NInputCustomerUses True "Uses per Customer" "usesPerCustomer" "number" "off"
        , dateRow model.errors model.expires NInputExpires
        , Form.checkboxRow model.isActive NToggleActive "Is Enabled" "isActive"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Add Coupon" ]
        ]
    ]


couponTypeRow : Api.FormErrors -> DiscountType -> String -> (DiscountType -> msg) -> (String -> msg) -> Html msg
couponTypeRow errors selectedType enteredAmount selectMsg inputMsg =
    Admin.selectInputRow
        { label = "Discount Type"
        , isRequired = True
        , selectMsg = selectMsg
        , inputMsg = inputMsg
        , selectedValue = selectedType
        , selectId = "DiscountType"
        , selectOptions = [ Shipping, Percentage, Flat ]
        , selectToValue =
            \t ->
                case t of
                    Shipping ->
                        "shipping"

                    Percentage ->
                        "percentage"

                    Flat ->
                        "flat"
        , selectToString =
            \t ->
                case t of
                    Shipping ->
                        "Free Shipping"

                    Percentage ->
                        "Percent Discount"

                    Flat ->
                        "Flat Amount"
        , selectValueParser =
            \t ->
                case t of
                    "shipping" ->
                        Ok Shipping

                    "percentage" ->
                        Ok Percentage

                    "flat" ->
                        Ok Flat

                    _ ->
                        Err <| "Unrecognized discount type: " ++ t
        , inputValue = enteredAmount
        , inputId = "DiscountAmount"
        , inputAttributes =
            \t ->
                case t of
                    Shipping ->
                        [ type_ "hidden" ]

                    Percentage ->
                        [ type_ "number", A.min "1", A.max "100", A.step "1" ]

                    Flat ->
                        [ type_ "number", A.min "0.01", A.step "0.01" ]
        , errors = errors
        , errorField = "discount"
        }


dateRow : FormErrors -> String -> (String -> msg) -> Html msg
dateRow errors date msg =
    Form.dateRow errors date msg True "Expiration Date" "expires"



-- Edit


type alias EditForm =
    { code : Maybe String
    , name : Maybe String
    , description : Maybe String
    , isActive : Maybe Bool
    , discountType : Maybe DiscountType
    , discountAmount : Maybe String
    , minimumOrder : Maybe String
    , expires : Maybe String
    , totalUses : Maybe String
    , customerUses : Maybe String
    , errors : FormErrors
    , isSaving : Bool
    }


initialEditForm : EditForm
initialEditForm =
    { code = Nothing
    , name = Nothing
    , description = Nothing
    , isActive = Nothing
    , discountType = Nothing
    , discountAmount = Nothing
    , minimumOrder = Nothing
    , expires = Nothing
    , totalUses = Nothing
    , customerUses = Nothing
    , errors = Api.initialErrors
    , isSaving = False
    }


type EditMsg
    = EInputCode String
    | EInputName String
    | EInputDescription String
    | EToggleActive Bool
    | ESelectType DiscountType
    | EInputAmount String
    | EInputMinimum String
    | EInputExpires String
    | EInputTotalUses String
    | EInputCustomerUses String
    | SubmitEdit
    | SubmitEditResponse (WebData (Result FormErrors ()))


updateEditForm : Routing.Key -> WebData PageData.AdminEditCouponData -> EditMsg -> EditForm -> ( EditForm, Cmd EditMsg )
updateEditForm key original msg model =
    case msg of
        EInputCode val ->
            noCommand <|
                updateEditField val original .code <|
                    \v -> { model | code = v }

        EInputName val ->
            noCommand <|
                updateEditField val original .name <|
                    \v -> { model | name = v }

        EInputDescription val ->
            noCommand <|
                updateEditField val original .description <|
                    \v -> { model | description = v }

        EToggleActive val ->
            noCommand <|
                updateEditField val original .isActive <|
                    \v -> { model | isActive = v }

        ESelectType val ->
            noCommand <|
                case model.discountAmount of
                    Nothing ->
                        { model
                            | discountType = Just val
                            , discountAmount =
                                RemoteData.map (.discount >> couponToAmount) original
                                    |> RemoteData.toMaybe
                        }

                    Just _ ->
                        { model | discountType = Just val }

        EInputAmount val ->
            noCommand <|
                case model.discountType of
                    Nothing ->
                        { model
                            | discountAmount = Just val
                            , discountType =
                                RemoteData.map (.discount >> couponToType) original
                                    |> RemoteData.toMaybe
                        }

                    Just _ ->
                        { model | discountAmount = Just val }

        EInputMinimum val ->
            noCommand <|
                updateEditField val original (.minimumOrder >> centsToString) <|
                    \v -> { model | minimumOrder = v }

        EInputExpires val ->
            noCommand <|
                updateEditField val original (.expires >> posixToDateString) <|
                    \v -> { model | expires = v }

        EInputTotalUses val ->
            noCommand <|
                updateEditField val original (.totalUses >> String.fromInt) <|
                    \v -> { model | totalUses = v }

        EInputCustomerUses val ->
            noCommand <|
                updateEditField val original (.customerUses >> String.fromInt) <|
                    \v -> { model | customerUses = v }

        SubmitEdit ->
            case RemoteData.map .id original |> RemoteData.toMaybe of
                Nothing ->
                    noCommand model

                Just couponId ->
                    case validateEditForm couponId model of
                        Err e ->
                            ( { model | errors = e }
                            , Ports.scrollToErrorMessage
                            )

                        Ok validModel ->
                            ( { model | isSaving = True }
                            , Api.patch Api.AdminEditCoupon
                                |> Api.withJsonBody (encodeEditForm validModel)
                                |> Api.withErrorHandler (Decode.succeed ())
                                |> Api.sendRequest SubmitEditResponse
                            )

        SubmitEditResponse response ->
            case response of
                RemoteData.Success (Ok ()) ->
                    ( initialEditForm
                    , Routing.newUrl key <| Admin CouponList
                    )

                RemoteData.Success (Err errors) ->
                    ( { model | errors = errors, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                RemoteData.Failure error ->
                    ( { model | errors = Api.apiFailureToError error, isSaving = False }
                    , Ports.scrollToErrorMessage
                    )

                _ ->
                    noCommand { model | isSaving = False }


type alias ValidEditForm =
    { id : Int
    , code : Maybe String
    , name : Maybe String
    , description : Maybe String
    , isActive : Maybe Bool
    , discount : Maybe CouponType
    , minimumOrder : Maybe Cents
    , expires : Maybe Posix
    , totalUses : Maybe Int
    , customerUses : Maybe Int
    }


validateEditForm : Int -> EditForm -> Result FormErrors ValidEditForm
validateEditForm couponId model =
    formValidation
        (\discount min expires total customer ->
            { id = couponId
            , code = model.code
            , name = model.name
            , description = model.description
            , isActive = model.isActive
            , discount = discount
            , minimumOrder = min
            , expires = expires
            , totalUses = total
            , customerUses = customer
            }
        )
        |> Validation.applyMaybe "discount"
            validateDiscount
            (Maybe.map2 Tuple.pair model.discountType model.discountAmount)
        |> Validation.applyMaybe "minimumOrder" Validation.cents model.minimumOrder
        |> Validation.applyMaybe "expires" Validation.date model.expires
        |> Validation.applyMaybe "totalUses" Validation.natural model.totalUses
        |> Validation.applyMaybe "usesPerCustomer" Validation.natural model.customerUses


encodeEditForm : ValidEditForm -> Value
encodeEditForm model =
    let
        encodeMaybe encoder =
            Maybe.map encoder >> Maybe.withDefault Encode.null
    in
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "code", encodeMaybe Encode.string model.code )
        , ( "name", encodeMaybe Encode.string model.name )
        , ( "description", encodeMaybe Encode.string model.description )
        , ( "isActive", encodeMaybe Encode.bool model.isActive )
        , ( "discount", encodeMaybe PageData.couponTypeEncoder model.discount )
        , ( "minimumOrder", encodeMaybe centsEncoder model.minimumOrder )
        , ( "expires", encodeMaybe Iso8601.encode model.expires )
        , ( "totalUses", encodeMaybe Encode.int model.totalUses )
        , ( "usesPerCustomer", encodeMaybe Encode.int model.customerUses )
        ]


edit : EditForm -> PageData.AdminEditCouponData -> List (Html EditMsg)
edit model original =
    let
        valueWithFallback s1 s2 =
            s1 model
                |> Maybe.withDefault (s2 original)

        inputRow s1 s2 =
            Form.inputRow model.errors (valueWithFallback s1 s2)
    in
    [ form [ class (Admin.formSavingClass model), onSubmit SubmitEdit ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Html.p [] [ text helpText ]
        , Api.generalFormErrors model
        , inputRow .code .code EInputCode True "Code" "code" "text" "off"
        , inputRow .name .name EInputName True "Name" "name" "text" "off"
        , inputRow .description .description EInputDescription False "Description" "description" "text" "off"
        , couponTypeRow model.errors
            (valueWithFallback .discountType (.discount >> couponToType))
            (valueWithFallback .discountAmount (.discount >> couponToAmount))
            ESelectType
            EInputAmount
        , inputRow .minimumOrder (.minimumOrder >> centsToString) EInputMinimum True "Minimum Order Size" "minimumOrder" "text" "off"
        , inputRow .totalUses (.totalUses >> String.fromInt) EInputTotalUses True "Total Uses" "totalUses" "number" "off"
        , inputRow .customerUses (.customerUses >> String.fromInt) EInputCustomerUses True "Uses per Customer" "usesPerCustomer" "number" "off"
        , dateRow model.errors (valueWithFallback .expires (.expires >> posixToDateString)) EInputExpires
        , Form.checkboxRow (valueWithFallback .isActive .isActive) EToggleActive "Is Enabled" "isActive"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Update Coupon" ]
        ]
    ]



-- UTILS


couponToType : CouponType -> DiscountType
couponToType type_ =
    case type_ of
        FreeShipping ->
            Shipping

        FlatDiscount _ ->
            Flat

        PercentageDiscount _ ->
            Percentage


couponToAmount : CouponType -> String
couponToAmount type_ =
    case type_ of
        FreeShipping ->
            ""

        FlatDiscount cents ->
            centsToString cents

        PercentageDiscount percent ->
            String.fromInt percent


validateDiscount : ( DiscountType, String ) -> Validation.Validation CouponType
validateDiscount ( type_, amount ) =
    let
        validatePercentage =
            Validation.int amount
                |> Validation.map PercentageDiscount

        validateFlat =
            Validation.cents amount
                |> Validation.map FlatDiscount
    in
    case type_ of
        Shipping ->
            Validation.succeed FreeShipping

        Percentage ->
            validatePercentage

        Flat ->
            validateFlat


helpText : String
helpText =
    String.join " "
        [ "The \"Code\" is what the Customer enters into the Checkout form."
        , "The \"Name\" & \"Description\" will be sent to StoneEdge."
        , "Set the minimum order amount, total use count, or customer use to enforce usage requirements for the Coupon."
        , "A value of \"0\" for any of those field will prevent their enforcement."
        , "Coupons will automatically be disabled when they reach their maximum total uses or pass their expiration date."
        ]
