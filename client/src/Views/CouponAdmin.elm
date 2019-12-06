module Views.CouponAdmin exposing
    ( NewForm
    , NewMsg
    , initialNewForm
    , list
    , new
    , updateNewForm
    )

import Api
import Dict
import Html exposing (Html, a, br, div, form, input, option, table, tbody, td, text, th, thead, tr)
import Html.Attributes as A exposing (class, id, name, required, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Models.Fields exposing (Cents, centsEncoder)
import PageData exposing (CouponType(..))
import Ports
import RemoteData exposing (WebData)
import Routing exposing (AdminRoute(..), Route(..))
import Time exposing (Posix)
import Update.Utils exposing (noCommand)
import Validation exposing (FormErrors, formValidation)
import Views.Admin as Admin
import Views.Format as Format
import Views.HorizontalForm as Form
import Views.Utils exposing (routeLinkAttributes)



-- LIST


{-| TODO: Link up the Edit Column
-}
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
                , td [] [ text "Edit" ]
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
    , minimumOrder = "0"
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


updateNewForm : NewMsg -> NewForm -> ( NewForm, Cmd NewMsg )
updateNewForm msg model =
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
                RemoteData.Success (Ok _) ->
                    ( initialNewForm
                    , Cmd.none
                      -- TODO: Redirect to Edit Page Once that Exists
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
        |> Validation.apply "discount" (Validation.succeed FreeShipping)
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
            String.join " "
                [ "Use this form to create a new Coupon for Customers to use during Checkout."
                , "The \"Code\" is what the Customer enters into the Checkout form."
                , "The \"Name\" & \"Description\" will be sent to StoneEdge."
                , "Set the minimum order amount, total use count, or customer use to enforce usage requirements for the Coupon."
                , "A value of \"0\" for any of those field will prevent their enforcement."
                , "Coupons will automatically be disabled when they reach their maximum total uses or pass their expiration date."
                ]
    in
    [ form [ class (Admin.formSavingClass model), onSubmit SubmitNew ]
        [ Form.genericErrorText <| not <| Dict.isEmpty model.errors
        , Html.p [] [ text formDescription ]
        , Api.generalFormErrors model
        , inputRow model.code NInputCode True "Code" "code" "text" "off"
        , inputRow model.name NInputName True "Name" "name" "text" "off"
        , inputRow model.description NInputDescription False "Description" "description" "text" "off"
        , couponTypeRow model.errors model.discountType model.discountAmount
        , inputRow model.minimumOrder NInputMinimum True "Minimum Order Size" "minimumOrder" "text" "off"
        , inputRow model.totalUses NInputTotalUses True "Total Uses" "totalUses" "number" "off"
        , inputRow model.usesPerCustomer NInputCustomerUses True "Uses per Customer" "usesPerCustomer" "number" "off"
        , dateRow model.errors model.expires
        , Form.checkboxRow model.isActive NToggleActive "Is Enabled" "isActive"
        , div [ class "form-group" ]
            [ Admin.submitOrSavingButton model "Add Coupon" ]
        ]
    ]


couponTypeRow : Api.FormErrors -> DiscountType -> String -> Html NewMsg
couponTypeRow errors selectedType enteredAmount =
    let
        inputId =
            "DiscountAmount"

        selectId =
            "DiscountType"

        inputAttrs =
            case selectedType of
                Shipping ->
                    [ type_ "hidden" ]

                Percentage ->
                    [ type_ "number", A.min "1", A.max "100", A.step "1" ]

                Flat ->
                    [ type_ "number", A.min "0.01", A.step "0.01" ]

        typeParser str =
            case str of
                "shipping" ->
                    Ok Shipping

                "percentage" ->
                    Ok Percentage

                "flat" ->
                    Ok Flat

                _ ->
                    Err <| "Unrecognized discount type: " ++ str

        typeToValue type_ =
            case type_ of
                Shipping ->
                    "shipping"

                Percentage ->
                    "percentage"

                Flat ->
                    "flat"

        typeToString type_ =
            case type_ of
                Shipping ->
                    "Free Shipping"

                Percentage ->
                    "Percent Discount"

                Flat ->
                    "Flat Amount"

        options =
            [ Shipping, Percentage, Flat ]
                |> List.map
                    (\t ->
                        option
                            [ value <| typeToValue t
                            , selected <| t == selectedType
                            ]
                            [ text <| typeToString t ]
                    )

        fieldErrors =
            Dict.get "discount" errors |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
    Form.withLabel "Discount Type"
        True
        [ Form.selectElement selectId "w-25 d-inline-block" typeParser NSelectType options
        , input
            ([ id <| "input" ++ inputId
             , name inputId
             , required True
             , value enteredAmount
             , onInput NInputAmount
             , class "form-control w-50 d-inline-block ml-4"
             ]
                ++ inputAttrs
            )
            []
        , errorHtml
        ]


{-| TODO: Will probably re-use when we make the Sales admin. At that point we
should move this to the HorizontalForm module.
-}
dateRow : FormErrors -> String -> Html NewMsg
dateRow errors date =
    let
        dateId =
            "Expires"

        fieldErrors =
            Dict.get "expires" errors |> Maybe.withDefault []

        errorHtml =
            if List.isEmpty fieldErrors then
                text ""

            else
                fieldErrors
                    |> List.map text
                    |> List.intersperse (br [] [])
                    |> div [ class "invalid-feedback" ]
    in
    Form.withLabel "Expiration Date"
        True
        [ input
            [ id <| "input" ++ dateId
            , name dateId
            , required True
            , value date
            , type_ "date"
            , onInput NInputExpires
            , class "form-control"
            ]
            []
        , errorHtml
        ]
