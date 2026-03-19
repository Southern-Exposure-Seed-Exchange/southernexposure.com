module Checkout.View exposing (PopupConfig, TextData, addPopupContainerView, failedAddressVerificationPopupView, textData)

import Components.Address.Address as Address
import Components.Button as Button exposing (defaultButton)
import Data.Locations exposing (AddressLocations)
import Html exposing (..)
import Html.Attributes exposing (..)



---------------------------------------------------------
-- Types
---------------------------------------------------------


type alias PopupConfig msg =
    { onEdit : msg
    , onContinue : msg
    , address : Address.Form
    , locations : AddressLocations
    }


type alias TextData =
    { title : String
    , descriptionLine1 : String
    , descriptionLine2 : String
    , shippingDetailsHeader : String
    , editAddressLabel : String
    , continueAnywayLabelMobile : String
    , continueAnywayLabelDesktop : String
    }


textData : TextData
textData =
    { title = "We couldn't automatically verify this address"
    , descriptionLine1 = "An unverified address is not necessarily incorrect, but it may cause delivery issues."
    , descriptionLine2 = "Please check the address again or continue if you're sure it's correct."
    , shippingDetailsHeader = "Shipping Details"
    , editAddressLabel = "Edit address"
    , continueAnywayLabelMobile = "Continue"
    , continueAnywayLabelDesktop = "Continue anyway"
    }



---------------------------------------------------------
-- Views
---------------------------------------------------------


addPopupContainerView : Html msg -> Html msg
addPopupContainerView content =
    div [ class "tw:fixed tw:inset-0 tw:z-50 tw:flex tw:items-center tw:justify-center tw:bg-[#1E0C03]/50 tw:p-[16px]" ]
        [ content ]


failedAddressVerificationPopupView : PopupConfig msg -> Html msg
failedAddressVerificationPopupView config =
    div [ class "tw:relative tw:w-full tw:max-w-[696px] tw:rounded-[16px] tw:bg-white tw:p-[32px] tw:shadow-xl" ]
        [ -- Title
          div [ class "tw:pb-[24px] tw:pr-[24px]" ]
            [ h2 [ class "tw:!text-[24px] tw:!font-[600] tw:!leading-[32px] tw:!text-[#1E0C03]" ]
                [ text textData.title ]
            ]

        -- Description
        , div [ class "tw:pb-[16px]" ]
            [ p [ class "tw:!text-[16px] tw:!font-[400] tw:!leading-[24px] tw:!text-[#1E0C03]" ]
                [ text textData.descriptionLine1
                , br [] []
                , strong [ class "tw:!font-[600]" ] [ text textData.descriptionLine2 ]
                ]
            ]

        -- Shipping Details Box
        , div [ class "tw:pb-[32px]" ]
            [ div [ class "tw:rounded-[8px] tw:bg-[#F9F9F9] tw:p-[24px]" ]
                [ div [ class "tw:pb-[16px]" ]
                    [ h3 [ class "tw:!text-[14px] tw:!font-[400] tw:opacity-70" ] [ text textData.shippingDetailsHeader ] ]
                , div [ class "tw:text-[16px] tw:leading-[24px] tw:text-[#1E0C03]" ]
                    [ Address.card config.address.model config.locations ]
                ]
            ]

        -- Actions
        , div [ class "tw:flex tw:gap-[12px] tw:justify-center" ]
            [ Button.view
                { defaultButton
                    | label = textData.editAddressLabel
                    , style = Button.Outline
                    , type_ = Button.TriggerMsg config.onEdit
                    , size = Button.Large
                    , padding = Button.Width "tw:w-1/2 tw:lg:w-[194px]"
                }
            , Button.view
                { defaultButton
                    | label = ""
                    , icon =
                        Just <|
                            div []
                                [ span [ class "tw:inline tw:lg:hidden" ] [ text textData.continueAnywayLabelMobile ]
                                , span [ class "tw:hidden tw:lg:inline" ] [ text textData.continueAnywayLabelDesktop ]
                                ]
                    , style = Button.Solid
                    , type_ = Button.TriggerMsg config.onContinue
                    , size = Button.Large
                    , padding = Button.Width "tw:w-1/2 tw:lg:w-[194px]"
                }
            ]
        ]
