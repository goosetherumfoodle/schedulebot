module View exposing (view)

import Html exposing (Html)
import Element as EL
import Element.Background as EB
import Element.Border as EBORD
import Element.Input as EI
import Element.Events as EE
import Element.Font as Font
import Array as Array

import Types exposing (..)
import Color as Color

getField : Staffer -> StafferField -> String
getField s field =
    case field of -- can we replace StafferField with access function?
        Number -> s.number
        Name -> s.name

displayField : StafferField -> String
displayField field =
    case field of
        Name -> "Name"
        Number -> "Number"

view : Model -> Html Msg
view model =
    EL.layout
        (editFieldModal model.editingModal)
        ( EL.row
              [ EL.width EL.fill
              , EL.height EL.fill
              ]
              [  EL.indexedTable
                     [ EL.scrollbarY
                     , EL.width <| EL.fillPortion 1
                     , EL.height EL.fill
                     , EB.color Color.tan
                     , EL.alignRight
                     ]

                     { data = Array.toList model.staffers
                     , columns =
                           [{ header = EL.text "Staffer"
                            , width = EL.shrink
                            , view = staffListing
                            }]

                     }

                    , EL.el
                    [ EB.color Color.blue
                    , EL.width <| EL.fillPortion 2
                    , EL.height EL.fill
                    ]
                    <| displayStaffer model
              ]
        )

displayStaffer : Model -> EL.Element Msg
displayStaffer mdl =
    let s = mdl.selectedStaffer
    in
    case s of
        Nothing -> EL.el [] <| EL.text <| "None selected"
        Just (ix, staffer) ->
            EL.column
                [EL.padding 10]
                [ stafferField
                      { field = Name
                      , value = staffer.name
                      , hoverOn = mdl.stafferFieldHovered
                      , ix = ix
                      }
                , stafferField
                      { field = Number
                      , value = staffer.number
                      , hoverOn = mdl.stafferFieldHovered
                      , ix = ix
                      }
                ]

staffListing ix data =
    EL.el [ EL.padding 3
          , EB.color Color.olive
          , EBORD.color Color.brown
          , EBORD.widthXY 0 2
          , EL.pointer
          , EE.onClick <| SelectStaffer ix
          ]
        <| EL.text data.name

editFieldModal : Maybe EditingModal -> List (EL.Attribute Msg) -- mStaffer editInput
editFieldModal mModal = -- model.editingField model.editingModalInput
    case mModal of
        Nothing -> []

        Just {staffer, editingField, ix, input} ->
            [EL.inFront <|
                EL.el
                    [ EB.color Color.gray
                    , EL.centerX
                    , EL.centerY
                    , EL.padding 10
                    ]
                    (EL.column
                         [EL.spacing 10]
                         [ EL.row []
                               [ EL.text <| displayField editingField ++ ": "
                               , EL.text <| getField staffer editingField
                               ]
                         , EI.text
                               [EL.width <| EL.shrink]
                               { label = EI.labelHidden "yo"
                               , onChange = \input_ -> ChangeEditingModalInput input_
                               , placeholder = Just <| EI.placeholder [] <| EL.text <| displayField editingField
                               , text = input
                               }
                         , EL.paragraph []
                             [ EL.text "lots of text ...."
                             , EL.el [ Font.bold ] (EL.text "this is bold")
                             , EL.text "lots of text ...."
                             ]
                         , EL.row
                               [ EL.alignRight
                               , EL.spacing 3
                               ]
                             [ EI.button
                                   [ EBORD.width 1
                                   , EBORD.rounded 3
                                   ]
                                   { label = EL.text "Cancel"
                                   , onPress = Just EditingModalCancel
                                   }

                             , EI.button
                                   [ EBORD.width 1
                                   , EBORD.rounded 3]
                                   { label = EL.text "Save"
                                   , onPress = Just EditingModalSave
                                   }
                             ]
                     ])]

stafferField : {ix: StafferIx, field: StafferField, value: String, hoverOn: Maybe StafferField} -> EL.Element Msg
stafferField deets =
    let styles =
            [ EBORD.color Color.maroon
            , EBORD.widthXY 2 1
            , EE.onMouseEnter <| FieldHovered deets.field
            , EE.onMouseLeave <| FieldExited
            , EL.pointer
            , EE.onClick <| EditField <| Just (deets.ix, deets.field)
            ]
        attrs =
            case deets.hoverOn of
                Nothing -> styles
                Just hovered ->
                    if hovered == deets.field then
                        EL.onRight (EL.el
                                      [ EB.color Color.pink
                                      , Font.size 15
                                      ]
                                      (EL.text "edit?"))
                            :: styles
                    else
                        styles
    in
    EL.row
        attrs
        [ EL.text <| (displayField deets.field) ++ ":"
        , EL.text deets.value
        ]
