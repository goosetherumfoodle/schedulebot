module Main exposing (..)

import Debug as DBG
import Element as EL
import Element.Background as EB
import Element.Border as EBORD
import Element.Input as EI
import Element.Events as EE
import Element.Font as Font
import Html exposing ( Html )
import Html.Attributes
    exposing
        ( src
        , placeholder
        , style
        , class
        , disabled
        , value
        , name
        , selected
        , href
        , alt
        )
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator, generate, int)
import List exposing (tail, sum, map, drop, range)
import Array
import Types exposing (..)
import Browser
import Ports

staff = [ { name = "Jesse", number = "412.232.5326", suspendedUntil = Nothing  }
        , { name = "Lars", number = "412.326.5837", suspendedUntil = Nothing  }
        , { name = "Aerin", number = "412.883.9592", suspendedUntil = Nothing  }
        , { name = "Wick", number = "412.121.1938", suspendedUntil = Nothing  }
        , { name = "Bret", number = "412.849.5839", suspendedUntil = Nothing  }
        , { name = "Angel", number = "412.472.5873", suspendedUntil = Nothing  }
        , { name = "Stephen", number = "412.583.1934", suspendedUntil = Nothing  }
        , { name = "Jack", number = "412.283.4932", suspendedUntil = Nothing  }
        ]



initModel : flags -> ( Model, Cmd Msg )
initModel _ =
    ({ notes = []
     , input = ""
     , selectedStaffer = Nothing
     , staffers = Array.fromList staff
     , stafferFieldHovered = Nothing
     , editingModal = Nothing
     } , Cmd.none )



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = initModel
        , update = update
        , subscriptions = Ports.subscriptions
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)

        SelectStaffer ix ->
            ( {model | selectedStaffer = Maybe.map (\s -> (ix, s)) <| Array.get ix model.staffers}
            , Cmd.none
            )

        Received recv ->
            ( { model | notes = recv :: model.notes }
            , Cmd.none )

        Send out -> (model, Ports.greet out)

        NewInput input_ ->
            ({model | input = (Debug.log "newinput: " input_)}
            , Cmd.none
            )

        SendIt ->
            ({model | input = ""}, Ports.greet model.input)

        FieldHovered hovered ->
            ({model | stafferFieldHovered = Just hovered }
            , Cmd.none
            )

        FieldExited ->
            ({model | stafferFieldHovered = Nothing }
            , Cmd.none
            )


        EditField mfield ->
            case mfield of
                Nothing -> ({model | editingModal = Nothing}, Cmd.none)

                Just (ix, field) ->
                    case Array.get ix model.staffers of
                        Nothing -> ({model | editingModal = Nothing}, Cmd.none)

                        Just staffer ->
                            let newEditingModal =
                                    { editingField = field
                                    , ix = ix
                                    , input = ""
                                    , staffer = staffer
                                    }
                            in
                                ({model | editingModal = Just newEditingModal}
                                , Cmd.none
                                )

        ChangeEditingModalInput input ->
            case model.editingModal of
                Nothing -> (model, Cmd.none)

                Just editingModal ->
                    let newEditingModal = {editingModal | input = input}
                    in
                        ({model | editingModal = Just newEditingModal}
                        , Cmd.none
                        )

        EditingModalCancel ->
            ({model | editingModal = Nothing}, Cmd.none)

        EditingModalSave ->
            case model.editingModal of
                Nothing -> (model, Cmd.none)

                Just editingModal ->
                    let newStaffer = updateStaffer editingModal.staffer editingModal.editingField editingModal.input
                        newStaffers = Array.set editingModal.ix newStaffer model.staffers
                    in
                        ({ model
                             | editingModal = Nothing
                             , staffers = newStaffers
                             , selectedStaffer = Just (editingModal.ix, newStaffer)
                         }, Cmd.none)

updateStaffer : Staffer -> StafferField -> String -> Staffer
updateStaffer s f input =
    case f of
        Number -> { s | number = input }
        Name -> { s | name = input }

---- VIEW ----

-- tan = "e4 e6 c3"
tan = EL.rgb255 228 230 195

-- blue = "2e 61 71"
blue = EL.rgb255 46 152 120

-- olive = "89 98 78"
olive = EL.rgb255 137 152 120

-- brown = "7a 54 2e"
brown = EL.rgb255 122 84 46

-- maroon = "6f 1d 1b"
maroon = EL.rgb255 11 29 27

green = EL.rgb255 50 168 82

pink = EL.rgb255 235 89 225

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
                     , EB.color tan
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
                    [ EB.color blue
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


stafferField : {ix: StafferIx, field: StafferField, value: String, hoverOn: Maybe StafferField} -> EL.Element Msg
stafferField deets =
    let styles =
            [ EBORD.color maroon
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
                                      [ EB.color pink
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

staffListing ix data =
    EL.el [ EL.padding 3
          , EB.color olive
          , EBORD.color brown
          , EBORD.widthXY 0 2
          , EL.pointer
          , EE.onClick <| SelectStaffer ix
          ]
        <| EL.text data.name

displayField : StafferField -> String
displayField field =
    case field of
        Name -> "Name"
        Number -> "Number"

editFieldModal : Maybe EditingModal -> List (EL.Attribute Msg) -- mStaffer editInput
editFieldModal mModal = -- model.editingField model.editingModalInput
    case mModal of
        Nothing -> []

        Just {staffer, editingField, ix, input} ->
            [EL.inFront <|
                EL.el
                    [ EB.color gray
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
                               { label = label
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

gray : EL.Color
gray = EL.rgb255 176 176 176

label : EI.Label msg
label = EI.labelHidden "oof"

getField : Staffer -> StafferField -> String
getField s field =
    case field of -- can we replace StafferField with access function?
        Number -> s.number
        Name -> s.name
