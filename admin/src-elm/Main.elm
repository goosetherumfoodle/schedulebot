module Main exposing (..)

import Debug as DBG
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

import View exposing (view)

initStaff = [ { name = "Jesse", number = "412.232.5326", suspendedUntil = Nothing  }
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
     , staffers = Array.fromList initStaff
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
