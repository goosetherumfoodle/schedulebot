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
import Array exposing (Array)
import Types exposing (..)
import Browser
import Ports as Ports
import Util exposing (getField, displayField)
import Json.Encode as Enc
import Json.Decode as Dec exposing (Decoder)

import View exposing (view)

initModel : flags -> ( Model, Cmd Msg )
initModel _ =
    ({ notes = []
     , input = ""
     , selectedStaffer = Nothing
     , staffers = Array.empty
     , stafferFieldHovered = Nothing
     , editingModal = Nothing
     }
    , Ports.getStaffers ()
    )



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
            case model.editingModal of
                Just _ -> (model, Cmd.none)

                Nothing ->
                    ( {model | selectedStaffer = Maybe.map (\s -> (ix, s)) <| Array.get ix model.staffers}
                    , Cmd.none
                    )

        NewInput input_ ->
            ({model | input = (Debug.log "newinput: " input_)}
            , Cmd.none
            )

        FieldHovered hovered ->
            case model.editingModal of
                Just _ ->
                    (model, Cmd.none)

                Nothing ->
                    ({model | stafferFieldHovered = Just hovered }
                    , Cmd.none
                    )

        FieldExited ->
            case model.editingModal of
                Just _ ->
                    (model, Cmd.none)

                Nothing ->
                    ({model | stafferFieldHovered = Nothing }
                    , Cmd.none
                    )


        EditField mfield ->
            case model.editingModal of
                Just _ ->
                    (model, Cmd.none)

                Nothing ->
                    case mfield of
                        Nothing -> ({model | editingModal = Nothing}, Cmd.none)

                        Just (ix, field) ->
                            case Array.get ix model.staffers of
                                Nothing -> ({model | editingModal = Nothing}, Cmd.none)

                                Just staffer ->
                                    let newEditingModal =
                                            { editingField = field
                                            , ix = ix
                                            , input = getField staffer field
                                            , staffer = staffer
                                            }
                                    in
                                        ({model
                                             | editingModal = Just newEditingModal
                                             , stafferFieldHovered = Nothing
                                         }
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
                         }
                        , Ports.saveStafferField
                             <| stafferFieldJson
                                 editingModal.staffer
                                 editingModal.editingField
                                 editingModal.input
                        )

        ReceivedStaffers json ->
            Debug.log json <|
            ({model | staffers = parseStaffers json}, Cmd.none)

parseStaffers : String -> Array Staffer
parseStaffers json =
    case Dec.decodeString staffersDecoder json of
        Err err -> Debug.log ("PARSE ERROR: " ++ (Debug.toString err)) <| Array.empty
        Ok staffers -> staffers

staffersDecoder : Decoder (Array Staffer)
staffersDecoder = Dec.array stafferDecoder

stafferDecoder : Decoder Staffer
stafferDecoder =
    Dec.map4 Staffer
        (Dec.field "name" Dec.string)
        (Dec.field "number" Dec.string)
        (Dec.maybe (Dec.field "suspendedUntil" Dec.string))
        (Dec.field "id" Dec.string)


stafferFieldJson : Staffer -> StafferField -> String -> String
stafferFieldJson staffer field val =
    Enc.encode 0
        <| Enc.object
            [ ("id", Enc.string staffer.id)
            , ("field", Enc.string <| displayField field)
            , ("value", Enc.string val)
            ]


updateStaffer : Staffer -> StafferField -> String -> Staffer
updateStaffer s f input =
    case f of
        Number -> { s | number = input }
        Name -> { s | name = input }
