module Main exposing (..)

import Debug
import Element as EL
import Element.Input as EI
import Html
    exposing
        ( Html
        , text
        , div
        , h1
        , h3
        , button
        , hr
        , select
        , option
        , label
        , a
        , img
        , textarea
        )
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
import Types exposing (..)
import Browser
import Ports


initModel : flags -> ( Model, Cmd Msg )
initModel _ =
    ( { notes = [], input = ""} , Cmd.none )



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

---- VIEW ----


view : Model -> Html Msg
view model =
    EL.layout []
        (EL.column []
    [
    -- , input [placeholder "name", value model.input, onInput NewInput ] []
     EI.text
        []
        {placeholder = Nothing
        , onChange = NewInput
        , label = EI.labelHidden "name input"
        , text = model.input
        }
        , EI.multiline [] { text = (List.foldr (++) "" model.notes)
                          , placeholder = Nothing
                          , onChange = \_ -> NoOp
                          , spellcheck = False
                          , label = EI.labelHidden ""
                          }
        , EL.el [] (EI.button [] { label = EL.text "button"
                                 , onPress = Just SendIt
                                 })

        ])
