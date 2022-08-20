module Graphics exposing (barbellSvg, addSvgPlates)

import Html exposing (Html)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, rx, ry, x, y)
import List exposing (concat, head)
import Types exposing (..)


plateBuffer : Float
plateBuffer =
    0


barbellSvg : List PlatePair -> Html Msg
barbellSvg plates =
    svg
        [ viewBox "0 0 400 120"
        ]
    <|
        concat
            [ emptyBar
            , List.map (plateRect << .leftPlate) plates
            , List.map (plateRect << .rightPlate) plates
            ]


plateRect : PlateSvgAttrs -> Svg Msg
plateRect attrs =
    rect
        [ x <| String.fromFloat attrs.x
        , y <| String.fromFloat attrs.y
        , width <| String.fromFloat attrs.width
        , height <| String.fromFloat attrs.height
        , rx "2"
        , ry "3"
        ]
        []


emptyBar : List (Svg Msg)
emptyBar =
    [ rect [ x "0", y "58", width "400", height "5", rx "1", ry "2" ] []
    , rect [ x "330", y "52", width "10", height "18", rx "1", ry "2" ] []
    , rect [ x "70", y "52", width "10", height "18", rx "1", ry "2" ] []
    , rect [ x "329", y "56", width "70", height "9", rx "1", ry "2" ] []
    , rect [ x "0", y "56", width "71", height "9", rx "1", ry "2" ] []
    ]


addSvgPlates : Int -> PlateWeight -> Model -> Model
addSvgPlates maxPairs weight model =
    if roomForMorePlates maxPairs model.plates then
        let
            newPlates =
                buildPlatePair (carPlates model.plates) weight
        in
            { model | plates = consPlates newPlates model.plates }
    else
        model


buildPlatePair : Maybe PlatePair -> PlateWeight -> PlatePair
buildPlatePair prevPlates weight =
    case prevPlates of
        Nothing ->
            { leftPlate = buildLeftPlate Nothing weight
            , rightPlate = buildRightPlate Nothing weight
            , weight = weight
            }

        Just pair ->
            { leftPlate = buildLeftPlate (Just pair.leftPlate) weight
            , rightPlate = buildRightPlate (Just pair.rightPlate) weight
            , weight = weight
            }


buildLeftPlate : Maybe PlateSvgAttrs -> PlateWeight -> PlateSvgAttrs
buildLeftPlate prevPlate weight =
    case prevPlate of
        Nothing ->
            { x = 70 - calcWidth weight
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }

        Just plate ->
            { x = plate.x - plateBuffer - calcWidth weight
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }


buildRightPlate : Maybe PlateSvgAttrs -> PlateWeight -> PlateSvgAttrs
buildRightPlate prevPlate weight =
    case prevPlate of
        Nothing ->
            { x = 340
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }

        Just plate ->
            { x = plate.x + plate.width + plateBuffer
            , y = calcY weight
            , height = calcHeight weight
            , width = calcWidth weight
            }


calcHeight : PlateWeight -> Float
calcHeight weight =
    case weight of
        FortyFive ->
            120

        ThirtyFive ->
            100

        TwentyFive ->
            70

        Ten ->
            50

        Five ->
            30

        TwoPointFive ->
            28


calcY : PlateWeight -> Float
calcY weight =
    case weight of
        FortyFive ->
            0

        ThirtyFive ->
            10

        TwentyFive ->
            25

        Ten ->
            35

        Five ->
            45

        TwoPointFive ->
            46


calcWidth : PlateWeight -> Float
calcWidth weight =
    case weight of
        FortyFive ->
            10

        ThirtyFive ->
            10

        TwentyFive ->
            9

        Ten ->
            6

        Five ->
            5

        TwoPointFive ->
            2


roomForMorePlates : Int -> List PlatePair -> Bool
roomForMorePlates max pairs =
    if List.length pairs < max then
        True
    else
        False


carPlates : List PlatePair -> Maybe PlatePair
carPlates =
    head


consPlates : PlatePair -> List PlatePair -> List PlatePair
consPlates =
    (::)
