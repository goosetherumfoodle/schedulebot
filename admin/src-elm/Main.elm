module Main exposing (..)

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
        )
import Html.Attributes
    exposing
        ( src
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
import Bootstrap.CDN as CDN
import Graphics exposing (barbellSvg, addSvgPlates)
import Types exposing (..)
import Browser


-- todo: make weight the combined of the pairs? in plate list
-- todo: re-organize


barWeight : Int
barWeight =
    45


maxPlatePairs : Int
maxPlatePairs =
    6


allPlates : List PlateWeight
allPlates =
    [ FortyFive
    , ThirtyFive
    , TwentyFive
    , Ten
    , Five
    , TwoPointFive
    ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { total = barWeight
      , target = 0
      , outcome = Incomplete
      , plates = []
      , maxTarget = weightWithTwo45s maxPlatePairs
      }
    , newTarget <| weightWithTwo45s maxPlatePairs
    )



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlates weight ->
            ( addPlates weight model, Cmd.none )

        Reset ->
            ( { model
                | total = 0
                , outcome = Incomplete
                , plates = []
              }
            , newTarget model.maxTarget
            )

        Target newTarget_ ->
            ( { model | target = newTarget_ }, Cmd.none )

        Test ->
            let
                newModel =
                    sumTotal model
            in
                if newModel.total == newModel.target then
                    ( { newModel | outcome = Correct }, Cmd.none )
                else
                    ( { newModel | outcome = Incorrect }, Cmd.none )

        Undo ->
            case model.outcome of
                Incomplete ->
                    ( { model | plates = drop 1 model.plates }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MaxTarget maxTargetInput ->
            let
                newMaxTarget =
                    selectedOrDefaultTarget maxTargetInput
            in
                ( { model | maxTarget = newMaxTarget }, newTarget newMaxTarget )


selectedOrDefaultTarget : String -> Int
selectedOrDefaultTarget selected =
    case String.toInt selected of
        Just int ->
            int

        Nothing ->
            weightWithTwo45s maxPlatePairs


addPlates : PlateWeight -> Model -> Model
addPlates weight model =
    case model.outcome of
        Incomplete ->
            addSvgPlates maxPlatePairs weight model

        _ ->
            model


sumTotal : Model -> Model
sumTotal model =
    { model
        | total =
            sum <|
                (::) barWeight <|
                    map
                        (round << (*) 2 << toNumber << .weight)
                        model.plates
    }


toNumber : PlateWeight -> Float
toNumber weight =
    case weight of
        FortyFive ->
            45

        ThirtyFive ->
            35

        TwentyFive ->
            25

        Ten ->
            10

        Five ->
            5

        TwoPointFive ->
            2.5


newTarget : Int -> Cmd Msg
newTarget maxTarget =
    generate Target <| targetGenerator maxTarget


targetGenerator : Int -> Generator Int
targetGenerator maxTarget =
    Random.map (divisibleBy 5) <| int barWeight maxTarget


divisibleBy : Int -> Int -> Int
divisibleBy x y =
    y - (modBy y x) + x



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container text-center" ]
        [ CDN.stylesheet
        , h1 [] [ text "Plate Math Quiz" ]
        , barbell model.plates
        , div [ class "row" ]
            [ div
                [ class "col"
                , style "font-size" "1.5rem"
                ]
                [ text <| "Target: " ++ String.fromInt model.target ++ " lbs"
                , outcomeDiv model.outcome model.total
                ]
            , commandButtons model.outcome
            ]
        , plateRackDiv model.outcome allPlates
        ]


commandButtons : Outcome -> Html Msg
commandButtons outcome =
    div [ class "col" ]
        [ checkButton outcome
        , undoButton outcome
        , resetButton
        , maxTargetSelect
        ]


resetButton : Html Msg
resetButton =
    button
        [ onClick Reset
        , class "btn btn-danger"
        ]
        [ text "Reset" ]


undoButton : Outcome -> Html Msg
undoButton outcome =
    disableableButton outcome
        [ onClick Undo
        , class "btn btn-warning"
        ]
        (text "Undo")


checkButton : Outcome -> Html Msg
checkButton outcome =
    disableableButton outcome
        [ onClick Test
        , class "btn btn-primary"
        ]
        (text "Check")


maxTargets : List Int
maxTargets =
    map weightWithTwo45s (range 1 maxPlatePairs)


weightWithTwo45s : Int -> Int
weightWithTwo45s numPlates =
    numPlates * round (toNumber FortyFive * 2) + barWeight


maxTargetSelect : Html Msg
maxTargetSelect =
    select
        [  onInput MaxTarget
        ]
    <|
        placeholderOption
            :: map maxTargetOption maxTargets


placeholderOption : Html Msg
placeholderOption =
    option [ disabled True, selected True ] [ text "Up To" ]


maxTargetOption : Int -> Html Msg
maxTargetOption max =
    option [] [ text <| String.fromInt max ]


barbell : List PlatePair -> Html Msg
barbell plates =
    div [] [ barbellSvg plates ]


disableableButton : Outcome -> List (Html.Attribute Msg) -> Html Msg -> Html Msg
disableableButton outcome style txt =
    case outcome of
        Incomplete ->
            button style [ txt ]

        _ ->
            button (disabled True :: style) [ txt ]


outcomeDiv : Outcome -> Int -> Html Msg
outcomeDiv outcome total =
    let
        displayTotal =
            text <| "Total: " ++ String.fromInt total
    in
        case outcome of
            Incomplete ->
                div [] [ text "" ]

            Correct ->
                div [ style  "color" "green" ]
                    [ displayTotal ]

            Incorrect ->
                div [ style  "color" "red" ]
                    [ displayTotal ]


plateRackDiv : Outcome -> List PlateWeight -> Html Msg
plateRackDiv outcome weights =
    div []
        [ hr [] []
        , h3 [] [ text "Add Plates" ]
        , div
            [ class "btn-group-lg"
            , style "role" "group"
            ]
          <|
            List.map (rackedPlateDiv outcome) weights
        ]


rackedPlateDiv : Outcome -> PlateWeight -> Html Msg
rackedPlateDiv outcome weight =
    disableableButton outcome
        [ class "btn btn-secondary"
        , onClick <| AddPlates weight
        ]
        (text << String.fromFloat <| toNumber weight)
