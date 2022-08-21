module Types exposing (..)


type alias Model =
    {
        notes : List String
    ,   input : String
    }

type Outcome = Correct | Incorrect | Incomplete


type Msg
    =
      Send String
    | Received String
    | SendIt
    | NewInput String
    | NoOp
