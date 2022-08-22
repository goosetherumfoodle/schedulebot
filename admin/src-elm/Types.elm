module Types exposing (..)

import Array exposing (Array)

type alias EditingModal =
    { staffer : Staffer
    , editingField : StafferField
    , ix : StafferIx
    , input : String
    }

type alias Model =
    { notes : List String
    , input : String
    , staffers : Array Staffer
    , selectedStaffer : Maybe (StafferIx, Staffer)
    , stafferFieldHovered : Maybe StafferField
    , editingModal : Maybe EditingModal
    }


type Msg =
      NoOp
    | NewInput String
    | SelectStaffer Int
    | FieldHovered StafferField
    | FieldExited
    | EditField (Maybe (StafferIx, StafferField))
    | ChangeEditingModalInput String
    | EditingModalCancel
    | EditingModalSave
    | ReceivedStaffers String

type alias StafferIx = Int

type alias Staffer =
    { name: String
    , number: String
    , suspendedUntil: Maybe String
    , id: String
    }

type StafferField =
    Name
    | Number
