port module Ports exposing (..)

import Types exposing (..)


port receiveStaffers : (String -> msg) -> Sub msg

port getStaffers : () -> Cmd msg
port saveStafferField : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  receiveStaffers ReceivedStaffers
