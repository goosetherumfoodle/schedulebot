port module Ports exposing (..)

import Types exposing (..)

port greet : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Received
