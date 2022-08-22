module Util exposing (getField, displayField)

import Types exposing ( Staffer, StafferField(..) )

getField : Staffer -> StafferField -> String
getField s field =
    case field of -- can we replace StafferField with access function?
        Number -> s.number
        Name -> s.name

displayField : StafferField -> String
displayField field =
    case field of
        Name -> "Name"
        Number -> "Number"
