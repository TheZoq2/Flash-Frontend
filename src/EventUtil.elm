module EventUtil exposing (onPreventDefault)

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode


alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg =
    (msg, True)


onPreventDefault : String -> Json.Decode.Decoder msg -> Attribute msg
onPreventDefault event msg =
    preventDefaultOn event (Json.Decode.map alwaysPreventDefault msg)
