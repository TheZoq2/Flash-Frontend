module Scroll exposing
    ( Event
    , onScroll
    )

import Html.Styled
import Html.Styled.Events exposing (preventDefaultOn)
import Json.Decode as Decode exposing (Decoder)

import Mouse


type alias Event =
    { deltaX: Float
    , deltaY: Float
    , deltaZ: Float
    , deltaMode: Int
    , mouseEvent: Mouse.Event
    }



onScroll : (Event -> msg) -> Html.Styled.Attribute msg
onScroll msg =
    let
        alwaysPreventDefault msg_ = (msg_, True)
    in
        preventDefaultOn "wheel" (Decode.map alwaysPreventDefault <| Decode.map msg scrollDecoder)


scrollDecoder : Decoder Event
scrollDecoder =
    Decode.map5 Event
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "deltaZ" Decode.float)
        (Decode.field "deltaMode" Decode.int)
        Mouse.eventDecoder

