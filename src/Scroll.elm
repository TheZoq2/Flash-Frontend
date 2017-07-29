module Scroll exposing
    ( Event
    , onScroll
    )

import Html
import Html.Events exposing (onWithOptions, defaultOptions)
import Mouse
import Json.Decode as Decode exposing (Decoder)


type alias Event =
    { deltaX: Float
    , deltaY: Float
    , deltaZ: Float
    , deltaMode: Int
    , mouseEvent: Mouse.Event
    }



onScroll : (Event -> msg) -> Html.Attribute msg
onScroll msg =
    let
        options = {defaultOptions | preventDefault = True}
    in
        onWithOptions "wheel" options (Decode.map msg scrollDecoder)


scrollDecoder : Decoder Event
scrollDecoder =
    Decode.map5 Event
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "deltaZ" Decode.float)
        (Decode.field "deltaMode" Decode.int)
        Mouse.eventDecoder

