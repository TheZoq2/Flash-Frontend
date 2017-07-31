module Touch exposing
    ( Event
    )

import Html
import Html.Events exposing (..)

import Json.Decode as Decode exposing (Decoder)

type alias Touch =
    { identifier: Int
    , clientX: Int
    , clientY: Int
    }

type alias Event =
    { touches: List Touch
    , changedTouches: List Touch
    }


stopOptions : Options
stopOptions =
    { stopPropagation = True
    , preventDefault = True
    }


onTouchStart : (Event -> msg) -> Html.Attribute msg
onTouchStart msg =
    Decode.map msg eventDecoder
        |> onWithOptions "touchstart" stopOptions

onTouchEnd : (Event -> msg) -> Html.Attribute msg
onTouchEnd msg =
    Decode.map msg eventDecoder
        |> onWithOptions "touchend" stopOptions





-- Decoders
touchDecoder : Decoder Touch
touchDecoder =
    Decode.map3 Touch
        (Decode.field "identifier" Decode.int)
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)


eventDecoder : Decoder Event
eventDecoder =
    Decode.map2 Event
        (Decode.field "touches" <| Decode.list touchDecoder)
        (Decode.field "changedTouches" <| Decode.list touchDecoder)




