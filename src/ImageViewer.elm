module ImageViewer
    exposing
        ( imageViewerHtml
        , Geometry
        , initGeometry
        , MouseEvents
        , handleMouseMove
        , handleZoom
        )

import Css
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on, onWithOptions, defaultOptions)
import Json.Decode exposing (..)
import Mouse
import Scroll

import Math.Vector2 exposing (..)

import Style

type alias Geometry =
    { position: Vec2
    , zoom: Float
    }

initGeometry : Geometry
initGeometry =
    Geometry (vec2 0 0) 1

type alias MouseEvents msg =
    { moveMsg: Mouse.Event -> msg
    , scrollMsg: Scroll.Event -> msg
    -- This is unused, but needed for preventing the default behaviour of images
    , downMsg: msg
    }

handleMouseMove : Mouse.Event -> Geometry -> Geometry
handleMouseMove event geometry =
    let
        (movedX, movedY) = event.movement
        moved =
            vec2 -movedX -movedY
    in
        case List.member Mouse.Left event.buttons of
            True ->
                {geometry | position = (add geometry.position moved)}
            False ->
                geometry


handleZoom : Float -> Vec2 -> Geometry -> Geometry
handleZoom scaling clientXY geometry =
    let
        zoom = geometry.zoom * scaling

        newPosition =
            Math.Vector2.sub (scale scaling (add clientXY geometry.position)) clientXY 
    in
        Geometry newPosition zoom


imageViewerHtml : msg -> Vec2 -> Geometry -> String -> MouseEvents msg -> Html msg
imageViewerHtml onLoaded containerSize {position, zoom} filename {moveMsg, downMsg, scrollMsg} =
    let
        (x, y) =
            toTuple position

        (w, h) =
            toTuple containerSize

        containerCss = 
            [ Css.width <| Css.px (w * zoom)
            , Css.height <| Css.px (h * zoom)
            , Css.left <| Css.px -x
            , Css.top <| Css.px -y
            , Css.position Css.relative
            ]

        mouseDownOptions =
            {defaultOptions | preventDefault = True}
    in
        div [Style.toStyle containerCss]
            [ img
                [ Style.class [Style.ImageViewerImage]
                , src filename
                , onLoadSrc onLoaded
                , Mouse.onMove moveMsg
                , onWithOptions "mousedown" mouseDownOptions (Json.Decode.succeed downMsg)
                , Scroll.onScroll scrollMsg
                ]
                []
            ]


onLoadSrc : msg -> Html.Attribute msg
onLoadSrc msg =
    on "load" (Json.Decode.succeed msg)


