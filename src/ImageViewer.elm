module ImageViewer
    exposing
        ( imageViewerHtml
        , Geometry
        , initGeometry
        , MouseEvents
        , handleMouseMove
        , handleZoom
        , TouchState
        , initTouchState
        , handleTouchStartEnd
        , handleTouchMove
        )

import Css
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on, onWithOptions, defaultOptions)
import Json.Decode exposing (..)
import Mouse
import Scroll
import Touch
import MultiTouch
import Dict

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
    , touchStart: Touch.Event -> msg
    , touchMove: Touch.Event -> msg
    , touchEnd: Touch.Event -> msg
    -- This is unused, but needed for preventing the default behaviour of images
    , downMsg: msg
    }

type alias TouchState =
    { touches: Dict.Dict Int Touch.Coordinates
    }

initTouchState : TouchState
initTouchState =
    TouchState Dict.empty


handleTouchStartEnd : Touch.Event -> TouchState -> TouchState
handleTouchStartEnd event state =
    { state | touches = event.touches }

handleTouchMove : Touch.Event -> TouchState -> Geometry -> (TouchState, Geometry)
handleTouchMove event state geometry =
    let
        -- Calculate the movement of all the touches

        calculateMovement id touch =
            let
                (newX, newY) = Touch.clientPos touch
                -- We should be able to safely use default here because assuming
                -- onStart and onEnd events are handled, the TouchState should match
                -- The new touches
                (oldX, oldY) = Touch.clientPos <| Maybe.withDefault touch <| Dict.get id state.touches
            in
                (newX - oldX, newY-oldY)

        movement = Dict.map calculateMovement event.touches

        newGeometry = case Dict.toList movement of
            [(id, moved)] ->
                moveGeometry (fromTuple moved) geometry
            [] -> --This should never happen but it needs to be handled
                geometry
            movementList ->
                geometry
    in
        ({state | touches = event.touches}, newGeometry)


handleMouseMove : Mouse.Event -> Geometry -> Geometry
handleMouseMove event geometry =
    let
        (movedX, movedY) = event.movement
        moved =
            vec2 movedX movedY
    in
        case List.member Mouse.Left event.buttons of
            True ->
                moveGeometry moved geometry
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


moveGeometry : Vec2 -> Geometry -> Geometry
moveGeometry moved geometry =
    {geometry | position = (add geometry.position (scale -1 moved))}


imageViewerHtml : msg -> Vec2 -> Geometry -> String -> MouseEvents msg -> Html msg
imageViewerHtml onLoaded containerSize {position, zoom} filename events =
    let
        {moveMsg, downMsg, scrollMsg, touchStart, touchMove, touchEnd} = events

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
                , MultiTouch.onStart touchStart
                , MultiTouch.onMove touchMove
                , MultiTouch.onEnd touchEnd
                , Scroll.onScroll scrollMsg
                ]
                []
            ]


onLoadSrc : msg -> Html.Attribute msg
onLoadSrc msg =
    on "load" (Json.Decode.succeed msg)


