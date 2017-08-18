module ImageViewer
    exposing
        ( imageViewerHtml
        , Geometry
        , initGeometry
        , MouseEvents
        , handleMouseMove
        , zoomGeometry
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

{-|
  Tracks the zoom level and position of an image
-}
type alias Geometry =
    { position: Vec2
    , zoom: Float
    }

initGeometry : Geometry
initGeometry =
    Geometry (vec2 0 0) 1

{-|
  Tracks the current touches on the image
-}
type alias TouchState =
    { touches: Dict.Dict Int Touch.Coordinates
    }

{-|
  Convenience alias for a type storing a touch identifier and its corresponding
  coordinate
-}
type alias Touches = Dict.Dict Int Vec2

initTouchState : TouchState
initTouchState =
    TouchState Dict.empty

{-|
  Converts a `Touch.Coordinat` to a `Math.Vector2.Vec2`
-}
touchCoordToVec : Touch.Coordinates -> Vec2
touchCoordToVec coord =
    fromTuple <| Touch.clientPos coord


{-|
  Handles touch start and end events
-}
handleTouchStartEnd : Touch.Event -> TouchState -> TouchState
handleTouchStartEnd event state =
    { state | touches = event.touches }


{-|
  Handles touch move events updating the current touch state and geometry
-}
handleTouchMove : Touch.Event -> TouchState -> Geometry -> (TouchState, Geometry)
handleTouchMove event state geometry =
    let
        touchesToCoords {touches} =
            Dict.map (\id coord -> touchCoordToVec coord) touches

        oldTouches =
            touchesToCoords state
        newTouches =
            touchesToCoords event
    in
        ({state | touches = event.touches}, handleTouchMoveVec oldTouches newTouches geometry)

{-|
  Handles touch moves without caring about touch state and with Touch.Coordinate converted to Vec
-}
handleTouchMoveVec : Touches -> Touches -> Geometry -> Geometry
handleTouchMoveVec oldTouches newTouches geometry =
    case Dict.toList newTouches of
        [(id, coordinates)] ->
            handleSingletouchMove
                (Maybe.withDefault coordinates <| Dict.get id oldTouches)
                (coordinates)
                geometry
        [] -> --This should never happen but it needs to be handled
            geometry
        _ ->
            handleMultitouchMove oldTouches newTouches geometry

{-|
  Handles updating the geometry on single finger touches
-}
handleSingletouchMove : Vec2 -> Vec2 -> Geometry -> Geometry
handleSingletouchMove oldPosition newPosition geometry =
    moveGeometry (Math.Vector2.sub newPosition oldPosition) geometry

{-|
  Handles updating the geometry on multi-finger touches
-}
handleMultitouchMove : Touches -> Touches ->  Geometry -> Geometry
handleMultitouchMove oldTouches newTouches geometry =
    case Dict.toList newTouches of
        [(id1, newFirst), (id2, newSecond)] ->
            let
                oldFirst = Maybe.withDefault newFirst <| Dict.get id1 oldTouches
                oldSecond = Maybe.withDefault newSecond <| Dict.get id2 oldTouches

                oldDistance = Math.Vector2.distance oldFirst oldSecond
                newDistance = Math.Vector2.distance newFirst newSecond

                oldCenter = add oldFirst (Math.Vector2.sub oldSecond oldFirst)
                center = add newFirst (Math.Vector2.sub newSecond newFirst)
            in
                moveGeometry (Math.Vector2.sub center oldCenter)
                    <| zoomGeometry (newDistance / oldDistance) center geometry
        _ -> -- We only care about the pinch gesture
            geometry



{-|
  Handles mousemove events
-}
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


{-|
  Zooms the specified geometry by scaling amount keeping pivot in the center of the screen
-}
zoomGeometry : Float -> Vec2 -> Geometry -> Geometry
zoomGeometry scaling pivot geometry =
    let
        zoom = geometry.zoom * scaling

        newPosition =
            Math.Vector2.sub (scale scaling (add pivot geometry.position)) pivot
    in
        Geometry newPosition zoom


{-|
  Moves the geometry `moved` amount
-}
moveGeometry : Vec2 -> Geometry -> Geometry
moveGeometry moved geometry =
    {geometry | position = (add geometry.position (scale -1 moved))}


{-|
  Convenience record for passing all event handlers to the view
-}
type alias MouseEvents msg =
    { moveMsg: Mouse.Event -> msg
    , scrollMsg: Scroll.Event -> msg
    , touchStart: Touch.Event -> msg
    , touchMove: Touch.Event -> msg
    , touchEnd: Touch.Event -> msg
    -- This is unused, but needed for preventing the default behaviour of images
    , downMsg: msg
    }


{-|
  Generates html for an image viewer
-}
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
            , Css.transforms <|
                [ Css.translateX <| Css.px -x
                , Css.translateY <| Css.px -y
                ]
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


{-|
  Event handler for image onLoad events
-}
onLoadSrc : msg -> Html.Attribute msg
onLoadSrc msg =
    on "load" (Json.Decode.succeed msg)


