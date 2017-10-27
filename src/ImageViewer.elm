module ImageViewer
    exposing
        ( imageViewerHtml
        , videoViewer
        , Geometry
        , initGeometry
        , MouseEvents
        , handleMouseMove
        , zoomGeometry
        , TouchState
        , initTouchState
        , handleTouchStartEnd
        , handleTouchMove
        , textureParameters
        )

import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on, onWithOptions, defaultOptions)
import Json.Decode exposing (..)
import Mouse
import Scroll
import Touch
import MultiTouch
import Dict
import Css
import Style

import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture, nonPowerOfTwoOptions)

import Math.Vector2 exposing (..)

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
imageViewerHtml : Vec2 -> Geometry -> Maybe Texture -> MouseEvents msg -> Html msg
imageViewerHtml containerSize geometry texture msgs =
    case texture of
        Just texture ->
            webGlRenderer containerSize geometry msgs texture
        Nothing ->
            div [] []


webGlRenderer : Vec2 -> Geometry -> MouseEvents msg -> Texture -> Html msg
webGlRenderer viewerSize geometry msgs texture =
    let
        {moveMsg, downMsg, scrollMsg, touchStart, touchMove, touchEnd} = msgs

        mouseDownOptions =
            {defaultOptions | preventDefault = True}
    in
        WebGL.toHtml
            [ Html.Attributes.width <| round <| getX viewerSize
            , Html.Attributes.height <| round <| getY viewerSize
            , Mouse.onMove moveMsg
            , onWithOptions "mousedown" mouseDownOptions (Json.Decode.succeed downMsg)
            , MultiTouch.onStart touchStart
            , MultiTouch.onMove touchMove
            , MultiTouch.onEnd touchEnd
            , Scroll.onScroll scrollMsg
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                glSquare
                (uniforms geometry viewerSize texture)
            ]

glSquare : Mesh Vertex
glSquare =
    [ ( Vertex (vec2 0 0) (vec2 0 0)
      , Vertex (vec2 0 1) (vec2 0 1)
      , Vertex (vec2 1 1) (vec2 1 1)
      )
    , ( Vertex (vec2 0 0) (vec2 0 0)
      , Vertex (vec2 1 1) (vec2 1 1)
      , Vertex (vec2 1 0) (vec2 1 0)
      )
    ]
        |> WebGL.triangles


type alias Vertex =
    { position: Vec2
    , coord: Vec2
    }

type alias Uniforms =
    { imagePosition: Vec2
    , zoom: Float
    , viewerSize: Vec2
    , imageSize: Vec2
    , texture: Texture
    }

uniforms : Geometry -> Vec2 -> Texture -> Uniforms
uniforms {position, zoom} viewerSize texture =
    let
        imageSize = fromTuple 
                <| (\(x,y) -> (toFloat x, toFloat y))
                <| WebGL.Texture.size texture
    in
        Uniforms position zoom viewerSize imageSize texture

vertexShader : Shader Vertex Uniforms {vCoord: Vec2}
vertexShader =
    [glsl|
        attribute vec2 position;
        attribute vec2 coord;

        uniform vec2 imagePosition;
        uniform float zoom;
        uniform vec2 imageSize;
        uniform vec2 viewerSize;

        varying vec2 vCoord;


        void main() {
            // The local position of each vertex
            vec2 localPos = position*imageSize / viewerSize * zoom;

            // The position of the image
            vec2 globalPos = imagePosition / viewerSize;

            // Put the image in the top left corner and make 0 < {x,y} < 1
            mat4 transform =
                mat4(
                      2, 0, 0, 0
                    , 0, -2, 0, 0
                    , 0, 0, 0, 0
                    , -1, 1, 0, 1
                    );

            gl_Position = transform * vec4(localPos + -globalPos, 0., 1.);
            vCoord = coord;
        }
    |]

fragmentShader : Shader {} Uniforms {vCoord: Vec2}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;

        varying vec2 vCoord;

        void main() {
            gl_FragColor = texture2D(texture, vCoord);
        }
    |]


textureParameters : WebGL.Texture.Options
textureParameters =
    let
        default = WebGL.Texture.nonPowerOfTwoOptions
    in
    { default |
        flipY = False
    }



videoViewer : msg -> Vec2 -> String -> Html msg
videoViewer onLoadStart containerSize url =
    let
        (w, h) =
            toTuple containerSize

        css =
            [ Css.width <| Css.px w
            , Css.height <| Css.px h
            ]
    in
        video
            [ src url
            , Style.toStyle css
            , onVideoLoadStart onLoadStart
            , Html.Attributes.controls True
            ]
            []

{-|
  Event handler for image onLoad events
-}
onLoadSrc : msg -> Html.Attribute msg
onLoadSrc msg =
    on "load" (Json.Decode.succeed msg)

{-|
  Event handler for video onLoadStart events
-}
onVideoLoadStart : msg -> Html.Attribute msg
onVideoLoadStart msg =
    on "loadStart" (Json.Decode.succeed msg)


