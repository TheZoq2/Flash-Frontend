module ImageViewer exposing(Model, Msg, ImageInfo, init, update)

import Style

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Css
import Mouse exposing (Position)

--Type containing info about the current image
type alias ImageInfo =
    {
        src: String,
        dimensions: (Int, Int)
    }



type alias Drag =
    {
        start: Position,
        current: Position
    }


-- MODEL

type alias Model =
    {
        currentImage: ImageInfo,
        zoomLevel: Float,
        position: (Float, Float),
        drag: Maybe Drag,

        mousePosOnImage: Position
    }

init : (Model, Cmd Msg)
init = 
    ({
        currentImage = (ImageInfo "/mnt/1TB-files/Pictures/dslr/sept08-2016/DSC_0001.JPG" (6000,4000)),
        zoomLevel = 1,
        position = (400, 100),
        drag = Nothing,

        mousePosOnImage = (Position 0 0)
    }, Cmd.none)







-- UPDATE
type Msg
    = SetImage ImageInfo
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | OnScroll Float

    | MouseMovedOnImage Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        SetImage imgInfo ->
            ({model | currentImage = imgInfo}, Cmd.none)

        DragStart pos ->
            ({model | drag = Just (Drag pos pos)}, Cmd.none)

        DragAt pos ->
            ({model | drag = (Maybe.map (\{start} -> Drag start pos) model.drag)}, Cmd.none)

        DragEnd _ ->
            let
                newPos = (getPosition model)
            in
                ({model | position = newPos, drag = Nothing}, Cmd.none)

        OnScroll amount ->
            let
                zoomModifier = 1.0 - amount * 0.05

                newZoomLevel = model.zoomLevel * zoomModifier

                newPos = getNewPositionOnScroll model newZoomLevel
            in
                ({model | zoomLevel = newZoomLevel, position = newPos}, Cmd.none)

        MouseMovedOnImage {x, y} ->
            let
                (imagePosX, imagePosY) = getPosition model

                relativePos = Position (x - round (imagePosX)) (y - round (imagePosY))
            in
                ({model | mousePosOnImage = relativePos}, Cmd.none)


getPosition : Model -> (Float, Float)
getPosition model =
    case model.drag of
        Nothing ->
            model.position
        Just {start, current} ->
            let
                (posX, posY) = model.position

                result = 
                    (
                        (posX + (toFloat (current.x - start.x))),
                        (posY + (toFloat (current.y - start.y)))
                    )
            in
                result


getNewPositionOnScroll: Model -> Float -> (Float, Float)
getNewPositionOnScroll model newZoomLevel =
    let
        (currW, currH) = getImageSizeFromModel model

        --The offset of the mouse as (float,float) between 0 and 1 which corresponds to
        --the position on the current image
        (mouseX, mouseY) = ((toFloat model.mousePosOnImage.x), (toFloat model.mousePosOnImage.y))
        (mouseOffsetX, mouseOffsetY) = (mouseX / currW, mouseY / currH)

        (newW, newH) = getImageSize model.currentImage.dimensions newZoomLevel

        --The amount to subtract from the current positon
        (posSubX, posSubY) = (newW * mouseOffsetX, newH*mouseOffsetY)

        (cPosX, cPosY) = model.position
    in
        (cPosX - (posSubX - mouseX), cPosY - (posSubY - mouseY))






-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        img [
            src model.currentImage.src,
            Style.toStyle (getImageStyle model),
            onMouseDown,
            onScroll,
            onMouseMove
        ] []
    ]



getImageStyle : Model -> List Css.Mixin
getImageStyle model =
    let
        (x, y) = getPosition model
        (w, h) = getImageSizeFromModel model
    in
        [
            --Css.margin2 (Css.px y) (Css.px x),
            Css.left (Css.px x),
            Css.top (Css.px y),
            Css.position Css.relative,
            Css.width (Css.px (w)),
            Css.height (Css.px (h))
        ]

getImageSizeFromModel : Model -> (Float, Float)
getImageSizeFromModel model =
    getImageSize model.currentImage.dimensions model.zoomLevel


getImageSize : (Int, Int) -> Float -> (Float, Float)
getImageSize (realWidth, realHeight) zoomLevel =
        (
            ((toFloat realWidth) * zoomLevel),
            ((toFloat realHeight) * zoomLevel)
        )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [Mouse.moves DragAt, Mouse.ups DragEnd]







--EVENT LISTENERS

onMouseDown : Attribute Msg
onMouseDown =
    let
        options = {defaultOptions | preventDefault = True}
    in
        onWithOptions "mousedown" options (Json.Decode.map DragStart Mouse.position)

onScroll : Attribute Msg
onScroll = 
    let
        options = {defaultOptions | preventDefault = True}

        scrollEventDecoder = 
            Json.Decode.map OnScroll (Json.Decode.at ["deltaY"] Json.Decode.float)
    in
        onWithOptions "wheel" options (scrollEventDecoder)
        --onWithOptions "click" options (Json.Decode.succeed msg)

onMouseMove : Attribute Msg
onMouseMove =
    let
        posDecoder =
            Json.Decode.object2 Position ("clientX" := Json.Decode.int) ("clientY" := Json.Decode.int)
    in
        on "mousemove" (Json.Decode.map MouseMovedOnImage posDecoder)





-- MAIN
main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
