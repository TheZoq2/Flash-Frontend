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
        drag: Maybe Drag
    }

init : (Model, Cmd Msg)
init = 
    ({
        currentImage = (ImageInfo "/mnt/1TB-files/Pictures/dslr/sept08-2016/DSC_0001.JPG" (0,0)),
        zoomLevel = 1,
        position = (400, 100),
        drag = Nothing
    }, Cmd.none)







-- UPDATE
type Msg
    = SetImage ImageInfo
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | OnScroll Float


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
            (model, Cmd.none)


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






-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        img [
            src model.currentImage.src,
            Style.toStyle (getImageStyle model),
            onMouseDown,
            onScroll
        ] []
    ]



getImageStyle : Model -> List Css.Mixin
getImageStyle model =
    let
        (x, y) = getPosition model
    in
        [
            Css.margin2 (Css.px y) (Css.px x)
            --Css.width (Css.px 100),
            --Css.height (Css.px 100)
        ]




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





-- MAIN
main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
