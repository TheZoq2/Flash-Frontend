module TagEditor exposing(..)

import TagListList
import TagListManager
import Style
import ImageViewer
import Vec exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char

-- MODEL

type KeyReceiver
    = None
    | TagListList


type alias Model =
    {
        tagListList: TagListList.Model,
        imageViewer: ImageViewer.Model,
        currentImage: String,
        currentImageDimensions: (Int, Int),
        lastError: String,
        keyReceiver: KeyReceiver
    }

init: (Model, Cmd Msg)
init = 
    let
        (imgViewModel, imgViewCmd) =
            ImageViewer.init
    in
        (
            Model TagListList.init imgViewModel "" (0,0) "" None,
            Cmd.batch [
                requestNewImage Current, 
                Task.perform WindowResized WindowResized Window.size,
                Cmd.map ImageViewerMsg imgViewCmd
            ]
        )






-- UPDATE

type Msg
    = TagListListMsg TagListList.Msg
    | ImageViewerMsg ImageViewer.Msg
    | RequestNext
    | RequestPrev
    | RequestCurrent
    | RequestSave
    | NetworkError Http.Error
    | NewImageReceived ImageResponse
    | OnSaved String
    | WindowResized Window.Size
    | Keypress Int



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        TagListListMsg tllMsg ->
            ({model | tagListList = (TagListList.update tllMsg model.tagListList)}, Cmd.none)
            --(model, Cmd.none)
        ImageViewerMsg imgViewMsg ->
            let
                (imgViewModel, imgViewCmd) =
                    ImageViewer.update imgViewMsg model.imageViewer
            in
            (
                {model | 
                    imageViewer = imgViewModel
                }, 
                Cmd.map ImageViewerMsg imgViewCmd
            )
            --(model, Cmd.none)

        RequestNext ->
            (model, requestNewImage Next)
        RequestPrev ->
            (model, requestNewImage Prev)
        RequestCurrent ->
            (model, requestNewImage Current)

        RequestSave ->
            (model, requestSaveImage (getSelectedTags model))

        OnSaved _ ->
            (model, requestNewImage Next)


        NetworkError e ->
            let
                _ = Debug.log "Network error" e
            in
                (model, Cmd.none)

        NewImageReceived response ->
            let
                currentImage = "http://localhost:3000/" ++ response.filePath

                (imgViewModel, imgViewCmd) =
                    ImageViewer.setCurrentImage model.imageViewer (ImageViewer.ImageInfo currentImage response.dimensions)
            in
                ({model | 
                    currentImage = "http://localhost:3000/" ++ response.filePath,
                    imageViewer = imgViewModel,
                    currentImageDimensions = response.dimensions
                } , Cmd.map ImageViewerMsg imgViewCmd)

        WindowResized size ->
            let
                viewerSize = 
                    Size ((toFloat size.width) - Style.totalSidebarSize) (toFloat size.height)
                (newImgViewer, imgViewCmd) = 
                    ImageViewer.resize model.imageViewer viewerSize
            in
                ({model | imageViewer = newImgViewer }, Cmd.map ImageViewerMsg imgViewCmd)

        Keypress code ->
            handleKeyboardInput model code





handleKeyboardInput : Model -> Int -> (Model, Cmd Msg)
handleKeyboardInput model code =
    let
        _ = Debug.log "" (Char.fromCode code)
    in
        case model.keyReceiver of
            None ->
                case Char.fromCode code of
                    'L' -> --Next
                        (model, requestNewImage Next)
                    'H' -> --Previous
                        (model, requestNewImage Prev)
                    'S' -> --Save
                        (model, requestSaveImage (getSelectedTags model))
                    'T' -> --Modify tags
                        ({model | model.keyReceiver = TagListList}, Cmd.none)
                    _ ->
                        (model, Cmd.none)
            TagListList ->
                case Char.fromCode code of
                    'I' ->
                        ({model | model.keyReceiver = None}, Cmd.none)
                    _ ->
                        (model, Cmd.none)

type ImageDirection
    = Next
    | Prev
    | Current

requestNewImage : ImageDirection -> Cmd Msg
requestNewImage direction = 
    let 
        action = case direction of 
            Next ->
                "next"
            Prev ->
                "prev"
            Current ->
                "current"

        url = "http://localhost:3000/list?action=" ++ action
    in
        Task.perform NetworkError NewImageReceived (Http.get decodeNewImage url)

requestSaveImage : (List String) -> Cmd Msg
requestSaveImage tags =
    let 
        --Encode the tag list
        tagsJson = List.map Json.Encode.string tags

        --TODO: Make surre X-Origin requests are allowed
        url = "http://localhost:3000/list?action=save&tags=" ++ toString tagsJson
    in
        Task.perform NetworkError OnSaved (Http.get (Json.Decode.at [] Json.Decode.string) url)



type alias ImageResponse = {
        filePath: String,
        dimensions: (Int, Int)
    }
decodeNewImage : Json.Decode.Decoder ImageResponse
decodeNewImage =
    let
        decodeDimensions =
            Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int

        decodeMsg = 
            Json.Decode.object2 ImageResponse 
                ("file_path" := Json.Decode.string) 
                ("dimensions" := decodeDimensions)
    in
        decodeMsg




getSelectedTags : Model -> List String
getSelectedTags model =
    TagListList.getSelectedTags model.tagListList



-- VIEW

view : Model -> Html Msg
view model =
    let
        nextButton = button [onClick RequestNext] [text "Next"]
        prevButton = button [onClick RequestPrev] [text "Prev"]
        saveButton = button [onClick RequestSave] [text "Save"]

        buttonRow = div [Style.class [Style.TagEditorButtonRow]] [prevButton, nextButton, saveButton]
    in
    div [Style.class [Style.TagEditorContainer]]
    [
        div [Style.class [Style.TagEditorContentContainer]]
        [
            Html.App.map (ImageViewerMsg) (ImageViewer.view model.imageViewer)
        ],
        div [Style.class [Style.TagEditorRightPane]] 
        [
            buttonRow,
            Html.App.map (TagListListMsg) (TagListList.view model.tagListList)
        ]
    ]


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Sub.map ImageViewerMsg (ImageViewer.subscriptions model.imageViewer),
        Window.resizes WindowResized,
        Keyboard.downs Keypress
    ]





-- Main
main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
