module TagEditor exposing (..)

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    , FileKind(..)
    , fileKindFromExtension
    )
import EditorMsg exposing
    ( Msg(..)
    )
import EditorView exposing
    ( view )

import EditorTagListUpdaters exposing
    ( addTagList
    , toggleTagList
    , toggleTag
    , removeTag
    , removeTagList
    , startTagAddition
    , cancelTagCreation
    )

import Tags
import ImageViewer
import FileList exposing (fileListDecoder)
import Mouse
import Touch
import Scroll
import Commands
import MsgCommand
import Urls

import Vec exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char
import Dom
import List.Extra
import UrlParser
import Navigation
import UrlParser
import UrlParser exposing ((</>))
import Math.Vector2 exposing (Vec2, vec2)
import CommandLine



init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { lastError = ""
      , keyReceiver = None
      , viewerSize = (Size 0 0 )
      , tags = Tags.emptyTagListList
      , tagTextfieldContent = Nothing
      , fileList = Nothing
      , oldTagList = Nothing
      , imageLoaded = True
      , sidebarVisible = True
      , oldUrl = ""
      , imageGeometry = ImageViewer.initGeometry
      , imageTouchState = ImageViewer.initTouchState
      , fileKind = Image
      }
    , Cmd.batch
        [ Task.perform WindowResized Window.size
        , updateLocation location
        ]
    )




-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestNext ->
            selectNextFile model
        RequestPrev ->
            selectPrevFile model
        RequestId id ->
            selectFileId id model
        RequestSave ->
            ( model, requestSaveImage model <| getSelectedTags model)
        OnSaved ->
            selectNextFile model
        NetworkError e ->
            networkError e model
        FileDataReceived data ->
            let
                newUrl = case model.fileList of
                    Just fileList ->
                        "#list/"
                               ++ (toString fileList.listId)
                               ++ "/file/"
                               ++ (toString fileList.fileIndex)
                    Nothing ->
                        ""
            in
                (onFileDataReceived data {model | oldUrl = newUrl } , Navigation.modifyUrl newUrl)
        UrlChanged location ->
            if location.hash /= model.oldUrl then
                ({model | oldUrl = location.hash}, updateLocation location)
            else
                (model, Cmd.none)
        WindowResized size ->
            let
                viewerSize =
                    Size ((toFloat size.width)) (toFloat size.height)
            in
                ( { model | viewerSize = viewerSize }, Cmd.none )
        ImageLoaded ->
                ( {model | imageLoaded = True, imageGeometry = ImageViewer.initGeometry}, Cmd.none)
        Keypress code ->
            handleKeyboardInput model code
        NewFileList selectedFile listId length ->
            let
                fileList = FileList.newWithSelected selectedFile listId length
            in
                ({model | fileList = Just fileList }, updateFileData fileList)
        CommandCanceled ->
            ({model | keyReceiver = None}, Cmd.none)
        -- TagList related messages
        AddTagList ->
            addTagList model
        FinnishAddTag id ->
            let
                newTags =
                    case model.tagTextfieldContent of
                        Just text ->
                            Tags.addTagToTagListList text id model.tags
                        Nothing ->
                            model.tags
            in
                ({model | tags = newTags} |> cancelTagCreation, Cmd.none)
        AddTag id text ->
            ({model | tags = Tags.addTagToTagListList text id model.tags}, Cmd.none)
        StartTagAddition id ->
            startTagAddition model id
        ToggleTagList id ->
            toggleTagList model id
        RemoveTagList id ->
            removeTagList model id
        ToggleTag listId tagId ->
            (toggleTag model listId tagId, Cmd.none)
        RemoveTag listId tagId ->
            (removeTag model listId tagId, Cmd.none)
        RemoveTagByName name ->
            ( updateModelTagsByName removeTag name model, Cmd.none)
        ToggleTagByName name ->
            ( updateModelTagsByName toggleTag name model, Cmd.none)
        CancelTagCreation ->
            (cancelTagCreation model, Cmd.none)
        TagTextFieldChanged text ->
            ({model | tagTextfieldContent = Just text}, Cmd.none)
        FocusResult result ->
            handleFocusResult result model
        MouseMovedOnImage event ->
            mouseMovedOnImage event model
        ImageScrolled event ->
            imageScrolled event model
        ImageTouchStart event ->
            imageTouchStartEnd event model
        ImageTouchEnd event ->
            imageTouchStartEnd event model
        ImageTouchMove event ->
            imageTouchMove event model
        ToggleSidebar ->
            toggleSidebar model
        CommandInput query ->
            handleCommandLineInput query model
        NoOp ->
            (model, Cmd.none)


updateModelTags : (Tags.TagListList -> Tags.TagListList) -> Model -> Model
updateModelTags updateFunc model =
    ({model | tags = updateFunc model.tags})


updateModelTagsByName : (Model -> Int -> Int -> Model) -> String -> Model -> Model
updateModelTagsByName updateFunc name model =
    let
        ids =
            Tags.indicesOfTag model.tags name
    in
        List.foldl (\(listId, tagId) model -> updateFunc model listId tagId) model ids


imageTouchStartEnd : Touch.Event -> Model -> (Model, Cmd Msg)
imageTouchStartEnd event model =
    ({model | imageTouchState = ImageViewer.handleTouchStartEnd event model.imageTouchState}, Cmd.none)

imageTouchMove : Touch.Event -> Model -> (Model, Cmd Msg)
imageTouchMove event model =
    let
        (touchState, geometry) =
            ImageViewer.handleTouchMove
                event
                model.imageTouchState
                model.imageGeometry
    in
        ({model | imageTouchState = touchState, imageGeometry = geometry}, Cmd.none)

imageScrolled : Scroll.Event -> Model -> (Model, Cmd Msg)
imageScrolled event model =
    let
        zoomModifier = 1.0 - event.deltaY * 0.05

        clientXY = Math.Vector2.fromTuple event.mouseEvent.clientPos

        newGeometry = (ImageViewer.zoomGeometry zoomModifier clientXY model.imageGeometry)
    in
        ({model | imageGeometry = newGeometry}, Cmd.none)


mouseMovedOnImage : Mouse.Event -> Model -> (Model, Cmd Msg)
mouseMovedOnImage event model =
    ( {model | imageGeometry = (ImageViewer.handleMouseMove event model.imageGeometry)}
    , Cmd.none
    )


handleFocusResult : (Result Dom.Error ()) -> Model -> (Model, Cmd Msg)
handleFocusResult result model =
    case result of
        Err (Dom.NotFound id) ->
            let
                _ = Debug.log "Failed to find dom element when trying to focus on" id
            in
                ( {model | keyReceiver = None}, Cmd.none)
        Ok () ->
            (model, Cmd.none)


networkError : Http.Error -> Model -> (Model, Cmd Msg)
networkError e model =
    let
        _ =
            Debug.log "Network error" e
    in
        ( model, Cmd.none )


-- Handling navigation and url updates
type Route
    = FileList Int
    | File Int Int

updateLocation : Navigation.Location -> Cmd Msg
updateLocation location =
    let
        route =
            UrlParser.oneOf
                [ UrlParser.map FileList (UrlParser.s "list" </> UrlParser.int)
                , UrlParser.map File
                    (UrlParser.s "list" </> UrlParser.int </> (UrlParser.s "file") </> UrlParser.int)
                ]
    in
        case UrlParser.parseHash route location of
            Just (FileList list) ->
                requestFileListData 0 list
            Just (File list file) ->
                requestFileListData file list
            Nothing ->
                Cmd.none






keyboardSelectorList : List Char
keyboardSelectorList =
    ['J', 'K', 'L', 'H', 'S', 'D', 'F', 'G']

handleKeyboardInput : Model -> Int -> ( Model, Cmd Msg )
handleKeyboardInput model code =
    case model.keyReceiver of
        None ->
            case Char.fromCode code of
                'L' ->
                    --Next
                    selectNextFile model

                'H' ->
                    --Previous
                    selectPrevFile model

                'S' ->
                    --Save
                    ( model, requestSaveImage model <| getSelectedTags model)

                'T' ->
                    --Modify tags
                    ( { model | keyReceiver = TagListList }, Cmd.none )
                'B' ->
                    toggleSidebar model
                ' ' -> -- Show commandline
                    showCommandLine model
                _ ->
                    ( model, Cmd.none )

        TagListList ->
            case Char.fromCode code of
                'I' ->
                    --Return to normal
                    ( {model | keyReceiver = None}, Cmd.none)
                'A' ->
                    addTagList model
                code ->
                    --Select a subcomponent
                    handleTagListSelectorKeys model code
        TagList id ->
            case Char.fromCode code of
                'I' ->
                    ( {model | keyReceiver = TagListList}, Cmd.none)
                'A' ->
                    startTagAddition model id
                'R' -> -- Remove the list
                    removeTagList model id
                'T' -> -- Toggle the list
                    toggleTagList model id
                code ->
                    handleTagSelectorKeys model id code
        Tag listId tagId ->
            case Char.fromCode code of
                'I' ->
                    ( {model | keyReceiver = TagList listId}, Cmd.none)
                'R' ->
                    (removeTag model listId tagId, Cmd.none)
                'T' ->
                    (toggleTag model listId tagId, Cmd.none)
                _ ->
                    (model, Cmd.none)
        CommandField commandData ->
            case code of
                27 ->
                    (hideCommandLine model, Cmd.none)
                13 ->
                    submitCommandLine commandData model
                _ ->
                    (model, Cmd.none)
        _ ->
            (model, Cmd.none)

toggleSidebar : Model -> (Model, Cmd Msg)
toggleSidebar model =
    ( { model | sidebarVisible = not model.sidebarVisible}, Cmd.none)

handleTagListSelectorKeys : Model -> Char -> (Model, Cmd Msg)
handleTagListSelectorKeys model code =
    case List.Extra.elemIndex code keyboardSelectorList of
        Just index ->
            let
                receiverId =
                    Tags.getNthTagListId model.tags index
            in
                case receiverId of
                    Just id ->
                        ({ model | keyReceiver = TagList id}, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)
        Nothing ->
            (model, Cmd.none)



handleTagSelectorKeys : Model -> Int -> Char -> (Model, Cmd Msg)
handleTagSelectorKeys model selectedListId code =
    case List.Extra.elemIndex code keyboardSelectorList of
        Just index ->
            let
                receiverId =
                    Tags.getNthTag model.tags selectedListId index
            in
                case receiverId of
                    Just receiverId ->
                        ({model | keyReceiver = Tag selectedListId receiverId}, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)
        Nothing ->
            (model, Cmd.none)


selectNextFile : Model -> (Model, Cmd Msg)
selectNextFile model =
    jumpFileList 1 model

selectPrevFile : Model -> (Model, Cmd Msg)
selectPrevFile model =
    jumpFileList -1 model


selectFileId : Int -> Model -> (Model, Cmd Msg)
selectFileId id model =
    case model.fileList of
        Just fileList ->
            let
                jumpAmount = id - fileList.fileIndex
            in
                jumpFileList jumpAmount model
        Nothing ->
            (model, Cmd.none)

jumpFileList : Int -> Model -> (Model, Cmd Msg)
jumpFileList amount model =
    case model.fileList of
        Just list ->
            let
                newList = FileList.jump amount list
            in
                ({model | fileList = Just newList, imageLoaded = False}, updateFileData newList)
        Nothing ->
            (model, Cmd.none)


decodeFileData : Json.Decode.Decoder FileData
decodeFileData =
    Json.Decode.map2 FileData
        (field "file_path" Json.Decode.string)
        (field "tags" (Json.Decode.list Json.Decode.string))


checkHttpAttempt : (a -> Msg) -> Result Http.Error a -> Msg
checkHttpAttempt func res=
    case res of
        Ok val ->
            func val
        Err e ->
            NetworkError e

requestFileData : Int -> Int -> Cmd Msg
requestFileData listId index =
    Http.send 
        (checkHttpAttempt FileDataReceived)
        (Http.get (Urls.fileListGetDataUrl listId index) decodeFileData)

updateFileData : FileList.FileList -> Cmd Msg
updateFileData fileList =
    requestFileData fileList.listId fileList.fileIndex


requestSaveImage : Model -> List String -> Cmd Msg
requestSaveImage model tags =
    case model.fileList of
        Just fileList ->
            let
                url = Urls.fileListSaveUrl
                        fileList.listId
                        fileList.fileIndex
                        tags
            in
                Http.send 
                    (checkHttpAttempt (\_ -> OnSaved))
                    (Http.get url <| Json.Decode.string)
        Nothing ->
            Cmd.none



showCommandLine : Model -> (Model, Cmd Msg)
showCommandLine model =
    let
        newKeyReceiver =
            case Commands.initCommandData (commands model) of
                Ok commandData ->
                    CommandField commandData
                Err e ->
                    let
                        _ = Debug.log "Failed to init commands: " e
                    in
                        model.keyReceiver

        _ = Debug.log "newKeyReceiver" newKeyReceiver
    in
        ( {model | keyReceiver = newKeyReceiver }
        , Dom.focus "command_field" |> Task.attempt FocusResult
        )


hideCommandLine : Model -> Model
hideCommandLine model =
    let
        _ = Debug.log "got hide commannd" ""
    in
        {model | keyReceiver = None}


commands : Model -> CommandLine.Command Msg
commands model =
    MsgCommand.topLevel <| getAllTags model


submitCommandLine : Commands.CommandData -> Model -> (Model, Cmd Msg)
submitCommandLine commandData model =
    let
        commandTemplate = commands model
    in
        case CommandLine.parseCommand commandData.expandedQuery commandTemplate of
            Ok msg ->
                update msg <| hideCommandLine model
            Err _ ->
                (hideCommandLine model, Cmd.none)



handleCommandLineInput : String -> Model -> (Model, Cmd Msg)
handleCommandLineInput query model =
    case model.keyReceiver of
        CommandField data ->
            let
                command = (commands model)

                newData =
                    case CommandLine.expandCommand query command of
                        (expansion, Ok suggestions) ->
                            {data
                                | query = query
                                , expandedQuery = expansion
                                , suggestions = suggestions
                            }
                        (expansion, Err e) ->
                            let
                                _ = Debug.log "Command parse error: " e
                            in
                                data
            in
                ({model | keyReceiver = CommandField newData}, Cmd.none)
        _ ->
            let
                _ = Debug.log "Got a commandLineInput msg with unfocused command field, ignoring" ""
            in
                (model, Cmd.none)



submitFileListRequest : String -> (FileList.FileListResponse -> Msg) -> Cmd Msg
submitFileListRequest url msgFunc =
    Http.send
        (checkHttpAttempt msgFunc)
        (Http.get url fileListDecoder)


requestFileListData : Int -> Int -> Cmd Msg
requestFileListData selected listId =
    let
        url =
            Urls.fileListListInfoUrl listId
    in
        submitFileListRequest url (\val -> NewFileList selected val.id val.length)



onFileDataReceived : FileData -> Model -> Model
onFileDataReceived data model =
    let
        -- Read the current file type
        newFileKind =
            String.split "." data.filePath
            |> List.reverse
            |> List.head
            |> Maybe.map fileKindFromExtension
            |> Maybe.withDefault model.fileKind


        -- Remove the old tag list containing old tags
        newTagListList = case model.oldTagList of
            Just id ->
                Tags.removeTagList id model.tags
            Nothing ->
                model.tags

        uniqueTags =
            List.filter
                (\tag -> (List.member (String.toLower tag)
                            <| List.map String.toLower
                            <| Tags.selectedTags newTagListList
                         ) == False)
                <| data.tags


        -- Add a new tag list with the old tags if they exist
        (newNewTagListList, id) = case uniqueTags of
            [] ->
                (newTagListList, Nothing)
            tags ->
                let
                    list = Tags.addTagsToList uniqueTags <| Tags.emptyTagList
                    (listList, id) = Tags.addTagList list newTagListList
                in
                    (listList, Just id)
    in
        {model | tags = newNewTagListList, oldTagList = id, fileKind = newFileKind}





getSelectedTags : Model -> List String
getSelectedTags model =
    Tags.selectedTags model.tags


getAllTags : Model -> List String
getAllTags model =
    Debug.log "Tags: " Tags.allTags model.tags



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResized
        , Keyboard.downs Keypress
        ]



-- Main


main : Program Never Model Msg
main =
    Navigation.program UrlChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
