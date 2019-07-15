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
import Scroll
import Commands
import MsgCommand
import Urls
import CommandLine
import MathUtil
import Mouse

import Vec exposing (..)
import Json.Decode exposing (..)
import Json.Decode as Decode
import Json.Encode
import Http
import Task
import Char
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation exposing (Key)
import Browser.Events
import Url
import List.Extra
import Url.Parser
import Url.Parser exposing ((</>))
import Math.Vector2 exposing (Vec2, vec2)
import Html.Styled exposing (toUnstyled)


init : flags -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ location key =
    ( { lastError = ""
      , keyReceiver = FocusNone
      , viewerSize = (Size 0 0 )
      , tags = Tags.emptyTagListList
      , tagTextfieldContent = Nothing
      , fileList = Nothing
      , oldTagList = Nothing
      , imageLoaded = True
      , sidebarVisible = True
      , oldUrl = location
      , imageGeometry = ImageViewer.initGeometry
      -- , imageTouchState = ImageViewer.initTouchState
      , fileKind = Image
      , navigationKey = key
      }
    , Cmd.batch
        [ Task.perform
            (\vp -> WindowResized (round vp.viewport.width) (round vp.viewport.height))
            Dom.getViewport
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
                newFragment = case model.fileList of
                    Just fileList ->
                        "list/"
                               ++ (String.fromInt fileList.listId)
                               ++ "/file/"
                               ++ (String.fromInt fileList.fileIndex)
                    Nothing ->
                        ""

                oldUrl = model.oldUrl
                newUrl = {oldUrl | fragment = Just newFragment}
            in
                ( onFileDataReceived data {model | oldUrl = newUrl }
                , Navigation.replaceUrl (model.navigationKey) (Url.toString newUrl)
                )
        UrlChanged location ->
            if location /= model.oldUrl then
                ({model | oldUrl = location}, updateLocation location)
            else
                (model, Cmd.none)
        UrlRequest _ ->
            (model, Cmd.none)
        WindowResized sizeX sizeY ->
            let
                viewerSize =
                    Size ((toFloat sizeX)) (toFloat sizeY)
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
            ({model | keyReceiver = FocusNone}, Cmd.none)
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
        MouseMovedOnImage coords ->
            mouseMovedOnImage coords model
        ImageScrolled event ->
            imageScrolled event model
        -- ImageTouchStart event ->
        --     imageTouchStartEnd event model
        -- ImageTouchEnd event ->
        --     imageTouchStartEnd event model
        -- ImageTouchMove event ->
        --     imageTouchMove event model
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
        List.foldl (\(listId, tagId) accModel -> updateFunc accModel listId tagId) model ids


-- TODO: Re-add touch support
-- imageTouchStartEnd : Touch.Event -> Model -> (Model, Cmd Msg)
-- imageTouchStartEnd event model =
--     ({model | imageTouchState = ImageViewer.handleTouchStartEnd event model.imageTouchState}, Cmd.none)
-- 
-- imageTouchMove : Touch.Event -> Model -> (Model, Cmd Msg)
-- imageTouchMove event model =
--     let
--         (touchState, geometry) =
--             ImageViewer.handleTouchMove
--                 event
--                 model.imageTouchState
--                 model.imageGeometry
--     in
--         ({model | imageTouchState = touchState, imageGeometry = geometry}, Cmd.none)

imageScrolled : Scroll.Event -> Model -> (Model, Cmd Msg)
imageScrolled event model =
    let
        zoomModifier = 1.0 - event.deltaY * 0.05

        clientXY = MathUtil.tupleToVec event.mouseEvent.clientPos

        newGeometry = (ImageViewer.zoomGeometry zoomModifier clientXY model.imageGeometry)
    in
        ({model | imageGeometry = newGeometry}, Cmd.none)


mouseMovedOnImage : Mouse.Event -> Model -> (Model, Cmd Msg)
mouseMovedOnImage coords model =
    ( {model | imageGeometry = (ImageViewer.handleMouseMove coords model.imageGeometry)}
    , Cmd.none
    )


handleFocusResult : (Result Dom.Error ()) -> Model -> (Model, Cmd Msg)
handleFocusResult result model =
    case result of
        Err (Dom.NotFound id) ->
            let
                _ = Debug.log "Failed to find dom element when trying to focus on" id
            in
                ( {model | keyReceiver = FocusNone}, Cmd.none)
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

updateLocation : Url.Url -> Cmd Msg
updateLocation location =
    let
        adjustedLocation = {location | path = Maybe.withDefault "" location.fragment, fragment = Nothing}
        route =
            Url.Parser.oneOf
                [ Url.Parser.map FileList (Url.Parser.s "list" </> Url.Parser.int)
                , Url.Parser.map File
                    (Url.Parser.s "list" </> Url.Parser.int </> (Url.Parser.s "file") </> Url.Parser.int)
                ]
    in
        case Url.Parser.parse route adjustedLocation of
            Just (FileList list) ->
                requestFileListData 0 list
            Just (File list file) ->
                requestFileListData file list
            Nothing ->
                Cmd.none






keyboardSelectorList : List String
keyboardSelectorList =
    ["j", "k", "l", "h", "s", "d", "f", "g"]

handleKeyboardInput : Model -> String -> ( Model, Cmd Msg )
handleKeyboardInput model code =
    case model.keyReceiver of
        FocusNone ->
            case code of
                "l" ->
                    --Next
                    selectNextFile model

                "h" ->
                    --Previous
                    selectPrevFile model

                "s" ->
                    --Save
                    ( model, requestSaveImage model <| getSelectedTags model)

                "t" ->
                    --Modify tags
                    ( { model | keyReceiver = FocusTagListList }, Cmd.none )
                "b" ->
                    toggleSidebar model
                " " -> -- Show commandline
                    showCommandLine model
                _ ->
                    ( model, Cmd.none )

        FocusTagListList ->
            case code of
                "i" ->
                    --Return to normal
                    ( {model | keyReceiver = FocusNone}, Cmd.none)
                "a" ->
                    addTagList model
                code_ ->
                    --Select a subcomponent
                    handleTagListSelectorKeys model code_
        FocusTagList id ->
            case code of
                "i" ->
                    ( {model | keyReceiver = FocusTagListList}, Cmd.none)
                "a" ->
                    startTagAddition model id
                "r" -> -- Remove the list
                    removeTagList model id
                "t" -> -- Toggle the list
                    toggleTagList model id
                code_ ->
                    handleTagSelectorKeys model id code_
        FocusTag listId tagId ->
            case code of
                "i" ->
                    ( {model | keyReceiver = FocusTagList listId}, Cmd.none)
                "r" ->
                    (removeTag model listId tagId, Cmd.none)
                "t" ->
                    (toggleTag model listId tagId, Cmd.none)
                _ ->
                    (model, Cmd.none)
        FocusCommandField commandData ->
            case code of
                "Escape" ->
                    (hideCommandLine model, Cmd.none)
                "Enter" ->
                    submitCommandLine commandData model
                _ ->
                    (model, Cmd.none)
        _ ->
            (model, Cmd.none)

toggleSidebar : Model -> (Model, Cmd Msg)
toggleSidebar model =
    ( { model | sidebarVisible = not model.sidebarVisible}, Cmd.none)

handleTagListSelectorKeys : Model -> String -> (Model, Cmd Msg)
handleTagListSelectorKeys model code =
    case List.Extra.elemIndex code keyboardSelectorList of
        Just index ->
            let
                receiverId =
                    Tags.getNthTagListId model.tags index
            in
                case receiverId of
                    Just id ->
                        ({ model | keyReceiver = FocusTagList id}, Cmd.none)
                    Nothing ->
                        (model, Cmd.none)
        Nothing ->
            (model, Cmd.none)



handleTagSelectorKeys : Model -> Int -> String -> (Model, Cmd Msg)
handleTagSelectorKeys model selectedListId code =
    case List.Extra.elemIndex code keyboardSelectorList of
        Just index ->
            let
                receiverId =
                    Tags.getNthTag model.tags selectedListId index
            in
                case receiverId of
                    Just receiverId_ ->
                        ({model | keyReceiver = FocusTag selectedListId receiverId_}, Cmd.none)
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
                    FocusCommandField commandData
                Err e ->
                    let
                        _ = Debug.log "Failed to init commands: " e
                    in
                        model.keyReceiver
    in
        ( {model | keyReceiver = newKeyReceiver }
        , Dom.focus "command_field" |> Task.attempt FocusResult
        )


hideCommandLine : Model -> Model
hideCommandLine model =
    {model | keyReceiver = FocusNone}


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
        FocusCommandField data ->
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
                ({model | keyReceiver = FocusCommandField newData}, Cmd.none)
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
        newTagListList =
            case model.oldTagList of
                Just id_ ->
                    Tags.removeTagList id_ model.tags
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
        (newNewTagListList, id) =
            case uniqueTags of
                [] ->
                    (newTagListList, Nothing)
                tags ->
                    let
                        list = Tags.addTagsToList uniqueTags <| Tags.emptyTagList
                        (listList, id_) = Tags.addTagList list newTagListList
                    in
                        (listList, Just id_)
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
        [ Browser.Events.onResize WindowResized
        , Browser.Events.onKeyDown (Decode.map Keypress (Decode.field "key" Decode.string))
        ]



-- Main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = (\model -> Browser.Document "flash" [toUnstyled <| view model])
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
        }
