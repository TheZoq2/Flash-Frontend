module TagEditor exposing (..)

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    )
import EditorMsg exposing
    ( Msg(..)
    )
import EditorView exposing
    ( view)

import Tags
import ImageViewer
import FileList exposing (fileListDecoder, fileListFileUrl, fileListListUrl)
import Touch

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
        RequestSave ->
            ( model, requestSaveImage model <| getSelectedTags model)
        OnSaved ->
            selectNextFile model
        NetworkError e ->
            let
                _ =
                    Debug.log "Network error" e
            in
                ( model, Cmd.none )
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
                ( {model | imageLoaded = True}, Cmd.none)
        Keypress code ->
            handleKeyboardInput model code
        NewFileList selectedFile listId length ->
            let
                fileList = FileList.newWithSelected selectedFile listId length
            in
                ({model | fileList = Just fileList }, updateFileData fileList)
        -- TagList related messages
        AddTagList ->
            addTagList model
        AddTag id ->
            let
                newTags =
                    case model.tagTextfieldContent of
                        Just text ->
                            Tags.addTagToTagListList text id model.tags
                        Nothing ->
                            model.tags
            in
                ({model | tags = newTags} |> cancelTagCreation, Cmd.none)
        StartTagAddition id ->
            startTagAddition model id
        ToggleTagList id ->
            toggleTagList model id
        RemoveTagList id ->
            removeTagList model id
        ToggleTag listId tagId ->
            toggleTag model listId tagId
        RemoveTag listId tagId ->
            removeTag model listId tagId
        CancelTagCreation ->
            (cancelTagCreation model, Cmd.none)
        TagTextFieldChanged text ->
            ({model | tagTextfieldContent = Just text}, Cmd.none)
        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    let
                        _ = Debug.log "Failed to find dom element when trying to focus on" id
                    in
                        ( {model | keyReceiver = None}, Cmd.none)
                Ok () ->
                    (model, Cmd.none)
        MouseMovedOnImage event ->
            ( {model | imageGeometry = (ImageViewer.handleMouseMove event model.imageGeometry)}
            , Cmd.none
            )
        ImageScrolled event ->
            let
                zoomModifier = 1.0 - event.deltaY * 0.05

                clientXY = Math.Vector2.fromTuple event.mouseEvent.clientPos

                newGeometry = (ImageViewer.handleZoom zoomModifier clientXY model.imageGeometry)
            in
                ({model | imageGeometry = newGeometry}, Cmd.none)
        NoOp ->
            (model, Cmd.none)



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


startTagAddition : Model -> Int -> (Model, Cmd Msg)
startTagAddition model tagListId =
    let
        newTags = Tags.startTagTextInput tagListId model.tags
        cmd = Dom.focus "tag_input_field" |> Task.attempt FocusResult
    in
        ({model | tags = newTags, keyReceiver = TagField tagListId}, cmd)


removeTagList : Model -> Int -> (Model, Cmd Msg)
removeTagList model id =
    let
        newTags =
            Tags.removeTagList id model.tags

        newReceiver =
            case model.keyReceiver of
                TagList _ ->
                    TagListList
                TagField _ ->
                    TagListList
                old -> old
    in
        ({model | tags = newTags, keyReceiver = newReceiver}, Cmd.none)

toggleTag : Model -> Int -> Int -> (Model, Cmd Msg)
toggleTag model listId tagId =
    let
        newTags = Tags.toggleTagInTagListList listId tagId model.tags
    in
        ({model | tags = newTags}, Cmd.none)



removeTag : Model -> Int -> Int -> (Model, Cmd Msg)
removeTag model listId tagId =
    let
        newTags =
            Tags.removeTagFromTagListList listId tagId model.tags

        newReceiver =
            case model.keyReceiver of
                Tag listId _ ->
                    TagList listId
                old -> old
    in
        ({model | tags = newTags, keyReceiver = newReceiver}, Cmd.none)



addTagList : Model -> (Model, Cmd Msg)
addTagList model =
    let
        (newTags, _) = Tags.addTagList Tags.emptyTagList model.tags
    in
        ( {model | tags = newTags }, Cmd.none)


toggleTagList : Model -> Int -> (Model, Cmd Msg)
toggleTagList model id =
    let
        newTags = Tags.toggleTagList id model.tags
    in
        ({model | tags = newTags}, Cmd.none)



cancelTagCreation : Model -> Model
cancelTagCreation model =
    let
        newReceiver = case model.keyReceiver of
            TagField id ->
                TagList id
            _ ->
                None
    in
        {model | tagTextfieldContent = Nothing
               , tags = Tags.cancelAddTag model.tags
               , keyReceiver = newReceiver
               }



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
                    ( { model | sidebarVisible = not model.sidebarVisible}, Cmd.none)
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
                    removeTag model listId tagId
                'T' ->
                    toggleTag model listId tagId
                _ ->
                    (model, Cmd.none)
        _ ->
            (model, Cmd.none)


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
    let
        url = fileListFileUrl [] "get_data" listId index
    in
        Http.send 
            (checkHttpAttempt FileDataReceived)
            (Http.get url decodeFileData)

updateFileData : FileList.FileList -> Cmd Msg
updateFileData fileList =
    requestFileData fileList.listId fileList.fileIndex


requestSaveImage : Model -> List String -> Cmd Msg
requestSaveImage model tags =
    case model.fileList of
        Just fileList ->
            let
                --Encode the tag list
                tagsJson =
                    List.map Json.Encode.string tags

                url = fileListFileUrl
                        [("tags", toString tagsJson)]
                        "save"
                        fileList.listId
                        fileList.fileIndex
            in
                Http.send 
                    (checkHttpAttempt (\_ -> OnSaved))
                    (Http.get url <| Json.Decode.string)
        Nothing ->
            Cmd.none



submitFileListRequest : String -> (FileList.FileListResponse -> Msg) -> Cmd Msg
submitFileListRequest url msgFunc =
    Http.send
        (checkHttpAttempt msgFunc)
        (Http.get url fileListDecoder)


requestFileListData : Int -> Int -> Cmd Msg
requestFileListData selected listId =
    let
        url =
            fileListListUrl [] "list_info" listId
    in
        submitFileListRequest url (\val -> NewFileList selected val.id val.length)



onFileDataReceived : FileData -> Model -> Model
onFileDataReceived data model =
    let
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
                            <| getSelectedTags model
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
        {model | tags = newNewTagListList, oldTagList = id}





getSelectedTags : Model -> List String
getSelectedTags model =
    Tags.selectedTags model.tags



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
