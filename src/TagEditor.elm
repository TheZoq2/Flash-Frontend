module TagEditor exposing (..)

import Tags
import Style
import ImageViewer
import FileList exposing (decodeNewFileList, fileListUrl)
import Vec exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char
import Css
import Elements exposing (flatButton)
import Dom
import List.Extra
import UrlParser
import Navigation
import UrlParser
import UrlParser exposing ((</>))


-- MODEL

type KeyReceiver
    = None
    | TagListList
    | TagList Int
    | TagField Int
    | Tag Int Int




-- Data about a file that has been received from the server. The server sends more
-- data than this but we don't process that for now

type alias FileData =
    { filePath: String
    , tags: List String
    }


type alias Model =
    { lastError : String
    -- The current part of the page that receives keypresses
    , keyReceiver : KeyReceiver
    -- The size of the image viewer
    , viewerSize: Size
    -- The currently selected tags
    , tags: Tags.TagListList
    -- The contents of the currently selected tag text input field
    , tagTextfieldContent: Maybe String
    -- The current list of files
    , fileList: Maybe FileList.FileList
    -- The id of the tag list containing the tags already on the image
    , oldTagList: Maybe Int
    -- False if the current image is not done loading
    , imageLoaded: Bool
    -- Wether or not the sidebar is shown
    , sidebarVisible: Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" None (Size 0 0 ) Tags.emptyTagListList Nothing Nothing Nothing True True
    , Cmd.batch
        [ Task.perform WindowResized Window.size
        , updateLocation location
        ]
    )



-- UPDATE

type Msg
    = RequestNext
    | RequestPrev
    | RequestSave
    | NetworkError Http.Error
    | OnSaved
    | WindowResized Window.Size
    | Keypress Int
    | NewFileList Int Int Int -- selectedFile listId length
    | FileDataReceived FileData
    | UrlChanged Navigation.Location
    | ImageLoaded
    -- Tag list specific messages
    | AddTagList
    | AddTag Int
    | StartTagAddition Int
    | ToggleTagList Int
    | RemoveTagList Int
    | ToggleTag Int Int
    | RemoveTag Int Int
    | TagTextFieldChanged String
    | CancelTagCreation
    | FocusResult (Result Dom.Error ())

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
            (onFileDataReceived data model, Cmd.none)
        UrlChanged location ->
            (model, updateLocation location)
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
        url = fileListUrl [] "get_data" listId index
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

                url = fileListUrl
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
        (Http.get url decodeNewFileList)


requestFileListData : Int -> Int -> Cmd Msg
requestFileListData selected listId =
    let
        url =
            "file_list?list_id=" ++ (toString listId)
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
            List.filter (\tag -> (List.member tag <| getSelectedTags model) == False) <| data.tags


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



-- VIEW

view : Model -> Html Msg
view model =
    let
        prevButton =
            flatButton [Style.BlockButton] [] RequestPrev "‹" 3

        nextButton =
            flatButton [Style.BlockButton] [] RequestNext "›" 3

        saveButton =
            flatButton [Style.BlockButton] [] RequestSave "✔" 1.5

        buttonRow =
            div [ Style.class [ Style.TagEditorButtonRow ] ] [ prevButton, nextButton, saveButton ]

        addTagList =
            flatButton [Style.WideButton, Style.BlockButton] [] AddTagList "+" 2

        loadingBar =
            case model.imageLoaded of
                False ->
                    div [ Style.class [ Style.LoadingContainer ] ]
                        [  div [ Style.class [Style.LoadingPulse ] ] [ ] 
                        ]
                True ->
                    div [] []

        additionalRightPaneClasses =
            if model.keyReceiver == TagListList then
                [ Style.TagEditorSelected ]
            else
                []

        listMessages =
                { onAddTag = StartTagAddition
                , onRemoveList = RemoveTagList
                , onToggleList = ToggleTagList
                , onTagRemoveButton = RemoveTag
                , onTagTextClick = ToggleTag
                , onTagnameUnfocus = CancelTagCreation
                , onTagSubmit = AddTag
                , onTextChanged = TagTextFieldChanged
                }

        selectedTag =
            case model.keyReceiver of
                TagList id ->
                    Tags.List id
                Tag listId tagId ->
                    Tags.Single listId tagId
                _ ->
                    Tags.None

        viewerWidth = model.viewerSize.width - if model.sidebarVisible then
                Style.totalSidebarSize
            else
                0

        imageViewer =
            case model.fileList of
                Just fileList ->
                    ImageViewer.imageViewerHtml
                        ImageLoaded
                        (viewerWidth, model.viewerSize.height)
                        (0, 0)
                        1
                        (fileListUrl [] "get_file" fileList.listId fileList.fileIndex)
                Nothing ->
                    div [] []

        sidebar =
            [ div [ Style.class ([ Style.TagEditorRightPane ] ++ additionalRightPaneClasses) ]
                [ buttonRow
                , loadingBar
                , Tags.tagListListHtml model.tags selectedTag listMessages
                , addTagList
                ]
            ]
    in
        div [ Style.class [ Style.TagEditorContainer ] ]
            <|
                [ div [ Style.class [ Style.TagEditorContentContainer], Style.styleFromSize model.viewerSize ] 
                    [imageViewer]
                ]
                ++ ( if model.sidebarVisible then
                         sidebar
                     else
                         []
                   )




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
