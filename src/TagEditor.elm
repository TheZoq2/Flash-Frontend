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
    , keyReceiver : KeyReceiver
    , viewerSize: Size
    , tags: Tags.TagListList
    , tagTextfieldContent: Maybe String
    , searchText: String
    , fileList: Maybe FileList.FileList
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" None (Size 0 0 ) Tags.emptyTagListList Nothing "" Nothing
    , Cmd.batch
        [ Task.perform WindowResized Window.size
        ]
    )



-- UPDATE


type Msg
    = RequestNext
    | RequestPrev
    | RequestCurrent
    | RequestSave
    | NetworkError Http.Error
    | OnSaved String
    | WindowResized Window.Size
    | Keypress Int
    | SubmitSearch
    | SearchTextChanged String
    | NewFileList Int Int
    | FileDataReceived FileData
    | SaveComplete
    | UrlChanged Navigation.Location
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
        -- TODO: Check if this is used anymore
        RequestCurrent ->
            (model, Cmd.none)
        RequestSave ->
            ( model, requestSaveImage model <| getSelectedTags model)
        OnSaved _ ->
            selectNextFile model
        NetworkError e ->
            let
                _ =
                    Debug.log "Network error" e
            in
                ( model, Cmd.none )
        FileDataReceived response ->
            (model, Cmd.none)
        SaveComplete ->
            (model, Cmd.none)

        WindowResized size ->
            let
                viewerSize =
                    Size ((toFloat size.width) - Style.totalSidebarSize) (toFloat size.height)

            in
                ( { model | viewerSize = viewerSize }, Cmd.none )
            --(model, Cmd.none)
        --TODO: Write a general function for dealing with taglistlist messages
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
        Keypress code ->
            handleKeyboardInput model code
        SubmitSearch ->
            (model, submitSearch model.searchText)
        SearchTextChanged newSearch ->
            ({model | searchText = newSearch}, Cmd.none)
        NewFileList listId length ->
            let
                fileList = FileList.new listId length
            in
                ({model | fileList = Just fileList }, updateFileData fileList)
        UrlChanged location ->
            updateLocation model location



updateLocation : Model -> Navigation.Location -> (Model, Cmd Msg)
updateLocation model location =
    let
        type Route
            = FileList Int
            | File Int Int

        route =
            UrlParser.oneOf
                [ UrlParser.map (UrlParser.s "list" </> UrlParser.int) FileList
                , UrlParser.map (UrlParser.s "list" </> UrlParser.int </> "file" </> UrlParser.int) File
                ]

        (listId, fileId) = case route of
            FileList list ->
                (list, 0)
            File list file ->
                (list, file)

        newModel =
            {model | fileList = }

        cmd = requestFileData newModel.fileList.listId newModel.fileList.fileIndex
    in
        (updateLocation, cmd)

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
        newTags = Tags.addTagList Tags.emptyTagList model.tags
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
                ({model | fileList = Just newList}, updateFileData newList)
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
                    (checkHttpAttempt (\_ -> SaveComplete))
                    (Http.get url <| Json.Decode.string)
        Nothing ->
            Cmd.none



submitSearch : String -> Cmd Msg
submitSearch text =
    let
        url =
            "file_list/from_path?path=" ++ text
    in
        Http.send
            (checkHttpAttempt (\val -> NewFileList val.id val.length))
            (Http.get url decodeNewFileList)




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
            div [ Style.class [ Style.LoadingContainer ] ]
                [  div [ Style.class [Style.LoadingPulse ] ] [ ] 
                ]

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

        searchField =
            div []
                [ input [ onInput SearchTextChanged ] [] 
                , flatButton [Style.BlockButton] [] SubmitSearch "Search" 1
                ]

        imageViewer =
            case model.fileList of
                Just fileList ->
                    ImageViewer.imageViewerHtml
                        (model.viewerSize.width, model.viewerSize.height)
                        (0, 0)
                        1
                        (fileListUrl [] "get_file" fileList.listId fileList.fileIndex)
                Nothing ->
                    div [] []

    in
        div [ Style.class [ Style.TagEditorContainer ] ]
            <|
                [ div [ Style.class [ Style.TagEditorContentContainer], Style.styleFromSize model.viewerSize ] 
                    [imageViewer]
                    ]
                ++ [ div [ Style.class ([ Style.TagEditorRightPane ] ++ additionalRightPaneClasses) ]
                    [ buttonRow
                    , loadingBar
                    , Tags.tagListListHtml model.tags selectedTag listMessages
                    , addTagList
                    , searchField
                    ]
                ]




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
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
