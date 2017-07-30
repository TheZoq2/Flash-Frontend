module EditorView exposing (view)

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    )
import EditorMsg exposing
    ( Msg(..)
    )

import Tags
import Style
import ImageViewer
import FileList exposing (FileList, fileListDecoder, fileListFileUrl, fileListListUrl)

import Vec exposing (..)
import Html exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char
import Elements exposing (flatButton)
import Dom
import List.Extra
import UrlParser
import Navigation
import UrlParser
import UrlParser exposing ((</>))
import Math.Vector2 exposing (Vec2, vec2)


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
            let
                events =
                    ImageViewer.MouseEvents
                        MouseMovedOnImage
                        ImageScrolled
                        NoOp

            in
                case model.fileList of
                    Just fileList ->
                        ImageViewer.imageViewerHtml
                            ImageLoaded
                            (vec2 viewerWidth model.viewerSize.height)
                            model.imageGeometry
                            (fileListFileUrl [] "get_file" fileList.listId fileList.fileIndex)
                            events
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
