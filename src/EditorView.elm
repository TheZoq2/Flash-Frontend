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

import Html exposing (..)
import Elements exposing (flatButton)
import Math.Vector2 exposing (Vec2, vec2)
import Css


lowerBar : Model -> Html Msg
lowerBar model =
    let
        buttonStyle =
            Style.toStyle
                [ Css.margin2 (Css.px 0) Css.auto
                ]

        showButton =
            div [Style.toStyle [Css.margin2 (Css.px 0) Css.auto]]
              [ flatButton [Style.RoundedSquareButton] [buttonStyle] RequestNext "▲" 1.5
              ]

        imageBar =
            case model.fileList of
                Just fileList ->
                    div []
                        []
                Nothing ->
                    div [] []

        container children =
            div
                [ Style.toStyle
                    [ Css.position Css.absolute
                    , Css.bottom <| Css.px 0
                    , Css.width <| Css.pct 100
                    ]
                ]
                children

    in
        container <| [showButton, imageBar]


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
                        ImageTouchStart
                        ImageTouchMove
                        ImageTouchEnd
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
                    [imageViewer, lowerBar model]
                ]
                ++ ( if model.sidebarVisible then
                         sidebar
                     else
                         []
                   )
