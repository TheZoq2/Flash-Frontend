module EditorView exposing (view, imageIdList)

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
import Elements exposing (flatButton, thumbnail)
import Math.Vector2 exposing (Vec2, vec2)
import Css

imageIdList : FileList -> Int -> (List Int, Int, List Int)
imageIdList fileList range =
    let
        start = max (fileList.fileIndex - range) 0
        end = min (fileList.fileIndex + range) (fileList.length - 1)
    in
        ( List.range start (fileList.fileIndex - 1)
        , fileList.fileIndex
        , List.range (fileList.fileIndex + 1) (end)
        )

thumbnailList : Int -> (Int -> Msg) -> (List Int, Int, List Int) -> List (Html Msg)
thumbnailList listId onClick (previousIds, currentId, nextIds) =
    let
        thumbnailUrlFromId id =
            fileListFileUrl [] "get_thumbnail"  listId id

        thumbnailCreatorFromId additionalAttributes id =
            thumbnail additionalAttributes (thumbnailUrlFromId id) (onClick id)
    in
        List.map (thumbnailCreatorFromId []) previousIds
        ++
        [thumbnailCreatorFromId [Style.class [Style.SelectedThumbnail]] currentId]
        ++
        List.map (thumbnailCreatorFromId []) nextIds

lowerBar : Model -> Html Msg
lowerBar model =
    let
        buttonStyle =
            Style.toStyle
                [ Css.margin2 (Css.px 0) Css.auto
                ]

        imageBar =
            case model.fileList of
                Just fileList ->
                    div [Style.class [Style.EditorThumbnailContainer]]
                        <| thumbnailList fileList.listId
                            RequestId
                            <| imageIdList fileList 4
                Nothing ->
                    div [] []

        container children =
            div
                [ Style.toStyle
                    [ Css.bottom <| Css.px 0
                    , Css.width <| Css.pct 100
                    ]
                ]
                children

    in
        container <| [imageBar]


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
                    [ div [Style.class [Style.EditorImageContainer]] [imageViewer], lowerBar model]
                ]
                ++ ( if model.sidebarVisible then
                         sidebar
                     else
                         []
                   )
