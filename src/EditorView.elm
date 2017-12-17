module EditorView exposing (view, imageIdList)

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    , FileKind(..)
    )
import EditorMsg exposing
    ( Msg(..)
    )

import Tags
import Style
import ImageViewer
import FileList exposing (FileList, fileListDecoder, fileListFileUrl, fileListListUrl)
import Commands

import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onBlur)
import Elements exposing (flatButton, thumbnail, hoverButton)
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
        container <| if model.sidebarVisible then
            [imageBar]
        else
            []

mediaViewer : Model -> Html Msg
mediaViewer model =
    let
        (viewerWidth, viewerHeight) = if model.sidebarVisible then
                ( model.viewerSize.width - Style.totalSidebarSize
                , model.viewerSize.height - Style.tagEditorThumbnailHeight
                )
            else
                ( model.viewerSize.width
                , model.viewerSize.height
                )

        imageViewer fileList =
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
                ImageViewer.imageViewerHtml
                    ImageLoaded
                    (vec2 viewerWidth viewerHeight)
                    model.imageGeometry
                    (fileListFileUrl [] "get_file" fileList.listId fileList.fileIndex)
                    events

        videoViewer fileList = 
            ImageViewer.videoViewer
                ImageLoaded
                (vec2 viewerWidth viewerHeight)
                (fileListFileUrl [] "get_file" fileList.listId fileList.fileIndex)
    in
        case model.fileList of
            Just fileList ->
                case model.fileKind of
                    Image ->
                        imageViewer fileList
                    Video ->
                        videoViewer fileList
            Nothing ->
                div [] [p [] [text "No file list selected"]]

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
                , onTagSubmit = FinnishAddTag
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


        sidebar =
            div [ Style.class ([ Style.TagEditorRightPane ] ++ additionalRightPaneClasses) ]
                [ buttonRow
                , loadingBar
                , Tags.tagListListHtml model.tags selectedTag listMessages
                , addTagList
                ]

        commandFieldEvents =
            { onBlur = CommandCanceled
            , onInput = CommandInput
            }

        commandField data =
            Commands.commandLineView commandFieldEvents data
    in
        div [ Style.class [ Style.TagEditorContainer ] ]
            <|
                [ div
                    [ Style.class [ Style.TagEditorContentContainer]
                    , Style.styleFromSize model.viewerSize 
                    ]
                    [ div [ Style.class [Style.EditorImageContainer]]
                        [ mediaViewer model
                        ]
                    , lowerBar model
                    ]
                ]
                ++ [ if model.sidebarVisible then
                         sidebar
                     else
                         hoverLayer
                   ]
                ++ case model.keyReceiver of
                    CommandField data -> [commandField data] 
                    _ -> []


hoverLayer : Html Msg
hoverLayer =
    div
        [Style.class [Style.HoverLayer]]
        [ flatButton [Style.BlockButton] [Style.toStyle [Css.float Css.left]] RequestPrev "‹" 3
        , flatButton [Style.BlockButton] [Style.toStyle [Css.float Css.right]] RequestNext "›" 3
        ]
