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
import Style exposing (..)
import ImageViewer
import FileList exposing (FileList, fileListDecoder)
import Commands
import Reset

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onBlur)
import Elements exposing (flatButton, thumbnail, hoverButton)
import Math.Vector2 exposing (Vec2, vec2)
import Css
import Css.Global

import Urls

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
        thumbnailUrlFromId index =
            Urls.fileListGetThumbnailUrl listId index

        thumbnailCreatorFromId additionalAttributes id =
            thumbnail additionalAttributes (thumbnailUrlFromId id) (onClick id)
    in
        List.map (thumbnailCreatorFromId []) previousIds
        ++
        [thumbnailCreatorFromId [css [Style.selectedThumbnailStyle]] currentId]
        ++
        List.map (thumbnailCreatorFromId []) nextIds

lowerBar : Model -> Html Msg
lowerBar model =
    let
        buttonStyle =
            css
                [ Css.margin2 (Css.px 0) Css.auto
                ]

        imageBar =
            case model.fileList of
                Just fileList ->
                    div [css [Style.editorThumbnailContainerStyle]]
                        <| thumbnailList fileList.listId
                            RequestId
                            <| imageIdList fileList 4
                Nothing ->
                    div [] []

        container children =
            div
                [ css
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
                        -- ImageTouchStart
                        -- ImageTouchMove
                        -- ImageTouchEnd
                        NoOp

            in
                ImageViewer.imageViewerHtml
                    ImageLoaded
                    (vec2 viewerWidth viewerHeight)
                    model.imageGeometry
                    (Urls.fileListGetFileUrl fileList.listId fileList.fileIndex)
                    events

        videoViewer fileList = 
            ImageViewer.videoViewer
                ImageLoaded
                (vec2 viewerWidth viewerHeight)
                (Urls.fileListGetFileUrl fileList.listId fileList.fileIndex)
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
            flatButton [Style.blockButtonStyle, Style.buttonRowButtonStyle] [] RequestPrev "‹" 3

        nextButton =
            flatButton [Style.blockButtonStyle, Style.buttonRowButtonStyle] [] RequestNext "›" 3

        saveButton =
            flatButton [Style.blockButtonStyle, Style.buttonRowButtonStyle] [] RequestSave "✔" 1.5

        buttonRow =
            div [ css [Style.tagEditorButtonRowStyle] ] [ prevButton, nextButton, saveButton ]

        addTagList =
            flatButton [Style.wideButtonStyle, Style.blockButtonStyle] [] AddTagList "+" 2

        loadingBar =
            case model.imageLoaded of
                False ->
                    div []
                        [ div [css [Style.loadingPulseStyle] ] [ ] 
                        ]
                True ->
                    div [] []

        additionalRightPaneClasses =
            if model.keyReceiver == FocusTagListList then
                [ Style.tagEditorSelectedStyle ]
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
                FocusTagList id ->
                    Tags.List id
                FocusTag listId tagId ->
                    Tags.Single listId tagId
                _ ->
                    Tags.None


        sidebar =
            div [ css ([ Style.tagEditorRightPaneStyle ] ++ additionalRightPaneClasses) ]
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

        globalStyle =
            Css.Global.global (Reset.resetStyle ++ Style.globalStyle)
    in
        div [ css [Style.tagEditorContainerStyle] ]
            <|
                [ globalStyle
                , div
                    [ css [Style.tagEditorContentContainerStyle]
                    , Style.styleFromSize model.viewerSize 
                    ]
                    [ div [ css [Style.editorImageContainerStyle]]
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
                    FocusCommandField data -> [commandField data] 
                    _ -> []


hoverLayer : Html Msg
hoverLayer =
    div
        [css [hoverLayerStyle]]
        [ flatButton [Style.blockButtonStyle] [css [Css.float Css.left]] RequestPrev "‹" 3
        , flatButton [Style.blockButtonStyle] [css [Css.float Css.right]] RequestNext "›" 3
        ]
