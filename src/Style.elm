port module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Html.Attributes exposing (style)
import Html.CssHelpers
import Html

port files : CssFileStructure -> Cmd msg


albumContainer : List Css.Mixin
albumContainer =
    [ displayFlex
    , flexDirection row
    , flexWrap wrap
    ]


albumItemContainer : List Css.Mixin
albumItemContainer =
    [ height (px 300)
    , margin (px 10)
    ]


toStyle : List Css.Mixin -> Html.Attribute msg
toStyle =
    Css.asPairs >> Html.Attributes.style



--Css helpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace ""


type CssClasses
    = AlbumIndexContainer
    | TagListContainer
    | RemoveButton
    | TagListName
    | TagListLi
    | DisabledTag
    | TagEditorButtonRow
    | TagListManager
    | CommandLineContainer
      --Tag editor specific tags
    | TagEditorContainer
    | TagEditorRightPane
    | TagEditorContentContainer
    | TagEditorSelected
    | ImageViewer
    | ImageViewerImage
    | EditorImageContainer
    | Button
    | WideButton
    | InlineButton
    | BlockButton
    | RoundedSquareButton
    | Tag
    | TagList
    | TagListButtonRow
    | AddTagButton
    | TagTextField
    | LoadingPulse
    | LoadingContainer
    | Thumbnail
    | AlbumThumbnailContainer
    | EditorThumbnailContainer
    | SelectedThumbnail
    | SearchContainer
    | HoverLayer
    | KeyboardHint



--Some very common parameters


primaryTextColor : Css.Color
primaryTextColor =
    hex "ffffff"

dividerColor : Css.Color
dividerColor =
    hex "444444"


primaryBackgroundColor : Css.Color
primaryBackgroundColor =
    hex "1c1c1c"

secondaryBackgroundColor : Css.Color
secondaryBackgroundColor =
    hex "1f1f1f"

buttonHoverColor : Css.Color
buttonHoverColor =
    hex "333"


tagListSelectedBackgroundColor : Css.Color
tagListSelectedBackgroundColor =
    hex "292929"


disabledTagColor : Css.Color
disabledTagColor =
    (rgb 100 100 100)


tagEditorSidebarWidth : Float
tagEditorSidebarWidth =
    350

tagEditorThumbnailHeight : Float
tagEditorThumbnailHeight =
    100

tagEditorLowerBarHeight: Float
tagEditorLowerBarHeight =
    100


tagEditorStdMargin : Float
tagEditorStdMargin =
    10

tagEditorStdHeight : Float
tagEditorStdHeight = 
    40


defaultMaxHeight : Float
defaultMaxHeight =
    1000



--The style to apply to all files in the project


globalStyle : List Css.Stylesheet
globalStyle =
    [(stylesheet)
        ([ body
            [ color primaryTextColor
            , backgroundColor primaryBackgroundColor
            , fontFamilies ["Tahoma, Geneva, sans-serif"]
            ]
         , Css.class RemoveButton
            [ display inlineBlock
            , borderRadius (Css.em 0.2)
            , backgroundColor (rgb 169 3 41)
            , fontSize (Css.em 1.6)
            , cursor pointer
            , hover
                [ backgroundColor (rgb 200 50 80)
                ]
            ]
         , Css.class TagListContainer
            [ width (px tagEditorSidebarWidth)
            ]
         , Css.class TagListName
            [ padding2 (Css.em 0.5) (px 0)
            ]
         , Css.class TagListLi
            [ height (Css.em 2)
            ]
         , Css.class DisabledTag
            [ color disabledTagColor
            ]
         , Css.class TagListContainer
            [ margin2 (px (tagEditorStdMargin * 3)) zero
            ]
         , Css.class Thumbnail
            [ display block
            , children 
                [ img
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.property "object-fit" "contain"
                    ]
                ]
            ]
         , Css.Elements.li
            [ listStyle none
            ]
         , Css.Elements.img
            [ Css.property "image-orientation" "from-image"]
         , Css.class KeyboardHint
            [ width (Css.em 1)
            , height (Css.em 1)
            , borderBottom3 (Css.em 0.2) solid (Css.hex "7b007b")
            , backgroundColor (Css.hex "3f2936")
            , borderRadius (Css.px 2)
            , padding (Css.em 0.3)
            , fontSize (Css.em 0.9)
            , children
                [ p
                    [ width (Css.pct 100)
                    , height (Css.pct 100)
                    , lineHeight (Css.em 1.2)
                    , textAlign center
                    , fontSize (Css.em 0.9)
                    ]
                ]
            ]
         ]
            ++ tagEditorCss
            ++ tagListManagerCss
            ++ imageViewerStyle
            ++ albumStyle
            ++ commandLineStyle
        )
    ]



--Tag editor specific styles

buttonWidth : Float
buttonWidth =
    tagEditorSidebarWidth / 3


tagEditorCss : List Css.Snippet
tagEditorCss =
    [ Css.class TagEditorContainer
        [ displayFlex
        ]
    , Css.class TagEditorRightPane
        [ maxHeight (px 1000)
        , --width (px tagEditorSidebarWidth),
          flexBasis (px tagEditorSidebarWidth)
        , flexGrow zero
        , flexShrink zero
        , backgroundColor secondaryBackgroundColor
        , margin2 (px 0) (px tagEditorStdMargin)
        ]
    , Css.class TagEditorSelected
        [ backgroundColor tagListSelectedBackgroundColor
        ]
    , Css.class TagEditorButtonRow
        [ displayFlex
        , descendants
            [ Css.class Button
                [ width <| Css.px buttonWidth
                , height <| Css.px tagEditorStdHeight
                , borderBottom3 (Css.px 1) Css.solid dividerColor
                ]
            ]
        ]
    , Css.class Button
        [ textAlign Css.center
        , lineHeight <| Css.px tagEditorStdHeight
        , color primaryTextColor
        , textDecoration Css.none
        , cursor pointer
        , hover
            [ backgroundColor buttonHoverColor]
        ]
    , Css.class WideButton
        [ width <| Css.px tagEditorSidebarWidth
        ]
    , Css.class BlockButton 
        [ display Css.block
        , padding2 (Css.px tagEditorStdMargin) zero
        ]
    , Css.class InlineButton
        [ lineHeight <| (Css.em 1)
        , display Css.inlineBlock
        , width <| (Css.em 1)
        ]
    , Css.class RoundedSquareButton
        [ width <| Css.em 1
        ]
    , Css.class Tag
        [ fontSize <| (Css.em 1)
        , displayFlex
        , descendants
            [ span
                [ flexGrow (Css.num 1)
                , cursor Css.pointer
                , hover
                    [ backgroundColor buttonHoverColor ]
                ]
            ]
        ]
    , Css.class TagList
        [ padding2 (Css.em 1) zero
        , borderTopWidth (px 1)
        , borderTopColor dividerColor
        , borderTopStyle solid
        ]
    , Css.class TagListButtonRow
        [ displayFlex
        , firstChild
            [ flexGrow <| Css.num 1
            ]
        ]
    , Css.class AddTagButton
        [ flexGrow <| Css.num 1
        ]
    , Css.class TagTextField
        [ border zero
        , backgroundColor Css.transparent
        , color primaryTextColor
        ]
    , Css.class HoverLayer
        [ position absolute
        , top <| px 0
        , left <| px 0
        , width <| pct 100
        , property "pointer-events" "none"
        , children
            [ everything [property "pointer-events" "all"]
            ]
        ]
    ]


tagListManagerCss : List Css.Snippet
tagListManagerCss =
    [ Css.class TagListManager
        [ children
            [ input
                []
            ]
        ]
    , Css.class TagListContainer
        [ borderTopWidth (px 1)
        , borderTopColor dividerColor
        , borderTopStyle solid
        , paddingTop (px 15)
        , margin2 (px 15) zero
        ]
    ]


imageViewerStyle : List Css.Snippet
imageViewerStyle =
    [ Css.class ImageViewer
        [ overflow hidden
        ]
    , Css.class ImageViewerImage
        [ Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.property "object-fit" "contain"
        ]
    , Css.class EditorThumbnailContainer
        [ displayFlex
        , justifyContent center
        , height <| Css.px 100
        , children
            [ Css.class Thumbnail
                [ height <| Css.px tagEditorThumbnailHeight
                , margin <| Css.px 2
                , cursor pointer
                ]
            , Css.class SelectedThumbnail
                [ transforms [translateY <| Css.px -10, scaleX 1.1, scaleY 1.1]
                , border3 (Css.px 2) solid <| Css.hex "3b9ddb"
                ]
            ]
        ]
    , Css.class EditorImageContainer
        [ flexGrow <| num 1
        , overflow hidden
        ]
    , Css.class TagEditorContentContainer
        [ overflow hidden
        , displayFlex
        , flexDirection column
        ]
    ]


albumSearchMaxWidth : Css.Px
albumSearchMaxWidth =
    px 960

albumStyle : List Css.Snippet
albumStyle =
    [ Css.class SearchContainer
        [ displayFlex
        , margin2 (px 0) auto
        , maxWidth albumSearchMaxWidth
        , descendants
            [ input
                [ Css.flexGrow <| Css.num 1
                ]
            ]
        ]
    , Css.class AlbumIndexContainer
        [ maxWidth albumSearchMaxWidth
        , margin3 (px 100) auto (px 0)
        ]
     , Css.class AlbumThumbnailContainer
        [ displayFlex
        , flexWrap wrap
        , children
            [ Css.class Thumbnail 
                [ width (px 300)
                , height (px 200)
                ]
            ]
        ]
    ]


commandLineStyle : List Css.Snippet
commandLineStyle =
    [ Css.class CommandLineContainer
        [ position absolute
        , backgroundColor <| hex "505050"
        , padding <| px 5
        , left <| pct 50
        , transform <| translate2 (pct -50) (pct 0)
        , borderRadius <| px 5
        , descendants
            [ strong [fontWeight bold]
            , input
                [ border zero
                , backgroundColor Css.transparent
                , color primaryTextColor
                ]
            ]
        ]
    ]


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "output/css/GlobalStyle.css", Css.File.compile globalStyle ) ]


totalSidebarSize : Float
totalSidebarSize =
    tagEditorSidebarWidth + tagEditorStdMargin * 2



styleFromSize : {width: Float, height: Float} -> Html.Attribute msg
styleFromSize size =
    toStyle
        [ width (px size.width)
        , height (px size.height)
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
