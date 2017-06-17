port module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.Attributes exposing (style)
import Html
import Html.CssHelpers
import Vec exposing (..)


port files : CssFileStructure -> Cmd msg


albumContainer =
    [ displayFlex
    , flexDirection row
    , flexWrap wrap
    ]


albumItemContainer =
    [ height (px 300)
    , margin (px 10)
    ]


toStyle =
    Css.asPairs >> Html.Attributes.style



--Css helpers


{ id, class, classList } =
    Html.CssHelpers.withNamespace ""


type CssClasses
    = AlbumContainer
    | AlbumItemContainer
    | TagListContainer
    | RemoveButton
    | TagListName
    | TagListLi
    | DisabledTag
    | TagEditorButtonRow
    | TagListManager
      --Tag editor specific tags
    | TagEditorContainer
    | TagEditorRightPane
    | TagEditorContentContainer
    | TagEditorSelected
    | ImageViewer
    | ImageViewerImage
    | Button
    | WideButton
    | InlineButton
    | BlockButton
    | Tag
    | TagList
    | TagListButtonRow
    | AddTagButton
    | TagTextField
    | LoadingPulse
    | LoadingContainer
    | Thumbnail
    | ThumbnailContainer



--Some very common parameters


primaryTextColor =
    hex "ffffff"

dividerColor =
    hex "444444"


primaryBackgroundColor =
    hex "1c1c1c"


secondaryBackgroundColor =
    hex "1f1f1f"

buttonHoverColor =
    hex "333"


tagListSelectedBackgroundColor =
    hex "292929"


disabledTagColor =
    (rgb 100 100 100)


tagEditorSidebarWidth =
    350


tagEditorStdMargin =
    10

tagEditorStdHeight = 
    40


defaultMaxHeight =
    1000



--The style to apply to all files in the project


globalStyle =
    [(stylesheet)
        ([ body
            [ color primaryTextColor
            , backgroundColor primaryBackgroundColor
            , fontFamilies ["Source Sans Pro"]
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
            , width (px 300)
            , height (px 200)
            , children 
                [ img
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.property "object-fit" "contain"
                    ]
                ]
            ]
         , Css.class ThumbnailContainer
            [ displayFlex
            , flexWrap wrap
            ]
         ]
            ++ tagEditorCss
            ++ tagListManagerCss
            ++ imageViewerStyle
        )
    ]



--Tag editor specific styles

buttonWidth =
    tagEditorSidebarWidth / 3

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
            [ a
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
    ]


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


imageViewerStyle =
    [ Css.class ImageViewer
        [ overflow hidden
        ]
    , Css.class ImageViewerImage
        [ Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.property "object-fit" "contain"
        ]
    ]


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "output/css/GlobalStyle.css", Css.File.compile globalStyle ) ]


totalSidebarSize =
    tagEditorSidebarWidth + tagEditorStdMargin * 2



styleFromSize size =
    toStyle
        [ width (px size.width)
        , height (px size.height)
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files cssFiles
