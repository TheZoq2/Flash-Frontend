port module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.Attributes exposing (style)
import Html.App
import Html
import Html.CssHelpers
import Vec exposing (..)



port files : CssFileStructure -> Cmd msg


albumContainer =
    [
        displayFlex,
        flexDirection row,
        flexWrap wrap
    ]
albumItemContainer =
    [
        height (px 300),
        margin (px 10)
    ]

toStyle = 
    Css.asPairs >> Html.Attributes.style


--Css helpers
{id, class, classList} =
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
    | TagEditorRightPaneSelected

    | ImageViewer


--Some very common parameters
primaryTextColor = hex "ffffff"
primaryBackgroundColor = hex "1c1c1c"
secondaryBackgroundColor = hex "1f1f1f"
tagListSelectedBackgroundColor = (rgb 50 50 50)
disabledTagColor = (rgb 100 100 100)
tagEditorSidebarWidth = 350
tagEditorStdMargin = 10
defaultMaxHeight = 1000

--The style to apply to all files in the project

globalStyle = 
    (stylesheet)
    (
        [
            body
            [
                color primaryTextColor,
                backgroundColor primaryBackgroundColor,
                fontFamily sansSerif
            ],
            (.) RemoveButton
            [
                display inlineBlock,
                borderRadius (em 0.2),
                backgroundColor (rgb 169 3 41),
                fontSize (em 1.6),

                cursor pointer,

                hover [
                    backgroundColor (rgb 200 50 80)
                ]
            ],
            (.) TagListContainer
            [
                width (px tagEditorSidebarWidth)
            ],
            (.) TagListName
            [
                padding2 (em 0.5) (px 0)
            ],
            (.) TagListLi
            [
                height (em 2)
            ],
            (.) DisabledTag
            [
                color disabledTagColor
            ],
            (.) TagListContainer
            [
                margin2 (px (tagEditorStdMargin*3)) zero
            ]
        ] 
        ++
        tagEditorCss
        ++
        tagListManagerCss
        ++
        imageViewerStyle
    )






--Tag editor specific styles
tagEditorCss =
    [
        (.) TagEditorContainer
        [
            displayFlex
        ],
        (.) TagEditorRightPane
        [
            maxHeight (px 1000),
            --width (px tagEditorSidebarWidth),
            flexBasis (px tagEditorSidebarWidth),
            flexGrow zero,
            flexShrink zero,
            backgroundColor secondaryBackgroundColor,
            margin2 (px 0) (px tagEditorStdMargin),

            descendants
            [
                input
                [
                    width (px (tagEditorSidebarWidth - tagEditorStdMargin * 2))
                ]
            ]
        ],
        (.) TagEditorRightPaneSelected
        [
            backgroundColor tagListSelectedBackgroundColor
        ],
        (.) TagEditorButtonRow
        [
            margin2 (px tagEditorStdMargin) zero,

            descendants
            [
                button
                [
                    marginRight (px tagEditorStdMargin)
                ]
            ]
        ],
        (.) TagEditorContentContainer
        [
        ]
    ]

tagListManagerCss = 
    [
        (.) TagListManager
        [
            children
            [
                input
                [
                ]
            ]
        ]
    ]

imageViewerStyle =
    [
        (.) ImageViewer
        [
            overflow hidden
        ]
    ]




cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ("output/css/GlobalStyle.css", Css.File.compile globalStyle) ]



totalSidebarSize =
    tagEditorSidebarWidth + tagEditorStdMargin * 2

styleFromSize size = 
    toStyle [
        width (px size.width),
        height (px size.height)
    ]



main : Program Never
main =
    Html.App.program
    {
        init = ( (), files cssFiles ),
        view = \_ -> (Html.div [] []),
        update = \_ _ -> ( (), Cmd.none ),
        subscriptions = \_ -> Sub.none
    }

