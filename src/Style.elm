port module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Html.Attributes exposing (style)
import Html.App
import Html
import Html.CssHelpers



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

tagListContainer =
    [
        width (pct 100)
    ]

toStyle = 
    Css.asPairs >> Html.Attributes.style


--Css helpers
{id, class, classList} =
    Html.CssHelpers.withNamespace ""

--Some very common parameters
primaryTextColor = hex "ffffff"
primaryBackgroundColor = hex "1c1c1c"
secondaryBackgroundColor = hex "1f1f1f"
disabledTagColor = (rgb 100 100 100)




type CssClasses
    = AlbumContainer
    | AlbumItemContainer
    | TagListContainer
    | RemoveButton
    | TagListName
    | TagListLi
    | DisabledTag

    --Tag editor specific tags
    | TagEditorRightPane


--The style to apply to all files in the project

globalStyle = 
    (stylesheet)
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
            margin (px 10)
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

        --Tag editor specific styles
        (.) TagEditorRightPane
        [
            width (px 320),
            backgroundColor secondaryBackgroundColor,

            descendants
            [
                input
                [
                    width (pct 100)
                ]
            ]
        ]
    ]









cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ("output/css/GlobalStyle.css", Css.File.compile globalStyle) ]


main : Program Never
main =
    Html.App.program
    {
        init = ( (), files cssFiles ),
        view = \_ -> (Html.div [] []),
        update = \_ _ -> ( (), Cmd.none ),
        subscriptions = \_ -> Sub.none
    }

