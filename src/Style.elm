port module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Html.Attributes exposing (style)
import Html.App
import Html

type CssClasses
    = AlbumContainer
    | AlbumItemContainer
    | TagListContainer
    | RemoveButton


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





--Some very common parameters
primaryTextColor = hex "ffffff"
primaryBackgroundColor = hex "1c1c1c"






--The style to apply to all files in the project

globalStyle = 
    (stylesheet)
    [
        body
        [
            color primaryTextColor,
            backgroundColor primaryBackgroundColor
        ],
        (.) RemoveButton
        [
            display inline-block,
            width (em 1.5),
            height (em 1.5),
            borderRadius (em 0.2),
            float right
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

