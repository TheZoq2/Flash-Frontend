module Style exposing (..)

import Css exposing (..)
import Css.File exposing (..)
import Css.Elements exposing (..)
import Html.Attributes exposing (style)

type CssClasses
    = AlbumContainer
    | AlbumItemContainer
    | TagListContainer


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



--port files : CssFileStructure -> Cmd msg


--cssFiles : CssFileStructure
--cssFiles =
--    toFileStructure [ ( "style.css", compile MyCss.css ) ]

