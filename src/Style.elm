module Style exposing (..)

import Html.Styled
import Html.Styled.Attributes exposing (css)

import Css
import Css exposing (..)
import Css.Global
import Css.Animations as Anim

type alias Placeholder = ()


styleFromSize : {width: Float, height: Float} -> Html.Styled.Attribute msg
styleFromSize size =
    css [width (px size.width), height (px size.height)]


--Some very common parameters



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

totalSidebarSize : Float
totalSidebarSize =
    tagEditorSidebarWidth + tagEditorStdMargin * 2



-- Common colors

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


-- Global style, primarily for styling body

globalStyle =
    [ Css.Global.body
        [ backgroundColor primaryBackgroundColor
        , color <| hex "#ffffff"
        , backgroundColor <| hex "#1c1c1c"
        , fontFamilies ["Source Sans Pro"]
        ]
    ]


-- Animation stuff
loadingPulseKeyframe =
    Anim.keyframes [(50, [Anim.backgroundColor (rgb 25 142 224)])]

loadingContainerStyle =
    [ displayFlex
    , justifyContent spaceAround
    , alignItems center
    ]

loadingPulseStyle =
    Css.batch
        [ position absolute
        , width (px tagEditorSidebarWidth)
        , height (px 5)
        , backgroundColor (rgba 25 142 224 0.2)
        , animationName loadingPulseKeyframe
        , animationDuration (ms 750)
        , property "animation-iteration-count" "infinite"
        ]


-- Styles

hoverLayerStyle : Style
hoverLayerStyle =
    Css.batch
        [ position absolute
        , top <| px 0
        , left <| px 0
        , width <| pct 100
        , property "pointer-events" "none"
        , Css.Global.children
            [ Css.Global.everything [property "pointer-events" "all"]
            ]
        ]


albumSearchMaxWidth : Css.Px
albumSearchMaxWidth =
    px 960


selectedThumbnailStyle : Style
selectedThumbnailStyle =
    Css.batch
        [ transforms [translateY <| Css.px -10, scaleX 1.1, scaleY 1.1]
        , border3 (Css.px 2) solid <| Css.hex "3b9ddb"
        ]


editorThumbnailContainerStyle : Style
editorThumbnailContainerStyle =
    Css.batch
        [ displayFlex
        , justifyContent center
        , height <| Css.px 100
        ]


editorThumbnailStyle : Style
editorThumbnailStyle =
    Css.batch
        [ height <| Css.px tagEditorThumbnailHeight
        , margin <| Css.px 2
        , cursor pointer
        , Css.Global.children 
            [ Css.Global.img
                [ Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.property "object-fit" "contain"
                ]
            ]
        ]

tagTextFieldStyle =
    Css.batch
        [ border zero
        , backgroundColor Css.transparent
        , color primaryTextColor
        ]

addTagButtonStyle =
    Css.batch [flexGrow <| Css.num 1]


-- Buttons

buttonStyle =
    Css.batch
        [ textAlign Css.center
        , lineHeight <| Css.px tagEditorStdHeight
        , color primaryTextColor
        , textDecoration Css.none
        , cursor pointer
        , hover
            [ backgroundColor buttonHoverColor]
        ]
inlineButtonStyle =
    Css.batch
        [ lineHeight <| (Css.em 1)
        , display Css.inlineBlock
        , width <| (Css.em 1)
        ]
blockButtonStyle =
    Css.batch
        [ display Css.block
        , padding2 (Css.px tagEditorStdMargin) zero
        ]
wideButtonStyle =
    Css.batch
        [ width <| Css.px tagEditorSidebarWidth
        ]
roundedSquareButton =
    Css.batch
        [ width <| Css.px tagEditorSidebarWidth
        ]



tagStyle =
    Css.batch
        [ fontSize <| (Css.em 1)
        , displayFlex
        , Css.Global.descendants
            [ Css.Global.span
                [ flexGrow (Css.num 1)
                , cursor Css.pointer
                , hover
                    [ backgroundColor buttonHoverColor ]
                ]
            ]
        ]

tagListStyle =
    Css.batch
        [ padding2 (Css.em 1) zero
        , borderTopWidth (px 1)
        , borderTopColor dividerColor
        , borderTopStyle solid
        ]

tagEditorSelectedStyle =
    Css.batch
        [ backgroundColor tagListSelectedBackgroundColor
        ]

disabledTagStyle =
    Css.batch
        [ color disabledTagColor
        ]

tagEditorRightPaneStyle =
    Css.batch
        [ maxHeight (px 1000)
        , --width (px tagEditorSidebarWidth),
          flexBasis (px tagEditorSidebarWidth)
        , flexGrow zero
        , flexShrink zero
        , backgroundColor secondaryBackgroundColor
        , margin2 (px 0) (px tagEditorStdMargin)
        ]

tagListButtonRowStyle =
    Css.batch
        [ displayFlex
        , firstChild
            [ flexGrow <| Css.num 1
            ]
        ]

editorImageContainerStyle =
     Css.batch
        [ flexGrow <| num 1
        , overflow hidden
        ]

imageViewerImageStyle =
     Css.batch
        [ Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.property "object-fit" "contain"
        ]

tagEditorContentContainerStyle =
     Css.batch
        [ overflow hidden
        , displayFlex
        , flexDirection column
        ]

tagEditorContainerStyle =
     Css.batch
        [ displayFlex
        ]

commandLineContainerStyle =
     Css.batch
        [ position absolute
        , backgroundColor <| hex "505050"
        , padding <| px 5
        , left <| pct 50
        , transform <| translate2 (pct -50) (pct 0)
        , borderRadius <| px 5
        , Css.Global.descendants
            [ Css.Global.strong [fontWeight bold]
            , Css.Global.input
                [ border zero
                , backgroundColor Css.transparent
                , color primaryTextColor
                ]
            ]
        ]

tagEditorButtonRowStyle =
     Css.batch
        [ displayFlex
        ]

buttonRowButtonWidth =
    tagEditorSidebarWidth / 3
buttonRowButtonStyle =
    Css.batch
        [ width <| Css.px buttonRowButtonWidth
        , height <| Css.px tagEditorStdHeight
        , borderBottom3 (Css.px 1) Css.solid dividerColor
        ]


-- Album specific styles

albumThumbnailContainerStyle : Style
albumThumbnailContainerStyle =
    Css.batch
        [ displayFlex
        , flexWrap wrap
        ]

albumThumbnailStyle : Style
albumThumbnailStyle =
    Css.batch
        [ width (px 300)
        , height (px 200)
        ]

searchContainerStyle : Style
searchContainerStyle =
    Css.batch
        [ displayFlex
        , margin2 (px 0) auto
        , maxWidth albumSearchMaxWidth
        , Css.Global.descendants
            [ Css.Global.input
                [ Css.flexGrow <| Css.num 1
                ]
            ]
        ]

albumIndexContainerStyle =
     Css.batch
        [ maxWidth albumSearchMaxWidth
        , margin3 (px 100) auto (px 0)
        ]

