module Elements exposing (flatButton, floatingLayer)

import Html exposing (..)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Css
import Style

--Complete elements
flatButton : List Style.CssClasses -> List (Html.Attribute a) -> a -> String -> Float -> Html a
flatButton classes attributes onClickMsg buttonText fontSize =
    p
        (
            [ Style.class (classes ++ [Style.Button])
            , onClick onClickMsg
            , href "#"
            , Style.toStyle 
                [ Css.fontSize (Css.em fontSize)
                ]
            ] 
            ++ attributes
        )
        [ text buttonText ]


{-
An invisible layer that does not accept input but can be used to place 'floating' things on
top of other content
-}

floatingLayer : List (Html msg) -> Html msg
floatingLayer children =
    div
        [ Style.toStyle
            [ Css.overflow Css.visible
            , Css.property "pointer-events" "none"
            , Css.property "background" "none !important"
            , Css.width <| Css.px 0
            , Css.zIndex <| Css.int 1
            ]
        ]
        children

