module Elements exposing (flatButton)

import Html exposing (..)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Css
import Style

--Complete elements
flatButton : List Style.CssClasses -> List (Html.Attribute a) -> a -> String -> Float -> Html a
flatButton classes attributes onClickMsg buttonText fontSize =
    a 
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
