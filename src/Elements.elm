module Elements exposing (flatButton, thumbnail, hoverButton)

import Html exposing (..)
import Html.Attributes exposing (style, href, src)
import Html.Events exposing (onClick)
import Css
import Style
import Svg
import Svg exposing (Svg)
import Svg.Attributes

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


thumbnail : List (Html.Attribute msg) -> String -> msg -> Html msg
thumbnail additionalAttributes thumbnailUrl clickMsg =
    div ([onClick clickMsg, Style.class [Style.Thumbnail]] ++ additionalAttributes)
        [ img [src thumbnailUrl] []
        ]



hoverButton : msg -> List (Svg msg) -> (Int, Int) -> Int -> Html msg
hoverButton onClick icon (x, y) innerRadius =
    let
        hoverRadius = innerRadius * 4

        hoverArea =
            Svg.circle
                [ Svg.Attributes.cx "50"
                , Svg.Attributes.cy "50"
                , Svg.Attributes.r (toString (hoverRadius + innerRadius//2))
                , Svg.Attributes.class "hoverArea"
                ]
                []
    in
        div [Style.toStyle [Css.position Css.absolute, Css.zIndex (Css.int 1)]]
            [ Svg.svg
                [ Svg.Attributes.viewBox
                    <|  toString (-hoverRadius)
                    ++ " "
                    ++ toString (-hoverRadius)
                    ++ " "
                    ++ toString (hoverRadius * 2 + innerRadius)
                    ++ " "
                    ++ toString (hoverRadius * 2 + innerRadius)
                , Svg.Attributes.width <| toString (innerRadius + hoverRadius * 2)
                , Svg.Attributes.height <| toString (innerRadius + hoverRadius * 2)
                , Svg.Attributes.class "hoverButton"
                ]
                <| [ hoverArea
                   ]
                   ++ icon
            ]
