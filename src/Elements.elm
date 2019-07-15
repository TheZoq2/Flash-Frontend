module Elements exposing (flatButton, thumbnail, hoverButton)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (style, href, src, css)
import Html.Styled.Events exposing (onClick)
import Css
import Style
import Svg.Styled as Svg
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes as SvgAttributes

--Complete elements

flatButton : List Css.Style -> List (Attribute a) -> a -> String -> Float -> Html a
flatButton classes attributes onClickMsg buttonText fontSize =
    p
        (
            [ css (classes ++ [Style.buttonStyle])
            , onClick onClickMsg
            , href "#"
            , css
                [ Css.fontSize (Css.em fontSize)
                ]
            ]
            ++ attributes
        )
        [ text buttonText ]


thumbnail : List (Attribute msg) -> String -> msg -> Html msg
thumbnail additionalAttributes thumbnailUrl clickMsg =
    div ([onClick clickMsg, css [Style.editorThumbnailStyle]] ++ additionalAttributes)
        [ img [src thumbnailUrl] []
        ]



hoverButton : msg -> List (Svg msg) -> (Int, Int) -> Int -> Html msg
hoverButton onClick icon (x, y) innerRadius =
    let
        hoverRadius = innerRadius * 4

        hoverArea =
            Svg.circle
                [ SvgAttributes.cx "50"
                , SvgAttributes.cy "50"
                , SvgAttributes.r (String.fromInt (hoverRadius + innerRadius//2))
                , SvgAttributes.class "hoverArea"
                ]
                []
    in
        div [css [Css.position Css.absolute, Css.zIndex (Css.int 1)]]
            [ Svg.svg
                [ SvgAttributes.viewBox
                    <|  String.fromInt (-hoverRadius)
                    ++ " "
                    ++ String.fromInt (-hoverRadius)
                    ++ " "
                    ++ String.fromInt (hoverRadius * 2 + innerRadius)
                    ++ " "
                    ++ String.fromInt (hoverRadius * 2 + innerRadius)
                , SvgAttributes.width <| String.fromInt (innerRadius + hoverRadius * 2)
                , SvgAttributes.height <| String.fromInt (innerRadius + hoverRadius * 2)
                , SvgAttributes.class "hoverButton"
                ]
                <| [ hoverArea
                   ]
                   ++ icon
            ]
