module Elements exposing (flatButton, thumbnail)

import Html exposing (..)
import Html.Attributes exposing (style, href, src)
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


thumbnail : List (Html.Attribute msg) -> String -> msg -> Html msg
thumbnail additionalAttributes thumbnailUrl clickMsg =
    div ([onClick clickMsg, Style.class [Style.Thumbnail]] ++ additionalAttributes)
        [ img [src thumbnailUrl] []
        ]
