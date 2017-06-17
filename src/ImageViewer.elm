module ImageViewer
    exposing
        ( imageViewerHtml
        )

import Css
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on)
import Json.Decode exposing (..)

import Style

imageViewerHtml : a -> (Float, Float) -> (Float, Float) -> Float -> String -> Html a
imageViewerHtml onLoaded containerSize position zoom filename =
    let
        (x, y) =
            position

        (w, h) =
            containerSize

        containerCss = 
            [ Css.width <| Css.px (w * zoom)
            , Css.height <| Css.px (h * zoom)
            , Css.top <| Css.px x
            , Css.top <| Css.px y
            ]
    in
        div [Style.toStyle containerCss] 
            [ img
                [ Style.class [Style.ImageViewerImage]
                , src filename
                , onLoadSrc onLoaded
                ]
                []
            ]


onLoadSrc : msg -> Html.Attribute msg
onLoadSrc msg =
    on "load" (Json.Decode.succeed msg)

