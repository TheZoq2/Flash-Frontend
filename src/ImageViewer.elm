module ImageViewer
    exposing
        ( imageViewerHtml
        )

import Css
import Html exposing (..)

import Style

type Msg
    = None

imageViewerHtml : (Float, Float) -> (Float, Float) -> Float -> String -> Html Msg
imageViewerHtml containerSize position zoom filename =
    let
        (x, y) =
            position

        (w, h) =
            containerSize

        containerCss = 
            [ Css.width <| Css.px w
            , Css.height <| Css.px h
            , Css.top <| Css.px x
            , Css.top <| Css.px y
            ]
    in
        div [Style.toStyle containerCss] 
            []
