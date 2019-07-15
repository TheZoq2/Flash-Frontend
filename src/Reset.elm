module Reset exposing (resetStyle)

{-|
   Elm port of 
   http://meyerweb.com/eric/tools/css/reset/ v2.0 | 20110126
-}

import Css exposing (..)
import Css.Global exposing (..)

resetStyle =
    [ each
        [ html
        , body
        , div
        , span
        , h1
        , h2
        , h3
        , h4
        , h5
        , h6
        , p
        , blockquote
        , Css.Global.pre
        , a
        , code
        , Css.Global.em
        , img
        , q
        , strong
        , i
        , dl
        , dt
        , dd
        , ol
        , ul
        , li
        , fieldset
        , form
        , label
        , legend
        , Css.Global.table
        , caption
        , tbody
        , tfoot
        , thead
        , tr
        , th
        , td
        , article
        , aside
        , canvas
        , details
        , footer
        , header
        , menu
        , nav
        , section
        , summary
        , time
        , audio
        , video
        ]
        [ margin (px 0)
        , padding (px 0)
        , border (px 0)
        , fontSize (pct 100)
        , verticalAlign baseline
        ]
    , body
        [ lineHeight (int 1)]
    , each [ol, ul]
        [ listStyle none ]
    ]
