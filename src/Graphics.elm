module Graphics exposing (menuIcon)

import Svg exposing (..)
import Svg.Attributes exposing (..)


arrow : List (Svg msg)
arrow =
    [ Svg.path
        [ Svg.Attributes.d "M 90 50 L 80 50 M 90 50 C 90 50 90 50 90 50 M 90 50 C 90 50 90 50 90 50"]
        [ Svg.animate
            [ Svg.Attributes.attributeName "d"
            , Svg.Attributes.values "M 90 50L 80 50M 90 50C 90 50 90 50 90 50M 90 50C 90 50 90 50 90 50;M 90 50L 50 50M 90 50C 60 50 70 50 70 50M 90 50C 60 50 70 50 70 50;M 90 50L 10 50M 90 50C 70,50 60,25 60,25M 90 50C 70,50 60,75 60,75"
            , Svg.Attributes.dur "1s"
            , Svg.Attributes.begin "click"
            , Svg.Attributes.repeatCount "1"
            , Svg.Attributes.keyTimes "0;0.5;1"
            , Svg.Attributes.keySplines "0.4 0 0.4 0.5;0.7 0.5 1 1"
            , Svg.Attributes.calcMode "spline"
            , Svg.Attributes.fill "freeze"
            ]
            []
        ]
    ]

menuIcon : List (Svg msg)
menuIcon =
    ( [ Svg.g
          [ fill "none"
          , stroke "#fff"
          , strokeWidth "10"
          ]
          [ Svg.line
              [ x1 "0"
              , x2 "100"
              , y1 "5"
              , y2 "5"
              ]
              []
          , Svg.line
              [ x1 "0"
              , x2 "100"
              , y1 "50"
              , y2 "50"
              ]
              []
          , Svg.line
              [ x1 "0"
              , x2 "100"
              , y1 "95"
              , y2 "95"
              ]
              []
          ]
      ]
    )
