module Audiogram.Grid exposing
  ( majorGraph
  , minorGraph
  )

import Svg as S
  exposing ( Svg
           , svg
           , line
           , text
           )
    
import Svg.Attributes
  exposing ( x
           , y
           , style
           , transform
           )

import Audiogram.Grid.X as X
import Audiogram.Grid.Y as Y

import Audiogram.Constants exposing (..)
import Audiogram.Types exposing (..)



majorGraph : Spec -> List (Svg msg)
majorGraph ((height, width, isInverted) as spec) =
  let
    (yTickX1, xTickY1) =
      graphOrigin spec
    
    yTickX2 = X.maxOffset spec
    xTickY2 = Y.maxOffset spec

  in
    List.concat
      [ [yAxisLabel]
      , [xAxisLabel spec]
      , X.xAxisSVG xTickY1 xTickY2 X.majorXAxisLabels spec
      , Y.yAxisSVG yTickX1 yTickX2 Y.yAxisLabels spec
      ]
    
-- y-axis label
yAxisLabel : Svg msg
yAxisLabel =
  S.text_ [ x "-100"
          , y "0"
          , style axisLabelStyle
          , transform "rotate(-90, 70, 50)"
          ] [ text "Level (dB)" ]

-- x-axis label
xAxisLabel : Spec -> Svg msg
xAxisLabel spec =
  S.text_ [ x "360"
          , y (String.fromInt (xAxisLabelY spec))
          , style axisLabelStyle
          ] [ text "Frequency (Hz)" ]


-- Minor (frequency) Graph
minorGraph : Spec -> List (Svg msg)
minorGraph ((height, width, isInverted) as spec) =
  []


