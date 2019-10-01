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


-- SVG list for the major x axis, y axis, and axes labels.
majorGraph : Spec -> List (Svg msg)
majorGraph ((height, width, isInverted) as spec) =
  let
    st =
      staticTicks spec
  in
    List.concat
      [ [yAxisLabel spec]
      , [xAxisLabel spec]
      , X.xAxisSVG st.xTickY1 st.xTickY2 X.majorXAxisLabels spec
      , Y.yAxisSVG st.yTickX1 st.yTickX2 Y.yAxisLabels spec
      ]


-- Minor (frequency) Graph
minorGraph : Spec -> List (Svg msg)
minorGraph ((height, width, isInverted) as spec) =
  let
    st =
      staticTicks spec
  in
    X.xAxisSVG st.xTickY1 st.xTickY2 X.minorXAxisLabels spec


-- StaticTicks!  (admittedly the name is awful) what we mean here is
-- that when drawing the xaxis ticks, the y values are static; when
-- drawing the y axis, the x values are static.  the static values are
-- the origin of the graph (graph coordinate (0,0) transposed onto the
-- SVG grid) and (X, Y) maxOffsets determined by the Grid.{X,Y}
-- modules.  Those offsets are:
--
--    (width/height of graph area // # of ticks) * (# of ticks)
type alias StaticTicks =
  { yTickX1 : Int
  , yTickX2 : Int
  , xTickY1 : Int
  , xTickY2 : Int
  }
      
staticTicks : Spec -> StaticTicks
staticTicks spec =
  let
    (yTickX1, xTickY1)
      = graphOrigin spec
  in
    (StaticTicks
       yTickX1
       (X.maxOffset spec)
       xTickY1
       (Y.maxOffset spec)
    )
      

      
-- y-axis label
yAxisLabel : Spec -> Svg msg
yAxisLabel spec =
  S.text_ [ x (String.fromInt (Y.yAxisLabelOffset spec))
          , y "0"
          , style axisLabelStyle
          , transform "rotate(-90, 70, 50)"
          ] [ text "Level (dB)" ]

-- x-axis label
xAxisLabel : Spec -> Svg msg
xAxisLabel spec =
  S.text_ [ x (String.fromInt (X.xAxisLabelOffset spec))
          , y (String.fromInt (xAxisLabelY spec))
          , style axisLabelStyle
          ] [ text "Frequency (Hz)" ]




