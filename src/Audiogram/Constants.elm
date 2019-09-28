module Audiogram.Constants
  exposing ( svgGraphSpec
           , xAxisParams
           , yAxisParams
           , axisLabelStyle
           , xAxisLabelY
           , yAxisTickLabelX
           , graphOrigin
           )

import Audiogram.Types exposing (..)


{-- Graph Parameters ------------------------------------------------------}

-- SVG graph height (PX) if not specified
defaultSvgHeight : Int
defaultSvgHeight = 480

                   
-- SVG graph width (PX) if not specified                 
defaultSvgWidth : Int
defaultSvgWidth = 660

                  
-- level inversion flag if not specified
--
-- By default the Y axis (Level dB) decreases up the axis (from 160 at
-- the origin to -10 at its limit; inverting the level causes level to
-- increase up the axis (from -10 at the origin to 160 at the limit).
defaultInversion : Bool
defaultInversion =
  False

    
-- Convert a GraphSpec into a Spec, substituting Maybe's for realized
-- values, using the defaults above where the attribute is
-- unspecified.
svgGraphSpec : GraphSpec -> Spec
svgGraphSpec spec =
  let
    height =
      case spec.height of
        Just h -> h
        Nothing -> defaultSvgHeight

    width = 
      case spec.width of
        Just w -> w
        Nothing -> defaultSvgWidth

    inversion =
      case spec.inverted_level of
        Just i -> i
        Nothing -> defaultInversion

  in
    (height, width, inversion)


-- Since this isn't a general purpose graphing solution and making
-- everything paramaterizable would be a significant undertaking,
-- we've decided to hardcode some of the graph values.  These are
-- represented as constants here.

-- essentially: how far from the bottom (20 px up) to place the x axis
-- label.  
xAxisLabelY : Spec -> Int
xAxisLabelY (height, _, _) =
  height - 20

-- how far from the left to place the y axis tick labels
yAxisTickLabelX : Int
yAxisTickLabelX =
  60

-- the (y, y) SVG coordinates of the origin of the graph
graphOrigin : Spec -> (Int, Int)
graphOrigin (height, _, _) =
  (80, (height - 80))
  
      
-- css styling applied to the axes labels
axisLabelStyle : String
axisLabelStyle =
  "font-size: larger; font-family: sans-serif; text-anchor: middle;"  

xAxisParams : XAxisParams
xAxisParams =
  ( XAxisParams
      -- majorMin
      0.125
      -- majorCnt
      8
      -- minorMin
      0.75
      -- minorCnt
      5
      -- majorTickStyle
      "stroke: black;"
      -- minorTickStyle
      "stroke: #ccc; stroke-dasharray: 2,5"
      -- majorLabelStyle
      "font-family: monospace; text-anchor: end; text-anchor: middle;"
      -- minorLabelStyle
      "font-family: monospace; font-size: smaller; text-anchor: middle;"
  )


yAxisParams : YAxisParams
yAxisParams =
  ( YAxisParams
      -- min
      -10
      -- max
      160
      -- tickStyle
      "stroke: black;"
      -- labelStyle
      "font-family: monospace; text-anchor: end; dominant-baseline: middle"
  )
     
     
  
