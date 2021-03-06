module Audiogram.Constants
  exposing ( xAxisParams
           , yAxisParams
           , axisLabelStyle
           , xAxisLabelY
           , yAxisTickLabelX
           , graphOrigin
           , markSymbol
           , defaultSvgHeight
           , defaultSvgWidth
           , defaultInversion
               
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


-- Since this isn't a general purpose graphing solution and making
-- everything paramaterizable would be a significant undertaking,
-- we've decided to hardcode some of the graph values.  These are
-- represented as constants here.

-- essentially: how far from the bottom (20 px up) to place the x axis
-- label.  
xAxisLabelY : Spec -> Int
xAxisLabelY (height, _, _) =
  height - 15

    
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
  "font-size: larger; font-family: sans-serif; text-align: middle;"  

    
-- parameters for generating the SVG for the major and minor x axes
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
      "stroke: #ccc;"
      -- minorTickStyle
      "stroke: #ccc; stroke-dasharray: 2,5"
      -- majorLabelStyle
      "font-family: monospace; text-anchor: end; text-anchor: middle;"
      -- minorLabelStyle
      "font-family: monospace; font-size: smaller; text-anchor: middle;"
  )


-- parameters for generating the SVG for the y axis
yAxisParams : YAxisParams
yAxisParams =
  ( YAxisParams
      -- min
      -10
      -- max
      160
      -- tickStyle
      "stroke: #ccc;"
      -- labelStyle
      "font-family: monospace; text-anchor: end; dominant-baseline: middle"
  )
     
     
  
-- what symbol to draw at a data point, based on ear, conduction, and
-- masking
markSymbol : Ear -> Conduction -> Masking -> String
markSymbol ear conduction masking =
  case (ear, conduction, masking) of
    (Right, Air,  Unmasked) -> "&#9675"  -- circle
    (Right, Air,  Masked)   -> "&#9651"  -- triangle
    (Left,  Air,  Unmasked) -> "&#10005" -- X
    (Left,  Air,  Masked)   -> "&#9633"  -- square
    (Right, Bone, Unmasked) -> "&lt"     -- <
    (Right, Bone, Masked)   -> "&#8847"  -- [
    (Left,  Bone, Unmasked) -> "&gt"     -- >
    (Left,  Bone, Masked)   -> "&#8848"  -- ]
