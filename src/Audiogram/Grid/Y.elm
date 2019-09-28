module Audiogram.Grid.Y
  exposing ( maxOffset
           , yAxisSVG
           , yAxisLabels
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
           , x1
           , y1
           , x2
           , y2
           , class
           , style
           , transform
           , stroke
           , height
           , width
           )
    
import Audiogram.Constants exposing (..)
import Audiogram.Types exposing (..)


maxOffset : Spec -> Int
maxOffset (height, _, _) =
  let
    tickCount =
      List.length yAxisLabels

    offset =
      80

    interval =
      (height - 80) // tickCount

  in
    height - (((tickCount - 1) * interval) + offset)

    
yAxisLabels : List Int
yAxisLabels =
  let
    min = yAxisParams.min // 10
    max = yAxisParams.max // 10
  in
  List.map (\x -> 10 * x) (List.range min max)


type alias YAxisTickVals =
  { labels : (List Int)
  , yValue : Int
  , tickDecrement : Int
  , yTickX1 : Int
  , yTickX2 : Int
  }
    
yAxisSVG : Int -> Int -> List Int -> Spec -> List (Svg msg)
yAxisSVG yTickX1 yTickX2 labels ((height, width, isInverted) as spec) =
  let
    orderedLabels =
      case isInverted of
        True -> List.reverse labels
        False -> labels
            
    initialTick =
      height - 80
        
    tickDecrement =
      (height - 80) // List.length labels

    yVals =
      YAxisTickVals orderedLabels initialTick tickDecrement yTickX1 yTickX2
        
  in
    yAxisTick yVals []

-- Recursively build some y-axis tick marks (line) and labels (text_).
-- The initial tick is based on the SVG height minus a number of px
-- based on emperical testing (that number is 80px which accounts for
-- space for the "Frequency" label as well as room for x-axis tick
-- labels.  The decrement (we're building from the bottom up) value is
-- 
--   (SVG height -  initial tick height) // # of y-axis ticks
--
-- The x1 and x2 values for all of the y-axis ticks are consistent.
--
-- We're using the YAxisTickVals as a documentation aid (so many
-- arguments is a mess to work with, passing along a structure is a
-- big help); there's nothing special about it though.

yAxisTick : YAxisTickVals -> List (Svg msg) -> List (Svg msg)
yAxisTick vals acc =
  case (List.head vals.labels) of
    Nothing ->
      acc
               
    Just label ->
      let
        newLine =
          line [ x1 (String.fromInt vals.yTickX1)
               , x2 (String.fromInt vals.yTickX2)
               , y1 (String.fromInt vals.yValue)
               , y2 (String.fromInt vals.yValue)
               , style yAxisParams.tickStyle
               ] []

        newText = 
          S.text_ [ x (String.fromInt yAxisTickLabelX)
                  , y (String.fromInt vals.yValue)
                  , style yAxisParams.labelStyle
                  ] [ (text (String.fromInt label)) ]
            
        newAcc =
          newLine :: newText :: acc
            
        newVals = { vals
                    | labels = case (List.tail vals.labels) of
                                 Nothing -> []
                                 Just rest -> rest
                    , yValue = vals.yValue - vals.tickDecrement
                  }
            
      in
        yAxisTick newVals newAcc
      
