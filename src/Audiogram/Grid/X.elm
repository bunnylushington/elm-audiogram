module Audiogram.Grid.X
  exposing ( maxOffset
           , xAxisSVG
           , majorXAxisLabels
           , minorXAxisLabels
           , xAxisLabelOffset
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

-- center (more or less) the x-axis label    
xAxisLabelOffset : Spec -> Int
xAxisLabelOffset ((_, width, _) as spec) =
  let
    (xOrigin, _) = graphOrigin spec
  in
    (maxOffset spec - xOrigin) // 2

      
-- Represents the right edge of the x-axis in px.  This value may not
-- be the full width of the SVG because we've divided up the area in
-- integer intervals based on how many frequencies (x-axis ticks) are
-- represented in the graph.
maxOffset : Spec -> Int
maxOffset (_, width, _) =
  let
    -- XXX: this is weird.  i like that the labels type is tagged
    -- Major | Minor (we use that distinction elsewhere) but here we
    -- *know* that the type will be tagged Major.  is there a way to
    -- specify the return value of majorXAxisLabels more specifically?
    ticks = case majorXAxisLabels of
              (Major t) -> t
              (Minor t) -> t
    
    offset =
      80 -- distance from x origin to first x tick
            
    tickCount =
      List.length ticks -- number of ticks
                
    interval =
      (width - 80) // tickCount -- distance between ticks
  in
    ((tickCount - 1) * interval) + offset

                
majorXAxisLabels : XAxisType
majorXAxisLabels =
  Major (genXAxisLabels (xAxisParams.majorCnt, xAxisParams.majorMin) [])

    
minorXAxisLabels : XAxisType
minorXAxisLabels =
  Minor (genXAxisLabels (xAxisParams.minorCnt, xAxisParams.minorMin) [])
    
      
genXAxisLabels : (Int, Float) -> (List Float) -> (List Float)
genXAxisLabels (count, initialTick) ticks =
  case count of
    0 ->
      List.reverse ticks
        
    x ->
      (genXAxisLabels
         ((count - 1), initialTick)
         ((newTickLabel ticks initialTick) :: ticks))

        
newTickLabel : (List Float) -> Float -> Float
newTickLabel ticks initialTick =
  case List.head ticks of
    Nothing -> initialTick
    Just x -> x * 2

              
xAxisSVG : Int -> Int -> XAxisType -> Spec -> List (Svg msg)
xAxisSVG xTickY1 xTickY2 labels spec =
  case labels of
    Major labelList ->
      xAxisSVGMajorTicks xTickY1 xTickY2 labelList spec
    Minor labelList ->
      xAxisSVGMinorTicks xTickY1 xTickY2 labelList spec


type alias XAxisTickVals =
  { labels : (List Float)
  , xValue : Int
  , tickIncrement : Int
  , xTickY1 : Int
  , xTickY2 : Int
  , labelOffsetPX : Int
  , tickStyle : String
  , labelStyle : String }

  
xAxisSVGMajorTicks : Int -> Int -> (List Float) -> Spec -> List (Svg msg)
xAxisSVGMajorTicks xTickY1 xTickY2 labels ((height, width, _) as spec) =
  let
    initialTick =
      80

    tickIncrement =
      (width - initialTick) // List.length labels

    offset =
      (height - 50)
        
    xVals =
      (XAxisTickVals labels initialTick tickIncrement xTickY1 xTickY2 offset
         xAxisParams.majorTickStyle xAxisParams.majorLabelStyle)

  in
    xAxisTick xVals []

      
xAxisSVGMinorTicks : Int -> Int -> (List Float) -> Spec -> List (Svg msg)
xAxisSVGMinorTicks xTickY1 xTickY2 labels ((heigh, width, _) as spec) =
  []

    
xAxisTick : XAxisTickVals -> List (Svg msg) -> List (Svg msg)
xAxisTick vals acc =
  case (List.head vals.labels) of
    Nothing ->
      acc

    Just label ->
      let
        newLine =
          line [ y1 (String.fromInt vals.xTickY1)
               , y2 (String.fromInt vals.xTickY2)
               , x1 (String.fromInt vals.xValue)
               , x2 (String.fromInt vals.xValue)
               , style vals.tickStyle
               ][]

        newText =
          S.text_ [ y (String.fromInt vals.labelOffsetPX)
                  , x (String.fromInt vals.xValue)
                  , style vals.labelStyle
                  ] [ (text (String.fromFloat label)) ]

        newAcc =
          newLine :: newText :: acc

        newVals = { vals
                    | labels = Maybe.withDefault [] (List.tail vals.labels)
                    , xValue = vals.xValue + vals.tickIncrement
                  }
      in
        xAxisTick newVals newAcc
      
