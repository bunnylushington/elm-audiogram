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

      
maxOffset : Spec -> Int
maxOffset (_, width, _) =
  let
    ticks =
      labelList majorXAxisLabels
    
    offset =
      80 -- distance from x origin to first x tick
            
    tickCount =
      List.length ticks -- number of ticks
                
    interval =
      (width - 80) // tickCount -- distance between ticks
  in
    ((tickCount - 1) * interval) + offset

-- unwrap the label list from the type spec
labelList : XAxisType -> List Float
labelList typedLabels =
  case typedLabels of
    Major labels -> labels
    Minor labels -> labels
                    
                
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
    Major l ->
      xAxisSVGMajorTicks xTickY1 xTickY2 l spec
    Minor l ->
      xAxisSVGMinorTicks xTickY1 xTickY2 l spec


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

      
xAxisSVGMinorTicks : Int -> Int -> List Float -> Spec -> List (Svg msg)
xAxisSVGMinorTicks xTickY1 xTickY2 minorLabels ((height, width, _) as spec) =
  let
    majorLabels =
      labelList majorXAxisLabels
    
    initialTick =
      80

    tickIncrement =
      (width - initialTick) // List.length majorLabels

    offset =
      (height - 40)

    firstMinorTick =
      minorTickOffset majorLabels minorLabels initialTick tickIncrement

    xVals =
      (XAxisTickVals minorLabels firstMinorTick tickIncrement xTickY1 xTickY2
         offset xAxisParams.minorTickStyle xAxisParams.majorLabelStyle)

  in
    xAxisTick xVals []


minorTickOffset : List Float -> List Float -> Int -> Int -> Int
minorTickOffset majorLabels minorLabels initialTick tickIncrement =
  let
    firstMinorTick =
      Maybe.withDefault 0 (List.head minorLabels)

    accumulatedMajorTicks =
      (List.length (List.filter (\x -> x < firstMinorTick) majorLabels)) - 1
        
  in
    initialTick + (accumulatedMajorTicks * tickIncrement) + (tickIncrement // 2)
    
        

    
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
      
