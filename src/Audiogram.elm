module Audiogram exposing (Model)

import Browser
import Html exposing (..)
import Json.Decode as Decode
  exposing (Decoder
           , Decoder
           , Error(..)
           , map3
           , decodeValue
           , field
           , maybe
           , decodeString
           , int
           , list
           , float
           , string
           , nullable
           , bool)
import Svg as S exposing (Svg, svg, line, text) 
import Svg.Attributes exposing (x
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
                               , width)


type alias DataPoint =
  { frequency : Float
  , masking : Maybe Bool
  , level : Int
  }
  
type alias Audiogram =
  { ear : String
  , conduction : String
  , data : List DataPoint
  }

type alias GraphSpec =
  { height: Maybe Int
  , width: Maybe Int
  , inverted_freqscale: Maybe Bool
  }

-- represents height, width, inverted
type alias Spec =
  (Int, Int, Bool)
  
type alias Model =
  (GraphSpec, List Audiogram)

type YAxisType
  = Major (List Float)
  | Minor (List Float)
    

init : () -> ( Model, Cmd msg )
init _ =
  let
    spec =
      case (decodeString graphDecoder testdata) of
        Ok gspec -> gspec
        Err _    -> GraphSpec Nothing Nothing Nothing
    data =
      case (decodeString audiogramDataDecoder testdata) of
        Ok adata -> adata
        Err _    -> []
  in                    
    ( (spec, data), Cmd.none )

      
view : Model -> Html msg
view (graphSpec, audiogramData) =
  let
    ((graphHeight, graphWidth, isInverted) as spec) = svgGraphSpec(graphSpec)
  in
    svg [ height (String.fromInt graphHeight)
        , width (String.fromInt graphWidth)
        , style ""
        ]
      (adMajorGraph spec)

adMajorGraph : Spec -> List (Svg msg)
adMajorGraph ((height, width, isInverted) as spec) =
  let
    yTickX1 = 80
    yTickX2 = maxXOffset spec
    xTickY1 = height - 80
    xTickY2 = maxYOffset spec

  in
        
  [-- y-axis label
   (S.text_ [x "-100"
            , y "0"
            , style axisLabelStyle
            , class "axis-label"
            , transform "rotate(-90, 70, 50)"
            ] [ text "Level (dB)" ])

  -- x-axis label
  , (S.text_ [x "360"
             , y (String.fromInt (xAxisLabelY height))
             , style axisLabelStyle
             , class "axis-label"
             ] [ text "Frequency (Hz)" ])
  ] ++ (yAxisSVG yTickX1 yTickX2 yaxisLabels spec)
    ++ (xAxisSVG xTickY1 xTickY2 majorXAxisLabels spec)


xAxisLabelY : Int -> Int
xAxisLabelY height =
  height - 20
  
axisLabelStyle : String
axisLabelStyle =
  "font-size: larger; font-family: sans-serif; text-anchor: middle;"  
    
update : msg -> Model -> ( Model, Cmd msg )
update msg model =
  ( model, Cmd.none )

    
main : Program () Model msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

{-- Graph Parameters --}
defaultSvgHeight : Int
defaultSvgHeight = 480

defaultSvgWidth : Int
defaultSvgWidth = 660

svgGraphSpec : GraphSpec -> Spec
svgGraphSpec spec =
  ((case spec.height of
      Just h -> h
      Nothing -> defaultSvgHeight)
  , (case spec.width of
       Just w -> w
       Nothing -> defaultSvgWidth)
  , (case spec.inverted_freqscale of
       Just i -> i
       Nothing -> False))
               
               
{-- JSON decoders --}
graphDecoder : Decoder GraphSpec
graphDecoder =
  map3 GraphSpec
    (field "height" (maybe int))
    (field "width" (maybe int))
    (field "inverted_freqscale" (maybe bool))

audiogramDataDecoder : Decoder (List Audiogram)
audiogramDataDecoder =
  (field "audiograms" (list audiogramDecoder))
               
audiogramDecoder : Decoder Audiogram
audiogramDecoder =
  map3 Audiogram
    (field "ear" string)
    (field "conduction" string)
    (field "data" (list dataPointDecoder))

dataPointDecoder : Decoder DataPoint
dataPointDecoder =
  map3 DataPoint
    (field "f" float)
    (maybe (field "m" bool))
    (field "db" int)


      
{-- X Axis (Hz) Ticks/Labels ------------------------------------------------}
xaxisMajorMin : Float
xaxisMajorMin = 0.125

xaxisMajorCnt : Int                
xaxisMajorCnt = 8

xaxisMinorMin : Float                
xaxisMinorMin = 0.75

xaxisMinorCnt : Int                
xaxisMinorCnt = 5

xaxisMajorTickStyle : String                
xaxisMajorTickStyle = "stroke: black;"

xaxisMinorTickStyle : String
xaxisMinorTickStyle = "stroke: #ccc; stroke-dasharray: 2,5"

xaxisMajorTickLabelStyle : String
xaxisMajorTickLabelStyle =
  "font-family: monospace; text-anchor: end; text-anchor: middle;"

xaxisMinorTickLabelStyle : String
xaxisMinorTickLabelStyle =
  "font-family: monospace; font-size: smaller; text-anchor: middle;"


maxXOffset : Spec -> Int
maxXOffset (_, width, _) =
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

                
majorXAxisLabels : YAxisType
majorXAxisLabels =
  Major (genXAxisLabels (xaxisMajorCnt, xaxisMajorMin) [])

minorXAxisLabels : YAxisType
minorXAxisLabels =
  Minor (genXAxisLabels (xaxisMinorCnt, xaxisMinorMin) [])
      
genXAxisLabels : (Int, Float) -> (List Float) -> (List Float)
genXAxisLabels (count, initialTick) ticks =
  case count of
    0 ->
      List.reverse ticks
        
    x ->
      (genXAxisLabels
         ((count - 1), initialTick)
         ((newXTickLabel ticks initialTick) :: ticks))
        
newXTickLabel : (List Float) -> Float -> Float
newXTickLabel ticks initialTick =
  case List.head ticks of
    Nothing -> initialTick
    Just x -> x * 2

xAxisSVG : Int -> Int -> YAxisType -> Spec -> List (Svg msg)
xAxisSVG xTickY1 xTickY2 labels spec =
  case labels of
    Major ls ->
      xAxisSVGMajorTicks xTickY1 xTickY2 ls spec
    Minor ls ->
      xAxisSVGMinorTicks xTickY1 xTickY2 ls spec


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
         xaxisMajorTickStyle xaxisMajorTickLabelStyle)

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
                    | labels = case (List.tail vals.labels) of
                                 Nothing -> []
                                 Just rest -> rest
                      , xValue = vals.xValue + vals.tickIncrement
                  }
      in
        xAxisTick newVals newAcc

    
{-- Y Axis (dB) Ticks/Labels ------------------------------------------------}
yaxisMin : Int
yaxisMin = -10

yaxisMax : Int           
yaxisMax = 160

yaxisTickStyle : String           
yaxisTickStyle = "stroke: black;"

yaxisTickLabelPX : Int                 
yaxisTickLabelPX = 60

yaxisTickLabelStyle : String                   
yaxisTickLabelStyle =
  "font-family: monospace; text-anchor: end; dominant-baseline: middle"


maxYOffset : Spec -> Int
maxYOffset (height, _, _) =
  let
    tickCount =
      List.length yaxisLabels

    offset =
      80

    interval =
      (height - 80) // tickCount

  in
    height - (((tickCount - 1) * interval) + offset)

    
yaxisLabels : List Int
yaxisLabels =
  let
    min = yaxisMin // 10
    max = yaxisMax // 10
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
               , style yaxisTickStyle
               ] []

        newText = 
          S.text_ [ x (String.fromInt yaxisTickLabelPX)
                  , y (String.fromInt vals.yValue)
                  , style yaxisTickLabelStyle
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
      
    
{-- testing artifacts -------------------------------------------------------}
testdata : String
testdata =
  """
{ \"width\": 660,
  \"height\": 480,
  \"inverted_freqscale\": true,

  \"audiograms\": [
    {
      \"ear\": \"left\",
      \"conduction\": \"air\",
      \"data\": [
        {
          \"f\": 0.25,
          \"db\": 70
        },
        {
          \"f\": 0.5,
          \"m\": true,
          \"db\": 70
        },
        {
          \"f\": 1,
          \"m\": false,
          \"db\": 90
        },
        {
          \"f\": 2,
          \"m\": false,
          \"db\": 80
        },
        {
          \"f\": 4,
          \"m\": false,
          \"db\": 80
        }
      ]
    }
  ]
}
"""
