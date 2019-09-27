module Audiogram exposing (Model
                          , audiogramDataDecoder
                          , audiogramDecoder
                          , dataPointDecoder
                          , xaxisLabels
                          , yaxisLabels
                          , testdata
                          , Audiogram
                          , YAxisType (..)
                          , DataPoint)

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

type alias Spec =
  (Int, Int, Bool)
  
type alias Model =
  (GraphSpec, List Audiogram)

type YAxisType
  = Major
  | Minor
    

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
        , style "border: 1px solid"
        ]
      (adMajorGraph spec)
      
--    [ line [ x1 "80", y1 "400", x2 "640", y2 "400", stroke "black" ] []]


adMajorGraph : Spec -> List (Svg msg)
adMajorGraph ((height, width, isInverted) as spec) =
  let
    yTickX1 = 80
    yTickX2 = width - 40

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
  ] ++ 

   (yAxisSVG yTickX1 yTickX2 yaxisLabels spec)



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


      
{-- X Axis (Hz) Labels --}
xaxisMajorMin : Float
xaxisMajorMin = 0.125

xaxisMajorCnt : Int                
xaxisMajorCnt = 8

xaxisMinorMin : Float                
xaxisMinorMin = 0.75

xaxisMinorCnt : Int                
xaxisMinorCnt = 5

xaxisLabels : YAxisType -> List Float
xaxisLabels axisType =
  case axisType of
    Major ->
      genXAxisLabels (xaxisMajorCnt, xaxisMajorMin) []

    Minor ->
      genXAxisLabels (xaxisMinorCnt, xaxisMinorMin) []
                
      
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

              
{-- Y Axis (dB) Labels --}
yaxisMin = -10
yaxisMax = 160

yaxisTickStyle = "stroke: black;"

yaxisTickLabelPX = 60
yaxisTickLabelStyle =
  "font-family: monospace; text-anchor: end; dominant-baseline: middle"
           
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
      
    
{-- testing artifacts --}
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
