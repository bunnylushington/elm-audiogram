module Audiogram exposing (main)

import Browser
import Html exposing (..)

import Svg as S
  exposing ( Svg
           , svg)
    
import Svg.Attributes
  exposing ( height
           , width
           , style )

    
import Audiogram.Types
  exposing (..)

import Audiogram.DataParser
  exposing ( decode )


import Audiogram.Grid
  exposing ( majorGraph
           , minorGraph
           )


{--------------------------------------------------------------------------}    

init : () -> ( Model, Cmd msg )
init _ =
  let
    audiogramData =
      Audiogram.DataParser.decode testdata
  in
    (audiogramData, Cmd.none)

      
view : Model -> Html msg
view (((graphHeight, graphWidth, isInverted) as spec), audiogramData) =
  svg [ height (String.fromInt graphHeight)
      , width (String.fromInt graphWidth)
      , style ""
      ]
  ( (majorGraph spec)
      ++ (minorGraph spec)
  )

        
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

      
    
{-- testing artifacts -------------------------------------------------------}
testdata : String
testdata =
  """
{ \"width\": 860,
  \"height\": 480,
  \"inverted_level\": true,

  \"audiograms\": [
    {
      \"ear\": \"left\",
      \"conduction\": \"Air\",
      \"data\": [
        {
          \"f\": 0.25,
          \"m\": true,
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
