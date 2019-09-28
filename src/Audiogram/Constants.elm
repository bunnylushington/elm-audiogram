module Audiogram.Constants exposing (svgGraphSpec)

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
