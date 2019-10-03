module Audiogram.Types
  exposing ( Model
           , Audiogram
           , DataPoint
           , Spec
           , XAxisType (..)
           , XAxisParams
           , YAxisParams
           , Ear (..)
           , Conduction (..)
           , Masking (..)
           , PlotPoint
           , XLookupTuple
           )



type alias Model =
  ( Spec, List (List PlotPoint) )
    
type alias Audiogram =
  { ear : Ear
  , conduction : Conduction
  , data : List DataPoint
  }

type alias DataPoint =
  { frequency : Float
  , masking : Masking
  , level : Int
  }
  
type XAxisType
  = Major (List Float)
  | Minor (List Float)

-- -- used by the JSON parser only    
-- type alias GraphSpec =
--   { height: Maybe Int
--   , width: Maybe Int
--   , inverted_level: Maybe Bool
--   }

-- represents height, width, inverted; a GraphSpec with known values
type alias Spec =
  (Int, Int, Bool)


type Ear
  = Left
  | Right

type Conduction
  = Air
  | Bone

type Masking
  = Masked
  | Unmasked
    

-- represents a point that can be plotted, a data point + audiogram
-- details with defaults and appropriate casting.
type alias PlotPoint =
  { ear : Ear
  , conduction : Conduction
  , masking : Masking
  , level : Int
  , freq : Float
  , symbol : String
  }
    
-- values used to construct the x-axis part of the grid
--
-- There are two types of x-axis labels/ticks: major and minor.  The
-- major labels/ticks are the primary frequencies used in audiograms
-- and are present in all graphs.  The minor labels/ticks are optional.
type alias XAxisParams =
  { majorMin : Float            -- the first major frequency
  , majorCnt : Int              -- number of major frequencies represented
  , minorMin : Float            -- the first minor frequency
  , minorCnt : Int              -- number of minor frequencies represented
  , majorTickStyle : String     -- css styling for major ticks
  , minorTickStyle : String     -- css styling for minor ticks
  , majorLabelStyle : String    -- css styling for major tick labels
  , minorLabelStyle : String    -- css styling for minor tick labels
  }

-- values used to construct the y-axis part of the grid
type alias YAxisParams =
  { min : Int                   -- the first dB level
  , max : Int                   -- the last dB level
  , tickStyle : String          -- css styling for tick
  , labelStyle : String         -- css styling for label
  }


-- a tuple describing an xaxis (float) value and the integer px
-- position where that xaxis tick mark resides; a list of these is
-- generated while we're generating the tick marks.  frequencies are
-- fixed, so no math required.
type alias XLookupTuple =
  (Float, Int)
