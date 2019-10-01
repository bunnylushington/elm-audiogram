module Audiogram.Types
  exposing ( Model
           , Audiogram
           , DataPoint
           , GraphSpec
           , Spec
           , XAxisType (..)
           , XAxisParams
           , YAxisParams
           )



type alias Model =
  (GraphSpec, List Audiogram)
    
type alias Audiogram =
  { ear : String
  , conduction : String
  , data : List DataPoint
  }

type alias DataPoint =
  { frequency : Float
  , masking : Maybe Bool
  , level : Int
  }
  
type XAxisType
  = Major (List Float)
  | Minor (List Float)

-- used by the JSON parser only    
type alias GraphSpec =
  { height: Maybe Int
  , width: Maybe Int
  , inverted_level: Maybe Bool
  }

-- represents height, width, inverted; a GraphSpec with known values
type alias Spec =
  (Int, Int, Bool)
    
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
