module Audiogram.Types
  exposing ( Model
           , Audiogram
           , DataPoint
           , GraphSpec
           , Spec
           , YAxisType (..) )


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
  
type alias GraphSpec =
  { height: Maybe Int
  , width: Maybe Int
  , inverted_level: Maybe Bool
  }

type YAxisType
  = Major (List Float)
  | Minor (List Float)

    
-- represents height, width, inverted; a GraphSpec with known values
type alias Spec =
  (Int, Int, Bool)
    
