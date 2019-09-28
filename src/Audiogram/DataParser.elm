module Audiogram.DataParser exposing
  ( decode )

import Audiogram.Types exposing (..)
    
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

-- Parse the incoming JSON string and return a Model.  Models comprise
-- two parts, an optional GraphSpec section that defines height,
-- width, and inversion (all of which have defaults if the section is
-- missing) and the audiogram data.  Audiogram data is a list of
-- Audiograms or the empty list if the section is missing or invalid.
decode : String -> Model
decode jsonString =
  let
    spec =
      case (decodeString graphDecoder jsonString) of
        Ok gspec -> gspec
        Err _    -> GraphSpec Nothing Nothing Nothing
    data =
      case (decodeString audiogramDataDecoder jsonString) of
        Ok adata -> adata
        Err _    -> []
  in                    
    (spec, data)

{-- JSON decoders -----------------------------------------------------------}
graphDecoder : Decoder GraphSpec
graphDecoder =
  map3 GraphSpec
    (field "height" (maybe int))
    (field "width" (maybe int))
    (field "inverted_level" (maybe bool))

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
      
