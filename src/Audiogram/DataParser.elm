module Audiogram.DataParser exposing
  ( decode )

import Audiogram.Types exposing (..)

import Audiogram.Constants exposing (..)
    
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
        Ok adata -> adata |> List.map asPlotPoints
        Err _    -> []
                    
    _ =
      Debug.log "data: " data
  in                    
    (spec, data)

-- Convert raw test data to plot data.
asPlotPoints : Audiogram -> List PlotPoint
asPlotPoints audiogram =
  dataPointToPlotPoint audiogram.ear audiogram.conduction audiogram.data []

dataPointToPlotPoint : Ear -> Conduction -> List DataPoint -> List PlotPoint
                     -> List PlotPoint
dataPointToPlotPoint ear conduction data acc =
  case List.head data of
    Nothing ->
      acc

    Just testParams ->
      let
        newPlotPoint =
          (PlotPoint
             ear
             conduction
             testParams.masking
             testParams.level
             testParams.frequency
             (markSymbol ear conduction testParams.masking)
          )
            
        newAcc =
           newPlotPoint :: acc
             
        newData =
          Maybe.withDefault [] (List.tail data)
      in
        dataPointToPlotPoint ear conduction newData newAcc
                        
    

      
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
    (field "ear" eardecoder)
    (field "conduction" conductiondecoder)
    (field "data" (list dataPointDecoder))

dataPointDecoder : Decoder DataPoint
dataPointDecoder =
  map3 DataPoint
    (field "f" float)
    (field "m" maskingdecoder)
    (field "db" int)


{-- custom type decoders ----------------------------------------------------}

eardecoder : Decoder Ear
eardecoder =
  Decode.string
    |> Decode.andThen (\str ->
                         case (String.toLower str) of
                           "left" ->
                             Decode.succeed Left
                           "right" ->
                             Decode.succeed Right
                           invalid ->
                             Decode.fail <| "unknown ear: " ++ invalid
                      )

       
conductiondecoder : Decoder Conduction
conductiondecoder =
  Decode.string
    |> Decode.andThen (\str ->
                         case (String.toLower str) of
                           "bone" ->
                             Decode.succeed Bone
                           "air" ->
                             Decode.succeed Air
                           invalid ->
                             Decode.fail <| "unknown conduction: " ++ invalid
                      )


-- XXX: this isn't quite right.  we would like masking ("m") to be an
-- optional JSON field; however, it's not clear how to make that
-- happen with a custom decoder like this.
maskingdecoder : Decoder Masking
maskingdecoder =
  (Decode.maybe Decode.bool)
    |> Decode.andThen (\tf ->
                         case (Maybe.withDefault False tf) of
                           False ->
                             Decode.succeed Unmasked
                           True ->
                             Decode.succeed Masked
                      )
    
       
