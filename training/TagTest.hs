import TAG
import TreeBank
import Sentence 
import Data.Array
import NLP.ChartParse.Eisner
import Debug.Trace.Helpers
import Debug.Trace

import Text.PrettyPrint.HughesPJClass

main = do 
  sent <- readSentence "data/sample6.data"
  let dsent = toTAGDependency initSemiCounts sent 
  let (TAGSentence sent _) = dsent
  --print sent
  let fsms = tagSentenceFSMs dsent
  print fsms
  let getFSM i _ = fsms !! (i-1)
  let symbolConv word = Just word 
  let (_,chart) =   eisnerParse getFSM symbolConv sent 
  putStrLn $ render $ pPrint chart 