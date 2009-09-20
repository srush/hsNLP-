import Dependency
import TreeBank
import Sentence 
import Data.Array
import NLP.ChartParse.Eisner
import Debug.Trace.Helpers
import Debug.Trace
main = do 
  sent <- readSentence "data/sample2.data"
  let dsent = toDependency sent
  let (DependencySentence sent _) = dsent
  let fsms = sentenceFSMs dsent mkDepDerivation
  let getFSM i word = fsms !! (i-1)
  return $ eisnerParse getFSM sent