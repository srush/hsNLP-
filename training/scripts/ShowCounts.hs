import TreeBank 
import TAG 
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import Sentence 
import NLP.ChartParse
import NLP.ChartParse.Eisner
import NLP.Semiring.ViterbiNBestDerivation
import Safe (fromJustDef)
import DependencyStructure
import Debug.Trace
import Text.PrettyPrint.HughesPJClass

separate :: (Eq el) => el -> [el] -> [[el]]
separate el [] = [] 
separate el ls = case elemIndex el ls of
                   Just n -> 
                       (take n ls): (separate el (drop (n+1) ls))
                   Nothing -> []


main = do 
  [countFile, testFile] <- getArgs
  counts <- decodeFile countFile
            --print counts
  let probs = estimateTAGProb counts
  contents <- readFile testFile
  let sents =  map (parseSentence testFile. unlines) $ 
               separate "" $ lines contents
                        --print sents
  let [(d, (Just b, _)) ] = map (parseSent counts probs) sents
  let TAGDerivation (_, dep)= getBestDerivation b
  let wrong = score d dep
  if length wrong == 0 then putStrLn "Perfect."
   else putStrLn $ render $ vcat $ map (text.show) wrong
          where parseSent counts probs insent = (dep ,eisnerParse getFSM symbolConv sent) 
                    where dsent = toTAGDependency (initSemiProbs probs) insent
                          (TAGSentence _ dep) = dsent
                          sent = toTAGTest counts probs insent   
                          getFSM i (Just word) =  (initAdj (snd. head . getWords sent) (leftProbs probs) word,
                                                   initAdj (snd. head . getWords sent) (rightProbs probs) word)
                          symbolConv word = Just word 
                          
  --print $ show (counts::TAGTrainingCounts) 
