import TreeBank 
import TAG 
import System (getArgs) 
import System.IO
import Data.Monoid
import Data.Binary
import Data.List
import qualified Data.Map as M
import Sentence 
import NLP.ChartParse
import NLP.Semiring
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
  let TAGDerivation (words, dep)= getBestDerivation b
  print $ TAGDerivation (words, dep)
  let wrong = score d (fmap fst dep)
  if length wrong == 0 then putStrLn "Perfect."
   else putStrLn $ render $ vcat $ map (text.show) wrong
          where 
            prune :: (Ord sig) => M.Map sig (ViterbiDerivation TAGDerivation) -> 
                     M.Map sig (ViterbiDerivation TAGDerivation)  
            prune m = M.filter (\a -> (getBestScore a) > (best / 10000  )) m
                where best = maximum $ map (getBestScore. snd) $ M.toList m 
            parseSent counts probs insent = (dep ,eisnerParse getFSM symbolConv sent prune) 
                    where dsent = toTAGDependency (initSemiProbs probs) insent
                          (TAGSentence _ dep) = dsent
                          sent = toTAGTest counts probs insent   
                          getFSM i (Just word) =  (initAdj (snd. head . getWords sent) (leftProbs probs) word,
                                                   initAdj (snd. head . getWords sent) (rightProbs probs) word)
                          symbolConv word = Just word 
                          
  --print $ show (counts::TAGTrainingCounts) 
